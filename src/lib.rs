use std::mem::{*};
use std::cell::{RefCell};
use std::any::Any;
use std::fmt::{Display, Error, Formatter};

extern crate smallvec;
use smallvec::*;
extern crate derive_more;

//The implementation of this object has some similarities to a brute-force solver for the Boolean Satisfiability Problem.
//https://en.wikipedia.org/wiki/Boolean_satisfiability_problem
//
//Eventually, if (when) this becomes a bottleneck, I'd like to investigate better implementations, and possibly better
//interfaces to get more performance.
//
//Currently we're using immutable HashSets and HashMaps for the QueryMasks on the theory that cloning them is cheap.  However
//  we also end up performing a lot of union operations in situations where it is highly possible both sets will contain a
//  large number of elements.  I am not sure about the performance of union operations in this case although algorithms
//  definitely exist to optimize this for our use case where we are likely to have many operations that involve very nearly
//  the same data.  Something along the lines of chunking the sets up with a checksum around each chunk and treating the
//  entire chunk as one entry.
//
//An alternative is to replace the datastore with something like SQLite for the main datastore, in order to support more
//  elaborate queries.  Another path to consider is a database that supports data-flow programs where the Mask that dictates
//  the status of each element at a given epoch can actually be a dataflow program to update a materialized view.
//When we get to this stage, consider looking at Noria Database, as well as the TimelyDataflow Rust crate.
//  

//BORIS, it would be damn useful to be able to ask a partially collapsed state for a list of additional transactions that are
//  keeping it from becoming a fully collapsed state


//BORIS
//After that would like to add an API to ask a collapsed view to iterate over all of the transactions holding some part of it in superposition
//And finally, of course, adding the query mechanism
//



// BORIS 3! things left to do, and we're done.
// 1. Add API to get transactions holding a world state in superposition.
//      This should likely take the form of a iterator to iterate over all uncollapsed transactions relevant to a given view
//      We might also want to add parameters to the element_iterator creation that allow iteration over all absent elements, all present elements, and all superposition elements
//      Remember to invalidate the collapse plan if either the view collapses further or the query changes
// 2. Add test for that API
// 3. √√√Add query functionality that works
//      √√Add an iterator object to iterate over query resutls.  The iterator object borrows the partially collapsed world state
// 4. Internally store a view's "collapsed_transactions" and "conflicting_transactions" as a HashSet, rather than a vec, which means I'll need
//      to create a TransactionIterator rather than returning a slice
// 5. Make some errors "silent", i.e. don't bother allocating an error string, if we're expecting the possibility of an error and intend to handle it
// 6. Format Comments with RustDoc
//

















//A wrapper around any type that makes it implement the QWSElement trait.
//In the future, I may allow extra keys to ride along, to provide metadata to help identify and locate the element
pub struct QWSElementWrapper<T> {
    element_type : QWSElementType,
    payload : T
}

//An element in the QuantumWorldState.
//In the future, I may extend this so it allows for more queryability, e.g. can provide more keys to locate it and the values for
//  those keys, so elements can be indexed and queried by more than just type
pub trait QWSElement {

    //Returns the QWSElementType specifying what kind of element we're dealing with
    fn element_type(&self) -> QWSElementType;

    //Returns the element as a &dyn Any, from which you can use downcast_ref() to get to the original object
    //Here is an example:
    //let cast_element = found_element.as_any().downcast_ref::<QWSElementWrapper<i32>>().unwrap();
    fn as_any(&self) -> &dyn Any;
}

impl<'dyn_trait> dyn QWSElement + 'dyn_trait {
    //If an element happens to be implemented as a QWSElementWrapper, then this is a shortcut to get its payload
    //For example, if you know the payload is an i32, you use it like this:
    //  found_element.get_payload::<i32>().unwrap()
    //Returns None if the type specified doesn't match the payload, or if the element isn't a QWSElementWrapper
    pub fn get_payload<'a, T: 'static>(&self) -> Option<&T> 
    {
        self.as_any().downcast_ref::<QWSElementWrapper<T>>().map(|element_wrapper| element_wrapper.payload())
    }
}

//An enum indicating whether an element is known to exist, known not to exist, or is in an undefined (superposition) state with
//  respect to a partially collapsed state
//These statuses are part of the public-facing interface.  The internal representation is different
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum QWSElementStatus {
    KnownPresent,   //The element definitely exists at the epoch
    KnownAbsent,    //The element definitely doesn't exist at the epoch
    Superposition,  //The element may or may not exist at the epoch
    Unknown         //The element has not yet been added to the quantum world by any transaction
}

//Destroyed elements are KnownAbsent in the epoch of the transaction
//  They must either be KnownPresent or Superposition in prior epochs from which the transaction can evolve
//Entangled elements are KnownPresent in the epoch of a transaction
//  They must either be KnownPresent or Superposition in prior epochs from which the transaction can evolve
//Created elements are KnownPresent in the epoch of the transaction
//

//A type of element in the QuantumWorldState.
//Eventiually we'll make this something more flexible than an Enum, but an Enum is fine to get up and running
//The internals of the QWS data structure don't care about what an element is at all.
#[derive(Copy, Debug, Clone, Hash, Eq, PartialEq)]
pub enum QWSElementType {
    Unspecified,
    GenericText,
    DocumentHeight,
    DocumentWidth,
}

//An index that uniquely describes an element in a QuantumWorldState
#[derive(Copy, Debug, Default, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, derive_more::Display, derive_more::From, derive_more::Add, derive_more::AddAssign)]
pub struct QWSElementID(usize);

//An index that uniquely describes a transaction in a QuantumWorldState
#[derive(Copy, Debug, Default, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, derive_more::Display, derive_more::From, derive_more::Add, derive_more::AddAssign)]
pub struct QWSTransactionID(usize);

//An error type that can be used to pass back error codes and error messages
#[derive(Debug, Clone, PartialEq)]
pub enum QWSError {
    MiscErr(String),
}

//A QuantumWorldState
pub struct QuantumWorldState {

    elements : QWSElementStore,         //The object that owns all elements in the QWS
    transactions : Vec<QWSTransaction>,   //The table that records all of the QWS Transactions
    uncollapsed_mask : QWSQueryMask //The mask that reflects the totally uncollapsed world.
        //NOTE: This just records every element in superposition, but it's here so we don't need to recreate
        //it every time we need it.  Right now, creating it from scratch is dirt cheap, but maybe that will
        //change in the future.
}

//A view into the QuantumWorldState that may represent a partially collapsed for fully collapsed world, and
//  may also represent the results of a query.
//
//The choice to use the same object for both collapsing and querying was made because it is a valid use case to
//  perform some collapsing, then perform some querying, then perform some additional collapsing in response to
//  the results found by the query.  Therefore, the QWSDataView is the single object responsible for holding
//  both query results and partially collapsed state.
pub struct QWSDataView<'a> {
    quantum_world : &'a QuantumWorldState,
    collapsed_transactions : Vec<QWSTransactionID>,
    data_mask : QWSQueryMask,
    query : Option<QWSQueryInternals>,
    full_collapse_plan : Option<QWSFullCollapsePlan>,
}

//private: A structure that contains members needed to fully collapse the view, but also to iterate over the
//  conflicting transactions that would prevent it from fully collapsing.  Building the conflicting_transactions
//  list is a side effect of attempting to perform a full collapse, and vice-versa.  So we don't want to throw
//  away this work, if we build it during one call
struct QWSFullCollapsePlan {
    // already_fully_collapsed : bool, BORIS DEAD
    // fully_collapsed_mask : QWSQueryMask,
    freely_collapsible_transactions : Vec<QWSTransactionID>,
    conflicting_transactions : Vec<QWSTransactionID>,
}

pub struct QWSElementsIterator<'a, 'b> {
    data_view : &'b QWSDataView<'a>,
    elements_iter : Box<dyn Iterator<Item=QWSElementID>+'a>
}

//A struct that is used to hold onto a transaction in the QuantumWorldState
pub struct QWSTransaction {
    id : QWSTransactionID,
    created_elements : Vec<QWSElementID>,
    destroyed_elements : Vec<QWSElementID>,
    entangled_elements : Vec<QWSElementID>,
    data_mask : QWSQueryMask,
}

//private: A struct that stores all elements, and is used to handle queries
struct QWSElementStore {
    table : Vec<QWSElementRecord>,   //The table that holds ownership of all of the elements in the QWS
    types_index : std::collections::HashMap<QWSElementType, Vec<QWSElementID>>, //A table that maps each elementType to all of the
        //individual elements that have that type
}

//private: A struct that is used internally to hold onto the elements in the QuantumWorldState
struct QWSElementRecord {
    element : Box<dyn QWSElement>, //The data represented by this element
    created_by : QWSTransactionID, //The transaction that created this QWSElement
//BEGIN BORIS
    //    deleted_by : SmallVec::<[QWSTransactionID; 1]>, //A vector recording every transaction that has a direct conflict with the
    // element.  i.e. if the given element exists, then it would be problematic to collapse any of the referenced transactions.
    // This list does not track indirect conflicts, i.e. conflicts because of sucessive entanglements.
    // Every time a new transaction is added, every element that is deleted by the transaction needs to have its record updated.

//BORIS
//Table is updated when a new transaction is sucessfully added.
//If a transaction deletes an element, it records that new transaction ID into that element's

//Fully collapsing is easy... THere might be a shortcut, but in essence, we just iterate over every query result, and collapse its
//creator.  If we make it to the end of the results without blowing up, we are fully collapsed.

//Iterating over the conflicts is where I'm stuck...  We start with an element from the query result, look up its deleted_by transactions,
// and somehow we need to figure out if each deleted_by transaction is something we need to care about.....
//
//I think we're probably going to need to get the query's result elements in a random-access structure (e.g. a HashMap)
//One way to answer that question about whether a transaction is relevant is to see if... 

//BBKing Yeltsin Boris

//FUCK I think I'm overthinking this...  Maybe the right solution is to just make an internally mutable fully collapsed map that is
//associated with every view and is initially invalid.  Then a call to either fully collapse or to begin conflict iteration generates
//map.
//
//To generate the map, we just go over the query results in order, and collapse each element's creator transaction into a new fully
//collapsed mask.  If we make it to the end we have a fully collapsed mask that can replace our view's mask.  If we hit an error than
//it means we've found a conflict between the transaction that triggered the error and (BORIS????), and both transactions need to be
//put into a conflicting transactions list for subsequent iteration.

//GOT IT!!!!  We just need a simple hashset inside the mask, to track transactions that the mask conflicts with.  It's a corroloary to
//for the AbsentPast set.  every time an element enters the absent past set, the transaction that put it there enters the conflicting
//transaction set.

//Collapsing a view or setting the query invalidates the map.

//END BORIS
}

//private: A struct to represent whatever fields are associated with a query as well as the internal data to allow iteration
//  through the query results.
//Since the only type of query we support is a simple "element_type matches a single value", this structure is more of a placeholder
struct QWSQueryInternals {
    query_by_type : QWSElementType
}

//private: The internal status code associated with each element in each QWSQueryMask
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum QWSInternalElementStatus {
    AbsentFuture,   //The element doesn't exist in this epoch because of the action of some future transaction
        // However, AbsentFuture elements may become Superposition or Present along the same causal timeline
        // This status maps into KnownAbsent, when presented publicly
    Present,        //The element exists at the epoch due to the actions of the current transaction, or a
        // transaction that was collapsed into this epoch
        // This status maps into KnownPresent, when presented publicly
    Superposition,  //The element may or may not exist at the epoch
        // This status maps into Superposition.  D'uh
    AbsentPast      //The element doesn't exist at this epoch because of the action of this or a prior transaction
        // This status maps into KnownAbsent, when presented publicly
}

//private: An internal struct that acts as a mask for query results.  The idea is that the collapse process assembles a
//  QWSQueryMask, and that mask is overlayed / joined with any query.  In effect, the whole QuantumWorldState is a database,
//  and a QWSQueryMask is a view of that database that takes into account an epochal perspective.
//Right now, it's implemented with some immutable HashSets and a HashMap, although, in the future we might make it an actual
//  DB view, like the "materialized views" used by Noria DB.
//
//There are 4 places in the code where a QueryMask is modified. 
// 1.) merge_for_base
//      When creating a new transaction, we propagate entangled transactions' status masks forward as the base for the new transaction
//      In this operation, we are mainly concerned with two things.  a.) can we confirm all depended-on (deleted & entanged)
//      elements are capable of coexisting, and b.) what impact do they have on the new transactions' status mask.
//          AbsentPast : If a dependent transaction has an AbsentPast element, it means that element was deleted along the path
//              to creating the transaction that created that dependent element.  Therefore it means it was deleted along the path
//              to creating our new transaction, and therefore, our transaction must inherit all AbsentPast statuses.
//          Present : If one dependent transaction has a present element, but another dependent transaction has a status of
//              AbsentPast or AbsentFuture on that element, it means the transactions cannot be reconciled and therefore can't
//              exist in the same epoch, so that's an error.
//              Being present in a dependent transaction doesn't tell us anything, however, about the element's state in our new
//              transaction, because a subsequent transaction (including a transaction that isn't settled yet) could come in and
//              delete the element.  Therefore Present elements in dependent transactions become Superposition elements in the
//              transaction we are creating.  (Deleted from QueryMask, because missing = Superposition)
//          Superposition : This is the easy-going state.  Not gonig to conflict with any other state, and propagates forward by
//              default, because it's represented by the element not being in any QueryMask
//          AbsentFuture : AbsentFuture is linked to the Present state.  It says essentially "Since this element A is present,
//              and future transaction Tx deletes A and created B, B can't coexist with A."  However, when we propagate the mask
//              forward into the new transaction, AbsentFuture elements also become Superposition (deleted from QueryMask)
//              The exception to this is when an AbsentFuture conflicts with another dependency, in which case they become
//              AbsentPast.  Details of this are explained in the merge_for_base function comments
// 2.) sync_transactions_with_mask
//      When bringing a transaction's status mask up to date, we propagate the future transactions' masks backwards
//      This is conceptually the reverse of Forward Propagation for creating new transactions based on existing transactions.
//      In effect, it shouldn't matter whether a mask was created earlier and future transactions were propagated backwards into
//      it, or whether it was created later.  With the same parameters, the transaction should arrive at the same mask.
// 3.) merge_for_collapse
//      When merging transactions to create a collapsed world
//      This merge carries forward many more states than Forward Propagation (1.)
//          AbsentPast : This is carried forward into the collapsed world
//          Present : This is also carried forward.  If an existing element is AbsentPast or AbsentFuture in one transaction and
//              Present in another, it is an error, and the collapsed world can't be created from those transactions.
//          Superposition : This carries forward by default, but will always give-way to another state.
//          AbsentFuture : This is also carried forward, and can cause a conflict with Present in the same way as AbsentPast
// 4.) add_present_elements, add_dependent_elements & add_settled_absent_elements, when called from add_transaction
//      When setting the known status of entangled elements on a newly creaated transaction
//      - Entangled Elements are all flagged as Present.  If they were previously in a state of AbsentPast, it is a conflict,
//          and the transaction cannot be created.
//      - All the known-absent elements are brought forward from all entangled transactions as per Forward Propagation (1.)
//      - Deleted elements must also not start out in the AbsentPast state from a prior transaction, because they must be able to
//          be present in order to be deleted (i.e. they must be in Superposition prior to the transaction)  This transaction then
//          flags them as AbsentPast in its QueryMask
//      - Created elements all get a state of Present in the transaction.  As the transaction creates the element for the first time
//          there is no possibility of conflict.
//
//NOTE: because we want internal mutability, the "contents" field actually stores the members
#[derive(Debug, Clone)]
struct QWSQueryMask {
    contents : RefCell<QWSQueryMaskContents>
}

#[derive(Debug, Clone)]
struct QWSQueryMaskContents {

    //All elements should be in at most one of the 3 sets below.  If an element is in multiple sets, it is an implementation bug.
    //If an element is in none of the sets its status is Superposition.
    //So...
    //Present = present_elements
    //AbsentFuture = future_absent_elements
    //AbsentPast = settled_absent_elements
    //Superposition = none of the sets

    present_elements : im::HashSet<QWSElementID>, //Keep created_elements & entangled_elements as a HashSet also, for faster checking

    settled_absent_elements : im::HashSet<QWSElementID>, //Elements in this set are permanently incompatible with this epoch.  If an element is in
        // this set, it means that the element was deleted in the process of getting to this point, or it means the element depended on another
        // element that was unavailable.  This is a synonym for "AbsentPast"

    future_absent_elements : im::HashMap<QWSElementID, SmallVec::<[QWSTransactionID; 1]>>, //Elements in this set are incompatible
        // with other elements that are currently present in this epoch, but may exist in an epoch in the causal future of this epoch.
        // We track which transaction(s) are responsible for each item in this set because elements in this state will either transition to
        // being in superposition or AbsentPast, depending on whether a future transaction is compatible with the transaction that caused the
        // element to enter this set.

    dependent_elements : im::HashSet<QWSElementID>, //This set tracks the elements that must not be absent in the dependent masks for this
        // data_mask to exist.  This is NOT part of the 3 status sets above, and WILL contain elements that overlap other sets.
        //In summary...
        //Present Set = Created + Entangled
        //Dependent Set = Created + Entangled + Deleted

    conflicting_transactions : im::HashSet<QWSTransactionID>, //This set tracks the transactions that could never be collapsed along side
        // this mask.  You can think of this as the set of transactions that contributed elements to the "settled_absent_elements" set.

    future_conflicting_transactions : im::HashSet<QWSTransactionID>, //This set also tracks transactions that could not be collapsed
        // along side this mask, but unlike conflicting_transactions, the conflict isn't always permanantly passed onto other descending
        // transactions.  This is associated with "future_absent_elements", and because future_absent_elements already tracks the
        // responsible transactions, this future_conflicting_transactions map can be entirely built from the future_absent_elements map.
        // In fact, you can think of this as a reverse lookup table to quickly determine if a transaction is in future_absent_elements.
        
    //BEGIN BORIS.  NOT SURE WHICH, if either or both, of these we should include
//    conflicting_superposition_elements : im::HashMap<QWSElementID, SmallVec::<[QWSTransactionID; 1]>>, //This set tracks the
        // elements that were created by transactions that, if collapsed would conflict with other transactions, and therefore,
        // if an attempt were made to fully_collapse() the mask, those elements would cause a conflict.  The Transactions
        // that are referred to are the transactions that conflict with a given element, either by deleting it or BORIS

//BORIS
//    non_conflicting_transactions : im::HashSet<QWSTransactionID>, //This set tracks all transactions that could be freely collapsed
        // into this mask without causing a conflict.  Transactions in this list are responsible for creating elements that are in
        // superposition because they aren't explicitly entangled or created at the specified epoch, but are unconflicted
        // nonetheless.  This set should include every transaction that is not referenced by an entry in the
        // conflicting_superposition_elements table.

//    fully_collapsed_present_elements : im::HashSet<QWSElementID>,
//BORIS, rather than saving transactions here, we might be better off saving a set of elements, "fully_collapsed_present_elements",
//which would save all the conflict-free elements in the mask.  Then we have an easy time providing a fully collapsed mask, no
//need to iterate transactions.  When we're back-propagating, we look for each entangled element in here, and if an entanglement
//causes an element to move into the conflicted state, then we remove it from this list and add it to the
//conflicting_superposition_elements.
//When we're merging for forward propagation, ... BORIS.


//THIS WHOLE IDEA of trying to track known conflicts IS FUCKED!!!!!!!  Tracking known non-conflicts (fully collapsed) is ok
// because, somewhere in a transaction's direct past, an element was created and not destroyed
// tracking known conflicts on the other hand means that every mask is aware of the goings-ons of every single other mask
// (in theory).  This will have n^2 scaling properties.
//
//Ideally, if two sets of transactions are totally disconnected from each other, the masks pertaining to one set shouldn't
//bloat with knowledge of which elements are conflicted in the other side of the world...
//MAYBE I JUST LIVE WITH IT!!!!!!

//LATEST REALIZATION:::::  The use case isn't to fully collapse a mask in the abstract, it's to fully collapse a query.
//In a way that makes the implementation a bit easier - we iterate over the elements found by the query.
//
//Buuuuuuuuuut........... We still need to be able to figure out whether a given transaction is in a conflicted state with
//another transaction.  That's a little bit tricky because a transaction can collapse sucessfully, but foreclose collapse of
//another transaction later on.
//
//Tools at our disposal:  We could track at the mask level all of the elements that are touched (i.e. created in the past) but
//remain unconflicted.  Not sure this helps but we can do it.
//
//One option is to create a master conflict list at the world level.  i.e. not part of the mask, but keeping a map from every
//element to every transaction that entangles it.
//THIS IS DEFINITELY PART OF THE SOLITION
//
//END BORIS

    consistent_to : QWSTransactionID //The ID of the last transaction this mask is aware of.  Because TransactionIDs are assigned
    //sequentially, subsequent IDs are not accurately captured in this mask even though subsequent transactions may delete items
    //from this mask.  NOTE: this ID is **EXCLUSIVE** of the transaction.  In other words, if consistent_to == 0, it means this
    //mask is NOT aware of TransactionID 0.  If consistent_to == 5, it means the mask is aware of transactionIDs 0 to 4.
}

impl <T>QWSElementWrapper<T> {
    pub fn new(element_type : QWSElementType, payload : T) -> QWSElementWrapper<T> {
        QWSElementWrapper {
            element_type : element_type,
            payload : payload
        }
    }
    pub fn payload(&self) -> &T {
        &self.payload
    }
}

impl  <T : 'static>QWSElement for QWSElementWrapper<T> {

    fn element_type(&self) -> QWSElementType {
        self.element_type
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl QWSElementRecord {

    fn new(element : Box<dyn QWSElement>, parent_transaction : QWSTransactionID, _new_element_id : QWSElementID) -> QWSElementRecord {
        QWSElementRecord {
            //NOTE: we are keeping the option open to record the QWSElementID in the ElementRecord, but for now there is no use for it.
            element : element,
            created_by : parent_transaction,
        }
    }
}

impl QWSElementStore {
    fn new() -> QWSElementStore {
        QWSElementStore{
            table : Vec::new(),
            types_index : std::collections::HashMap::new()
        }
    }

    fn get(&self, el_id : QWSElementID) -> Option<&QWSElementRecord> {
        self.table.get(el_id.0)
    }

    fn next_new_element_id(&self) -> QWSElementID {
        QWSElementID::from(self.table.len())
    }

    //Adds elements to the QWSElementStore in the order they are provided in the new_element_records vec, with the first element being
    //  assigned the ID of next_new_element_id, and incrementing by 1 for each additional element
    fn add_elements(&mut self, new_element_records : Vec<QWSElementRecord>) {

        //Go over each new element record
        let element_id_base = self.next_new_element_id();
        for (index, element_record) in new_element_records.iter().enumerate() {
            let new_element_id = element_id_base + QWSElementID::from(index);

            //Add it to the index for querying by type
            match self.types_index.get_mut(&element_record.element.element_type()) {
                Some(type_vec) => { type_vec.push(new_element_id); },
                None => { self.types_index.insert(element_record.element.element_type(), vec![new_element_id]); },
            }
        }

        //Add the element records to the table
        self.table.extend(new_element_records);
    }

    //Get an iterator to go over the ID of every element in the QWSElementStore
    fn iter_for_all_elements<'a>(&'a self) -> Box<dyn Iterator<Item=QWSElementID>+'a> {
        Box::new(self.table.iter().enumerate().map(|(index, _record)| QWSElementID::from(index)))
    }
    
    //Get an iterator to go over the IDs for every element of a specified type
    //NOTE: in the future, this is a pattern for how to extend to a general query
    fn iter_for_elements_of_type<'a>(&'a self, element_type : QWSElementType) -> Box<dyn Iterator<Item=QWSElementID>+'a> {
        let created_iter = match self.types_index.get(&element_type) {
            Some(type_vec) => type_vec.iter().cloned(),
            None => [].iter().cloned() //The slice operator is an exception to the general inability to return an empty iterator without it being boxed.  Nice :-)
        };

        Box::new(created_iter)
    }
}

impl std::ops::Index<QWSElementID> for QWSElementStore {
    type Output = QWSElementRecord;

    fn index(&self, idx : QWSElementID) -> &Self::Output {
        &self.table[idx.0]
    }
}

impl QWSTransaction {

    fn new(destroyed_elements : &[QWSElementID], entangled_elements : &[QWSElementID], created_elements : Vec<QWSElementID>, data_mask : QWSQueryMask, new_transaction_id : QWSTransactionID) -> QWSTransaction {
        QWSTransaction {
            id : new_transaction_id,
            created_elements : created_elements,
            destroyed_elements : destroyed_elements.to_vec(),
            entangled_elements : entangled_elements.to_vec(),
            data_mask : data_mask
        }
    }

    pub fn id(&self) -> QWSTransactionID {
        self.id
    }

    pub fn created_elements(&self) -> &[QWSElementID] {
        &self.created_elements[..]
    }
}

impl QuantumWorldState {

    //Creates a new empty QuantumWorldState
    pub fn new() -> QuantumWorldState {

        QuantumWorldState{
            elements : QWSElementStore::new(),
            transactions : Vec::new(),
            uncollapsed_mask : QWSQueryMask::new(QWSTransactionID(0))
        }
    }

    pub fn get_element(&self, element_id : QWSElementID) -> Option<&dyn QWSElement> {
        self.elements.get(element_id).map(|record| &(*record.element))
    }

    pub fn get_transaction(&self, transaction_id : QWSTransactionID) -> Option<&QWSTransaction> {
        self.transactions.get(transaction_id.0)
    }

    pub fn get_creator_transaction(&self, element_id : QWSElementID) -> Option<&QWSTransaction> {
        if let Some(element) = self.elements.get(element_id) {
            self.get_transaction(element.created_by)
        } else {
            None
        }
    }

    pub fn add_transaction(&mut self,
        elements_to_destroy : &[QWSElementID],
        elements_to_entangle : &[QWSElementID],
        elements_to_add : Vec<Box<dyn QWSElement>>
    ) -> Result<&QWSTransaction, QWSError> {

        //Get the ID for the new transaction we're creating
        let new_transaction_id = QWSTransactionID::from(self.transactions.len());

        //Create a element records to hold each of our new elements, and
        //Assemble a vec of the new Element IDs along the way
        let element_id_base = self.elements.next_new_element_id();
        let mut created_element_ids : Vec<QWSElementID> = Vec::with_capacity(elements_to_add.len());
        let element_records : Vec<QWSElementRecord> = elements_to_add.into_iter().enumerate().map(|(index, element)| {
            let new_element_id = element_id_base + QWSElementID::from(index);
            created_element_ids.push(new_element_id);
            QWSElementRecord::new(element, new_transaction_id, new_element_id)
        }).collect();
        
        //Extend the world's uncollapsed_mask, so we have an entry for every element
        //NOTE: Given the implementation of QWSQueryMask, this does nothing and may be wasteful, but probably negligible perf cost.
        //  It's in here for correctness, so that if we change the implementation of QWSQueryMask it will be kept up to date
        self.sync_transactions_with_mask(&self.uncollapsed_mask)?;
        
        //Create the new transaction's data_mask using the data_mask of all of the entangled, deleted, and added elements
        let mut new_transaction_data_mask = self.uncollapsed_mask.clone();

        //Make our data_mask aware that the created and entangled elements are present
        new_transaction_data_mask.add_present_elements(elements_to_entangle)?;
        new_transaction_data_mask.add_present_elements(&created_element_ids[..])?;

        //Make our new data_mask aware of the created, deleted and entangled elements for dependency checking
        new_transaction_data_mask.add_dependent_elements(elements_to_entangle)?;
        new_transaction_data_mask.add_dependent_elements(elements_to_destroy)?;
        new_transaction_data_mask.add_dependent_elements(&created_element_ids[..])?;

        //Go over all entangled elements and merge their data_masks, in order to create the data_mask for our new transaction
        //NOTE: Deleting an element is a form of entangling with it, so we do this identically for deleted and entangled elements
        let all_entangled_elements_iter = elements_to_destroy.iter().chain(elements_to_entangle.iter());
        for entangled_element_id in all_entangled_elements_iter {
            let element_record = &self.elements[*entangled_element_id];
            let transaction_record = &self.transactions[element_record.created_by.0];

            //Bring the mask up to date of the transaction that created the element we're entangling.
            self.sync_transactions_with_mask(&transaction_record.data_mask)?;

            //Merge the mask of the element we're entangling with, using the "dependency merge" behavior
            new_transaction_data_mask.merge_for_base(&transaction_record.data_mask, &self.transactions[..])?;
        }
        QWSQueryMask::add_settled_absent_elements(new_transaction_data_mask.contents.get_mut(), elements_to_destroy)?;
        for &element_id in elements_to_destroy {
            let conflicting_transaction = self.elements[element_id].created_by;
            QWSQueryMask::add_conflicting_transaction(new_transaction_data_mask.contents.get_mut(), conflicting_transaction)?;
        }

        //We don't want this data_mask to attempt to sync with itself later on
        new_transaction_data_mask.set_consistent_to(new_transaction_id + QWSTransactionID::from(1));

        //Push our elements onto the QWS elements vec, now that we're sure the transaction is going to be created sucessfully
        self.elements.add_elements(element_records);
        
        //Create our new transaction record and Push it into the world's table
        let new_transaction_record = QWSTransaction::new(elements_to_destroy, elements_to_entangle, created_element_ids, new_transaction_data_mask, new_transaction_id);
        self.transactions.push(new_transaction_record);

        //Get our transaction record out of the table, so we can return it
        let our_transaction_record = &mut self.transactions[new_transaction_id.0];

        Ok(our_transaction_record)
    }

    //Returns a new QWSDataView for the world, in which all elements are in superposition
    pub fn new_view(&self) -> QWSDataView {
        QWSDataView::new(self)
    }

    //An internal function to propagate transactions backwards, bringing the mask to to date with all transaction in the QuantumWorld
    //This is necessary because future transactions may destroy existing elements, thus entangling the elements in the mask with 
    //  elements created by future transactions
    //
    //BackPropagating Rules:
    //1.) If a future transaction attempts to delete a P element, An Af entry is created for each element the transaction creates.
    //      In this situation, the Af entry references the future transaction that's being back-propagated.
    //2.) If a future transaction attempts to entangle or delete an Ap element, An Ap entry is created for each element the transaction
    //      creates. A conflicting_transactions entry is created for the transaction that's being back-propagated.
    //3.) If a future transaction attempts to entangle or delete an Af element, An Af entry is created for each element the transaction
    //      creates.  In this situation, the Af entry references the union set of transactions from all Af entries that the transaction
    //      entangles with (deletion is a form of entanglement)



    //BEGIN BORIS, WHAT DO WE DO ABOUT conflicting_superposition_elements and non_conflicting_transactions ???????????????????????
    //There are two kinds of elements that exist in superposition.  Those in superposition because its unknown
    //  whether the transaction that creates or deletes them has been included - those cause conflicts, and those in superposition because
    //  they weren't expressly entangled or created in a given transaction, but they're still not guarenteed because that may change in
    //  future.  Those don't cause conflicts.
    //If a transaction deletes an element, check the element's creator transaction.  If the creator transaction is in the non_conflicting_transactions
    //  list, then remove it.  For the deleted element, add an entry for the transaction doing the deletion.  By default, the transaction creating an
    //  element that has an entry in the conflicting_superposition_elements map is a conflicting transaction.
    //QUESTION IS... Do I need to further propagate the information?  In other words, do I care about second-order conflicts??????
    //Maybe I don't need to because If I collapse a base transaction, all future conflicting transactions become AbsentPast.
    //4.)
    //END BORIS
    fn sync_transactions_with_mask(&self, mask : &QWSQueryMask) -> Result<(), QWSError> {

        let mask_contents = &mut(*mask.contents.borrow_mut());

        //Loop over every transaction not already reflected in our mask
        let mut transaction_id = mask_contents.consistent_to;
        while transaction_id < QWSTransactionID::from(self.transactions.len()) {

            let transaction = self.get_transaction(transaction_id).unwrap();

            //RULE 1.) If a future transaction attempts to delete a P element, An Af entry is created for each element the transaction
            // creates. In this situation, the Af entry references the future transaction that's being back-propagated.
            for destroyed_element_id in &transaction.destroyed_elements {
                if mask_contents.present_elements.contains(destroyed_element_id) {
                    QWSQueryMask::add_future_absent_elements(mask_contents, &transaction.created_elements[..], transaction_id)?;
                }
            }

            let all_entangled_elements_iter = transaction.destroyed_elements.iter().chain(transaction.entangled_elements.iter());
            for entangled_element_id in all_entangled_elements_iter {

                //RULE 2.) If a future transaction attempts to entangle or delete an Ap element, An Ap entry is created for each element
                // the transaction creates. (Ap entries don't reference transactions)
                if mask_contents.settled_absent_elements.contains(entangled_element_id) {
                    QWSQueryMask::add_settled_absent_elements(mask_contents, &transaction.created_elements[..])?;
                    QWSQueryMask::add_conflicting_transaction(mask_contents, transaction_id)?;
                }

                //RULE 3.) If a future transaction attempts to entangle or delete an Af element, An Af entry is created for each element
                // the transaction creates.  In this situation, the Af entry references the union set of transactions from all Af entries
                // that the transaction entangles with (deletion is a form of entanglement)
                //NOTE: We aren't checking first because of the behavior of propagate_to_future_absent_elements.  It already includes
                //  the check.  If there is no AbsentFuture entry for element_to_propagate, then it will do nothing
                QWSQueryMask::propagate_to_future_absent_elements(mask_contents, &transaction.created_elements[..], *entangled_element_id)?;
            }

            transaction_id += QWSTransactionID::from(1);
        }

        //Reflect that this mask is now consistent, to the present
        mask_contents.consistent_to = QWSTransactionID::from(self.transactions.len());

        Ok(())
    }

}

impl <'a>QWSDataView<'a> {

    fn new(quantum_world : &'a QuantumWorldState) -> QWSDataView<'a> {

        //Create our new partially collapsed mask
        //Start with a data_mask for every element in superposition, that represents the QuantumWorldState in totality
        let new_data_view = QWSDataView {
            quantum_world : quantum_world,
            collapsed_transactions : vec![],
            data_mask : quantum_world.uncollapsed_mask.clone(),
            query : None,
            full_collapse_plan : None
        };

        new_data_view
    }

    //Returns a pertially collapsed view in which each of the supplied transactions occurred.  Therefore all other elements that are entangled
    //  with the supplied transactions will either be known to exist or be known not to exist.  If two or more of the supplied transactions
    //  reference elements that cannot exist at the same epoch, then this function will return None.
    pub fn collapse(mut self, collapse_transactions : &[QWSTransactionID]) -> Result<Self, QWSError> {

        //Loop over the elements, and further collapse the data_mask around each one
        for transaction_id in collapse_transactions.into_iter() {

            let transaction_record = &self.quantum_world.transactions[transaction_id.0];

            //Bring each of the transaction_record's masks up to date.
            self.quantum_world.sync_transactions_with_mask(&transaction_record.data_mask)?;

            //Merge all of the masks together
            self.data_mask.merge_for_collapse(&transaction_record.data_mask)?;
        }

        //Add the transactions to the collapsed_transactions list
        self.collapsed_transactions.extend(collapse_transactions);

        //Void the full collapse plan
        self.full_collapse_plan = None;

        Ok(self)
    }

    pub fn fully_collapse(mut self) -> Result<Self, QWSError> {

        //BORIS, somehow we need to loop over the elements in superposition or the transactions that got them there
        //BORIS!!!

        Ok(self)
    }

    pub fn get_element_status(&self, element_id : QWSElementID) -> QWSElementStatus {
        if element_id < self.quantum_world.elements.next_new_element_id() { // If the element_id is outside the range tracked by the elements table, the mask won't have a meaningful entry for it either, but we don't want to assume it's in superposition - although, in a sense, it is.
            let internal_status = self.data_mask.get_element_internal_status(element_id);
            match internal_status {
                QWSInternalElementStatus::AbsentFuture => QWSElementStatus::KnownAbsent,
                QWSInternalElementStatus::Present => QWSElementStatus::KnownPresent,
                QWSInternalElementStatus::Superposition => QWSElementStatus::Superposition,
                QWSInternalElementStatus::AbsentPast => QWSElementStatus::KnownAbsent
            }
        } else {
            QWSElementStatus::Unknown
        }
    }

    pub fn get_collapsed_transactions(&self) -> &[QWSTransactionID] {
        &self.collapsed_transactions[..]
    }

    //NOTE: Conceptually, this shouldn't be a mutating API, but we need a mutable reference to self, because we do the work
    //  to compile the conflicting transactions lazily and we want to store the results inside the object, which will speed up
    //  subsequent calls as well as fully_collapse(), since the two are related.
    //An alternative is to store the lazily evaluated parts inside a RefCell, but that would mean we couldn't pass back a naked slice,
    //  and instead would need to return a "QWSTransactionIterator" object to encapsulate the Ref, from borrowing the RefCell contents
    //A third alternative would be to force the caller to make a call to build the list, e.g. "build_conflicting_transactions_list",
    //  but that feels incredibly ugly.
    pub fn get_conflicting_transactions(&mut self) -> &[QWSTransactionID] {

        self.build_full_collapse_plan(); //If a plan already exists, this will just return without any cost

        match &self.full_collapse_plan {
            Some(the_plan) => &the_plan.conflicting_transactions,
            None => unreachable!(), //We just built the full_collapse_plan, it won't be None
        }
    }

    //Narrows the elements in the view to contain only elements whose type matches the element_type parameter
    pub fn query_by_type(mut self, element_type : QWSElementType) -> Result<Self, QWSError> {

        //Void the full collapse plan
        self.full_collapse_plan = None;

        //Set the query internals object
        if self.query.is_none() {
            self.query = Some(QWSQueryInternals{query_by_type : element_type});
            Ok(self)
        } else {
            Err(QWSError::MiscErr(format!("UNSUPPORTED: Attempt to specify multiple queries.  Currently only a single element type may be queried.")))
        }
    }

    pub fn elements_iter(&self) -> QWSElementsIterator {

        let elements_iter = self.query_result_elements_iter();

        QWSElementsIterator{
            data_view : self,
            elements_iter : elements_iter
        }
    }

    fn query_result_elements_iter(&self) -> Box<dyn Iterator<Item=QWSElementID>+'a> {
        //If we have a query, we want to iteate over the results of that query
        if let Some(query) = &self.query {
            self.quantum_world.elements.iter_for_elements_of_type(query.query_by_type)
        } else {
            //If we don't have a query, we want to iterate over every element
            self.quantum_world.elements.iter_for_all_elements()
        }
    }

    //Internal function to determine if a full collapse is possible, and create the view if it is
    fn build_full_collapse_plan(&mut self) {
        if self.full_collapse_plan.is_none() {

            //BORIS, REALIZATION(s)!!  We don't need to attempt to collapse conflicting transactions in order to know if they'll hit a snag
            //  We are actually fine to just look at the conflicting transaction list, THIS means we don't actually need to make the data_view part of the collapse plan!!
            //buuuuuuut, I also realized we need to consider conflicts in AbsentFuture entries as well as straight-up conflicts...  Which probably means we'd do well to
            //maintain an AbsentFuture conflict set, so that we can look up by transaction, not just by element

            //BORIS DEAD
            // //Clone the fully_collapsed_mask from the view's data_mask
            // let mut new_mask = self.data_mask.clone();

            //Sets to hold the transactions we'll be processing
            let mut freely_collapsible_transactions = im::HashSet::new();
            let mut conflicting_transactions = im::HashSet::new();

            //A set of the transactions that we know we conflict with, given the transactions we've already looked at
            let mut conflict_master_set = im::HashSet::new();

            //Loop over each query result
            for element_id in self.query_result_elements_iter() {

                match self.get_element_status(element_id) {
                    QWSElementStatus::KnownAbsent => (), //If the result is absent then we don't need to worry about it
                    QWSElementStatus::KnownPresent => (), //If the result is already present, nothing to do with it
                    QWSElementStatus::Unknown => unreachable!(), //There shouldn't be any Unknown elements in our world
                    QWSElementStatus::Superposition => {
                        //If the result is in superposition, then try to collapse the element's creator transaction into
                        //  the fully_collapsed_mask
                        let creator_transaction_id = self.quantum_world.elements.get(element_id).unwrap().created_by; //unwrap should never fail because we got the element_id from the iterator

                        //See if we've already processed this transaction
                        if !freely_collapsible_transactions.contains(&creator_transaction_id) &&
                        !conflicting_transactions.contains(&creator_transaction_id) {

                            //Bring the transaction up to date before making a determination about whether it conflicts or not
                            let transaction_record = &self.quantum_world.transactions[creator_transaction_id.0];
                            self.quantum_world.sync_transactions_with_mask(&transaction_record.data_mask).unwrap(); //If this unwrap fails, it's an internal error and needs to be debugged

                            //Get the transaction's data_mask contents
                            let transaction_mask_contents = transaction_record.data_mask.contents.borrow();

                            //BORIS DEAD
                            //Attempt to merge the creator transaction's mask into the new fully collapsed mask
                            // let function_result = new_mask.merge_for_collapse(&transaction_record.data_mask);
                            
                            // if !function_result.is_err() {
                            //     //If the collapse succeeded, add the creator transaction to the additional_collapsed_transactions set
                            //     newly_collapsed_transactions.insert(creator_transaction_id);
                            // } else {
                            //     //If the collapse failed, add the transaction to the conflicting_transactions set
                            //     conflicting_transactions.insert(creator_transaction_id);

                            //     //And then find the counter-transaction that we already merged, that caused us to fail.  Pluck it out
                            //     //  of the additional_collapsed_transactions set and also add it to the conflicting_transactions set.

                            // }

                            //See if the transaction conflicts with any of the ones we've already looked at
                            if !conflict_master_set.contains(&creator_transaction_id) {
                                //If there is no conflict, add the creator transaction to the freely_collapsible_transactions set
                                freely_collapsible_transactions.insert(creator_transaction_id);
        println!("Boris FREE Collapse {}", creator_transaction_id);
                            } else {
                                //If there is a conflict, add the transaction to the conflicting_transactions set
                                conflicting_transactions.insert(creator_transaction_id);
        println!("Boris CONFLICT {}", creator_transaction_id);

                                //And then find the counter-transactions that caused us to fail.  Pluck them out
                                //  of the freely_collapsible_transactions set and add them to the conflicting_transactions set.
                                //Conflicts should be reciprocal, so the intersection set of the freely_collapsible_transactions set
                                //  and this transaction's conflict set should be the transactions we need to move
        println!("Transaction Past conflict el.1= {}, el.2= {}", transaction_mask_contents.conflicting_transactions.iter().nth(0).unwrap(), transaction_mask_contents.conflicting_transactions.iter().nth(1).unwrap());
                                let transaction_conflicting_transactions = transaction_mask_contents.conflicting_transactions.clone().union(transaction_mask_contents.future_conflicting_transactions.clone());
        println!("ConflictList len {}", transaction_conflicting_transactions.iter().count());
                                let conflicting_counter_transactions = freely_collapsible_transactions.clone().intersection(transaction_conflicting_transactions);
        println!("CounterTransaction Len {}", conflicting_counter_transactions.iter().count());
                                let new_freely_collapsible_transactions = freely_collapsible_transactions.relative_complement(conflicting_counter_transactions.clone()); //This is a "set subtract"
                                let new_conflicting_transactions = conflicting_transactions.union(conflicting_counter_transactions); //This is a "set add"

                                freely_collapsible_transactions = new_freely_collapsible_transactions;
                                conflicting_transactions = new_conflicting_transactions;
                            }

                            //Add this transaction's conflicts to the conflict_master_set
                            conflict_master_set = im::HashSet::unions(vec![conflict_master_set,
                                transaction_mask_contents.conflicting_transactions.clone(),
                                transaction_mask_contents.future_conflicting_transactions.clone()]);
                        }   
                    }
                }   
            }

            //BORIS.
            //NOTES from DEBUGGGING.
            //Transaction 1 (and others) should have a past conflict with T0, because it deletes elements from T0.  The fact that no such conflict exists is the source of one bug
            //But I need to contemplate whether it's a bug to have T3 & T4 included in the conflict list.  They inherit their conflicted status from T1 & T2 respectively.
            //  It would be nice to point out only the conflicting transactions that are "crucial", i.e. at a crossroads.  But that wouldn't be correct in all circumstances.
            //  For this test, we could get away with only reporting back T0, T1, & T2 as the conflicted transactions, even though T1 has a conflict with T4 and T2 has a conflict
            //  with T3.  This is because addressing the issue by collapsing any of the T1-T4 transactions effectively takes the conflicting transactions out of consideration.
            //  But consider what would happen if the query further narrowed the results in an unexpected way.
            //
            //DECISION: Inherited conflicts are still conflicts
            //
            //BORIS: This also points to the fact that I need a test with 2 different non-conflicting pockets of elements.
            //  Imaging Creating A1 & B1 all in the same transaction, and then 2 separate transactions, A2 replaces A1, B2 replaces B1,
            //  Then 2 more, where A3 replaces A2 and B3 replaces B2.  Finally, we collapse B3, and the conflicting transaction iter should
            //  only allow iteration of the A transactions.

            //BORIS
            for trans in conflicting_transactions.iter() {
                println!("RESULT!!!  T= {}", trans);
            }
     
            let new_plan = QWSFullCollapsePlan{
                //already_fully_collapsed : false,
                //fully_collapsed_mask : new_mask, BORIS dead
                freely_collapsible_transactions : freely_collapsible_transactions.into_iter().collect(),
                conflicting_transactions : conflicting_transactions.into_iter().collect(),
            };

            self.full_collapse_plan = Some(new_plan);
        }
    }
}

impl Iterator for QWSElementsIterator<'_, '_> {
    type Item = QWSElementID;
    
    fn next(&mut self) -> Option<QWSElementID> {

        //We will filter the output from the query results iterator based on the data_view's mask
        while let Some(candidate_element_id) = self.elements_iter.next() {

            match self.data_view.get_element_status(candidate_element_id) {
                QWSElementStatus::KnownPresent => return Some(candidate_element_id),
                QWSElementStatus::Superposition => return Some(candidate_element_id),
                QWSElementStatus::KnownAbsent => (),
                QWSElementStatus::Unknown => ()
            }
        }

        //If we get here, we're out of query results
        None
    }
}

impl QWSQueryMask {

    //Creates a new empty QWSQueryMask, where all known elements are in superposition
    //BORIS, this comment likely unnecessary... NOTE: This should only be called when a new QuantumWorldState is created, to create the uncollapsed_mask.  All subsequent operations
    //  that require a mask should clone an existing one
    fn new(consistent_to : QWSTransactionID) -> QWSQueryMask {
        QWSQueryMask {
            contents : RefCell::new(QWSQueryMaskContents {
                present_elements : im::HashSet::new(),
                settled_absent_elements : im::HashSet::new(),
                future_absent_elements : im::HashMap::new(),
                dependent_elements : im::HashSet::new(),
                conflicting_transactions : im::HashSet::new(),
                future_conflicting_transactions : im::HashSet::new(),
//                conflicting_superposition_elements : im::HashMap::new(), BORIS
//                non_conflicting_transactions : im::HashSet::new(),
                consistent_to : consistent_to
            })
        }
    }

    fn set_consistent_to(&mut self, consistent_to : QWSTransactionID) {
        let self_contents = self.contents.get_mut();
        self_contents.consistent_to = consistent_to;
    }

    //This function merges the elements in other_mask into self.  Used to create the data_mask for a new transaction
    //This function is at the heart of creating new transactions, so my fear is that it might be too slow.
    //  If this is too slow in the future, we may need to revisit this implementation and possibly this whole design...
    //
    //"Cloning Behavior" Rules:
    //1. Make sure neither mask's Dependent set conflicts with the other mask's AbsentPast set, if they do then it's an error
    //2. union the two AbsentPast sets and the two conflicting_transactions sets
    //3. Evaluate each absentFuture entry we're merging in, examining each transaction, and if that transaction conflicts
    //      with the mask we're merging, then degenerate the AbsentFuture entry to an absentPast entry, and if not,
    //      ignore the element, effectively makeing it Superposition.
    //4. There is nothing to do for our AbsentFuture or Present entries.  A freshly created transaction data_mask has no
    //      Present entries except what it created or entangled itself and no AbsentFuture entries whatsoever, because
    //      by definition, AbsentFuture entries occur when a future transaction references the elements Present here.
    fn merge_for_base(&mut self, other_mask : &QWSQueryMask, transactions : &[QWSTransaction]) -> Result<(), QWSError> {

        let self_contents = self.contents.get_mut();
        let other_contents = &(*other_mask.contents.borrow());

        //RULE 1.) Make sure there are no conflicts between Dependent and AbsentPast elements
        let mut conflicting_element_id = QWSElementID::default();
        if QWSQueryMask::check_for_conflicts(self_contents, other_contents, Some(&mut conflicting_element_id)) {
            return Err(QWSError::MiscErr(format!("CONFLICT on element: {} when attempting to create transaction", conflicting_element_id)));
        }

        //RULE 2.) Merge the AbsentPast element sets and the conflicting_transactions sets
        //NOTE: this swap saves us needing to clone out HashSets, but instead we need to create and discard an empty set.
        //Cloning the other mask's sets seems unavoidable given the API, but better to clone one mask's sets than both.
        //  I haven't measured to see if we actually come out ahead.
        let mut temp_set = im::HashSet::new();
        swap(&mut self_contents.settled_absent_elements, &mut temp_set);
        self_contents.settled_absent_elements = temp_set.union(other_contents.settled_absent_elements.clone());
        let mut temp_set = im::HashSet::new();
        swap(&mut self_contents.conflicting_transactions, &mut temp_set);
        self_contents.conflicting_transactions = temp_set.union(other_contents.conflicting_transactions.clone());

        //RULE 3.) Evaluate the AbsentFuture elements in the other_mask to see whether we should promote it to a settled_absent element
        let mut conflict_cache : std::collections::HashMap<QWSTransactionID, bool> = std::collections::HashMap::with_capacity(8); //A temporary cache so we don't end up checking the same transactions multiple times
        for (&future_absent_element, transaction_id_vec) in other_contents.future_absent_elements.iter() {

            for transaction_id in transaction_id_vec.iter() {

                if let Some(cached_status) = conflict_cache.get(transaction_id) {
                    if *cached_status {
                        self_contents.settled_absent_elements.insert(future_absent_element);
                        QWSQueryMask::add_conflicting_transaction(self_contents, *transaction_id)?;
                    }
                } else {
                    let referenced_transaction = &transactions[transaction_id.0]; //If we have an invalid TransactionID, it's an internal bug and panicking is the right thing to do

                    if QWSQueryMask::check_for_conflicts(self_contents, &(*referenced_transaction.data_mask.contents.borrow()), None) {
                        self_contents.settled_absent_elements.insert(future_absent_element);
                        QWSQueryMask::add_conflicting_transaction(self_contents, *transaction_id)?;
                        conflict_cache.insert(*transaction_id, true);
                    } else {
                        conflict_cache.insert(*transaction_id, false);
                    }
                }
            }            
        }

        //RULE 4.) There is no RULE 4!

        Ok(())
    }

    //This function merges the elements in other_mask into self.  Used to create the data_mask for a collapsed world state
    //This function is at the heart of collapsing the QWS, so my fear is that it might be too slow.
    //  If this is too slow in the future, we may need to revisit this implementation and possibly this whole design...
    //
    //"Collapsing Behavior" Rules:
    //1. Promote every entry in the AbsentFuture set to the AbsentPast set.  (In this case, the description of "settled" is
    //      more appropriate than the description of "past")  Since the end result will be a merged mask, we'll actually
    //      promote AbsentFuture entries in other_mask into AbsentPast entries in self.  It shouldn't actually matter, and
    //      we have mutable access to self.  Also promote the conflicting_transactions in the process.
    //2. Make sure neither mask's Dependent set conflicts with the other mask's AbsentPast set, if they do then it's an error
    //3. union the two AbsentPast sets and conflicting_transactions sets
    //4. union the two Present sets
    fn merge_for_collapse(&mut self, other_mask : &QWSQueryMask) -> Result<(), QWSError> {

        let self_contents = self.contents.get_mut();
        let other_contents = &(*other_mask.contents.borrow());

        //RULE 1.) Promote AbsentFuture entries to settled_absent elements, and promote their transactions to full-on conflicts
        for (&future_absent_element, transaction_id_vec) in other_contents.future_absent_elements.iter() {            
            self_contents.settled_absent_elements.insert(future_absent_element);
            for &conflicting_transaction_id in transaction_id_vec {
                QWSQueryMask::add_conflicting_transaction(self_contents, conflicting_transaction_id)?;
            }
        }

        //RULE 2.) Make sure there are no conflicts between Dependent and AbsentPast elements
        let mut conflicting_element_id = QWSElementID::default();
        if QWSQueryMask::check_for_conflicts(self_contents, other_contents, Some(&mut conflicting_element_id)) {
            return Err(QWSError::MiscErr(format!("CONFLICT on element: {} when attempting to create transaction", conflicting_element_id)));
        }

        //RULE 3.) Merge the AbsentPast element sets (See NOTE on Rule 2 for merge_for_base)
        let mut temp_set = im::HashSet::new();
        swap(&mut self_contents.settled_absent_elements, &mut temp_set);
        self_contents.settled_absent_elements = temp_set.union(other_contents.settled_absent_elements.clone());
        let mut temp_set = im::HashSet::new();
        swap(&mut self_contents.conflicting_transactions, &mut temp_set);
        self_contents.conflicting_transactions = temp_set.union(other_contents.conflicting_transactions.clone());
        
        //RULE 4.) Merge the Present element sets (See NOTE on Rule 2 for merge_for_base)
        let mut temp_set = im::HashSet::new();
        swap(&mut self_contents.present_elements, &mut temp_set);
        self_contents.present_elements = temp_set.union(other_contents.present_elements.clone());
        
        Ok(())
    }

    //This internal function checks two mask content structures against eachother, and will return true if there is a conflict and false if there isn't any
    //A conflict, in this case, is defined narrowly to mean that a settled absent element (aka AbsentPast) in one mask is Present in the other.
    //NOTE: AbsentFuture elements aren't taken into account by this function
    //The conflicting_element optional parameter provides a way to get back the QWSElementID of the first found conflicting element.  If there are multiple
    //  conflicting elements, it is unspecified which one will be returned, and it is further not guaranteed to be the same element between invocations of
    //  the function.
    fn check_for_conflicts(mask_a_contents : &QWSQueryMaskContents, mask_b_contents : &QWSQueryMaskContents, conflicting_element : Option<&mut QWSElementID>) -> bool {

        let intersection_set = mask_a_contents.dependent_elements.clone().intersection(mask_b_contents.settled_absent_elements.clone());
        if intersection_set.len() > 0 {
            if let Some(conflicting_element_id) = conflicting_element {
                *conflicting_element_id = *intersection_set.iter().next().unwrap();
            }
            return true;
        }
        let intersection_set = mask_b_contents.dependent_elements.clone().intersection(mask_a_contents.settled_absent_elements.clone());
        if intersection_set.len() > 0 {
            if let Some(conflicting_element_id) = conflicting_element {
                *conflicting_element_id = *intersection_set.iter().next().unwrap();
            }
            return true;
        }
        
        false
    }

    //Sets all of the elements specified in the slice to be "KnownPresent" in the datamask
    fn add_present_elements(&mut self, elements : &[QWSElementID]) -> Result<(), QWSError> {

        let self_contents = self.contents.get_mut();

        //NOTE: Should we include a debug-check to make sure the elements we're flagging aren't already in another set,
        //  like we did in add_settled_absent_elements?

        //Add the elements to the present_elements_set.  It should end up containing all of the created element ids + all of the entangled element ids
        let new_elements_set: std::collections::HashSet<QWSElementID> = elements.iter().cloned().collect();
        self_contents.present_elements.extend(new_elements_set);

        Ok(())
    }

    //Adds all of the elements specified in the slice to the data_mask's dependency set
    fn add_dependent_elements(&mut self, elements : &[QWSElementID]) -> Result<(), QWSError> {

        let self_contents = self.contents.get_mut();

        //Add the elements to the dependent_elements set.  It should end up containing all of the deleted element ids + all of the entangled element ids
        let new_elements_set: std::collections::HashSet<QWSElementID> = elements.iter().cloned().collect();
        self_contents.dependent_elements.extend(new_elements_set);

        Ok(())
    }

    //Add an AbsentPast entry for an element.
    //NOTE: we are passing the "contents" structure because we can't always mutably borrow the QueryMask
    fn add_settled_absent_elements(mask_contents : &mut QWSQueryMaskContents, elements : &[QWSElementID]) -> Result<(), QWSError> {

        for &element in elements {

            //This should never happen, we're just checking so we'll catch the error as early as possible
            //This will be optimized away in a release build
            if cfg!(debug_assertions) {
                if mask_contents.present_elements.contains(&element) || mask_contents.future_absent_elements.contains_key(&element) {
                    panic!("INTERNAL ERROR: attempt to set AbsentPast status on element that already has another state");
                }
            }

            mask_contents.settled_absent_elements.insert(element);
        }
        
        Ok(())
    }

    fn add_conflicting_transaction(mask_contents : &mut QWSQueryMaskContents, transaction_id : QWSTransactionID) -> Result<(), QWSError> {

        mask_contents.conflicting_transactions.insert(transaction_id);
        Ok(())
    }

    //Add an AbsentFuture entry for an element.
    // First check to see if the future_absent_elements map contains a vec for this element, and append the transaction id to that vec
    //  if it already exists, otherwise we need to create a new SmallVec.
    //NOTE: we are passing the "contents" structure because we can't always mutably borrow the QueryMask
    fn add_future_absent_elements(mask_contents : &mut QWSQueryMaskContents, elements : &[QWSElementID], blame_transaction : QWSTransactionID) -> Result<(), QWSError> {

        for &element in elements {

            //This should never happen, we're just checking so we'll catch the error as early as possible
            //This will be optimized away in a release build
            if cfg!(debug_assertions) {
                if mask_contents.present_elements.contains(&element) || mask_contents.settled_absent_elements.contains(&element) {
                    panic!("INTERNAL ERROR: attempt to set AbsentFuture status on element that already has another state");
                }
            }
            
            if let Some(transaction_vec) = mask_contents.future_absent_elements.get_mut(&element) {
                transaction_vec.push(blame_transaction);
                //NOTE: It would be interesting to measure how many times we overflow our SmallVec, to see if we would
                //  get a performance boost from increasing the allocation size.  (Currently 1)
            } else {
                mask_contents.future_absent_elements.insert(element, smallvec![blame_transaction]);
            }
        }

        //Record the transaction in the future_conflicting_transactions set
        mask_contents.future_conflicting_transactions.insert(blame_transaction);
    
        Ok(())
    }

    //Add an AbsentFuture entry for elements based on an existing AbsentFuture entry
    //If there is no AbsentFuture entry for element_to_propagate, this function does nothing
    //NOTE: we are passing the "contents" structure because we can't always mutably borrow the QueryMask
    fn propagate_to_future_absent_elements(mask_contents : &mut QWSQueryMaskContents, elements : &[QWSElementID], element_to_propagate : QWSElementID) -> Result<(), QWSError> {

        if let Some(source_vec) = mask_contents.future_absent_elements.get(&element_to_propagate) {

            //NOTE: We end up making one extra clone here that could be avoided if we refactored this function with two code paths
            //It probably won't matter for performance.
            let cloned_source_vec = source_vec.clone();

            for &element in elements {
                
                if let Some(transaction_vec) = mask_contents.future_absent_elements.get_mut(&element) {
                    transaction_vec.extend(cloned_source_vec.clone());
                    //NOTE: It would be interesting to measure how many times we overflow our SmallVec, to see if we would
                    //  get a performance boost from increasing the allocation size.  (Currently 1)
                } else {
                    mask_contents.future_absent_elements.insert(element, cloned_source_vec.clone());
                }    
            }

            //Note: There shouldn't be a need to update the future_conflicting_transactions set, because every transaction we're working
            // with in this function comes from an existing future_absent_elements entry and therefore is already accounted for in
            // future_conflicting_transactions
        }
        
        Ok(())
    }

    //Returns the internal status of an element in a mask
    //In Debug builds, the function also Validates that the element isn't in more than one set.
    //  This should never happen unless there is an internal bug in this QuantumWorldState module
    //  This checking code should be optimized away completely in release builds
    fn get_element_internal_status(&self, element_id : QWSElementID) -> QWSInternalElementStatus {
        
        let contents = self.contents.borrow();
        let mut return_status = QWSInternalElementStatus::Superposition;

        if contents.present_elements.contains(&element_id) {

            return_status = QWSInternalElementStatus::Present;

            if !cfg!(debug_assertions) {
                return return_status;
            }
        }

        if contents.settled_absent_elements.contains(&element_id) {

            if cfg!(debug_assertions) && return_status != QWSInternalElementStatus::Superposition {
                panic!("INTERNAL ERROR: Element in multiple exclusive sets");
            }

            return_status = QWSInternalElementStatus::AbsentPast;
            
            if !cfg!(debug_assertions) {
                return return_status;
            }
        }

        if contents.future_absent_elements.contains_key(&element_id) {

            if cfg!(debug_assertions) && return_status != QWSInternalElementStatus::Superposition {
                panic!("INTERNAL ERROR: Element in multiple exclusive sets");
            }   

            return_status = QWSInternalElementStatus::AbsentFuture;
        }

        return_status
    }

}

impl Display for QWSElementStatus {
    fn fmt(&self, f : &mut Formatter<'_>) -> Result<(), Error> {
        match &self {
            QWSElementStatus::KnownAbsent => write!(f, "KnownAbsent"),
            QWSElementStatus::KnownPresent => write!(f, "KnownPresent"),
            QWSElementStatus::Superposition => write!(f, "Superposition"),
            QWSElementStatus::Unknown => write!(f, "Unknown"),
        }
    }
}

// Tests

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_chain() {
        let mut quantum_world_state = QuantumWorldState::new();

        //This chart shows what the status of each element should be internally at each transaction epoch
        // Af=AbsentFuture, P=Present, S=Superposition, Ap=AbsentPast
        //    |                              | Based from    |  Zer One Two
        // T0 |	Create Zero  	             | -             |  P   Af  Af  
        // T1 | Destroy Zero, Create One     | T0            |  Ap  P   Af  
        // T2 |	Destroy One, Create Two	     | T1            |  Ap  Ap  P 

        //Add an element, collapse around the element, and confirm it's KnownPresent
        //This tests that the transaction sucessfully added an element
        let transaction = quantum_world_state.add_transaction(&[], &[], vec![Box::new(QWSElementWrapper::new(QWSElementType::GenericText, "Zero"))]).unwrap();
        let &zero_id = transaction.created_elements().first().unwrap();
        let zero_trans_id = transaction.id();
        let collapsed_view = quantum_world_state.new_view().collapse(&vec![zero_trans_id]).unwrap();
        assert_eq!(collapsed_view.get_element_status(zero_id), QWSElementStatus::KnownPresent);

        //Add element "One" through a transaction that deletes element "Zero"
        //Then collapse around element "One", and confirm that it's KnownPresent
        //This tests that we can sucessfully add a second element 
        let transaction = quantum_world_state.add_transaction(&vec![zero_id], &[], vec![Box::new(QWSElementWrapper::new(QWSElementType::GenericText, "One"))]).unwrap();
        let &one_id = transaction.created_elements().first().unwrap();
        let one_trans_id = transaction.id();
        let collapsed_view = quantum_world_state.new_view().collapse(&vec![one_trans_id]).unwrap();
        assert_eq!(collapsed_view.get_element_status(one_id), QWSElementStatus::KnownPresent);

        //Confirm that element "Zero" is KnownAbsent
        //This tests that we can delete an element sucessfully
        assert_eq!(collapsed_view.get_element_status(zero_id), QWSElementStatus::KnownAbsent);

        //Collapse around element "Zero", and confirm that element "Zero" is now KnownPresent
        //This tests that we can collapse around an element to arrive at an epoch that isn't the latest
        let collapsed_view = quantum_world_state.new_view().collapse(&vec![zero_trans_id]).unwrap();
        assert_eq!(collapsed_view.get_element_status(zero_id), QWSElementStatus::KnownPresent);

        //Confirm element "One", that was added after the element "Zero" transaction is KnownAbsent.
        //This tests the logic to back-propagate future transactions to prior epochs.
        assert_eq!(collapsed_view.get_element_status(one_id), QWSElementStatus::KnownAbsent);

        //Confirm that we get an error if we try to collapse around element "One" and element "Two"
        //  at the same time because they can't coexist in the same epoch
        assert!(quantum_world_state.new_view().collapse(&vec![zero_trans_id, one_trans_id]).is_err(), "elements should not be allowed to coexist!");

        //Add a third element, "Two", that destroys element "One"
        let transaction = quantum_world_state.add_transaction(&vec![one_id], &[], vec![Box::new(QWSElementWrapper::new(QWSElementType::GenericText, "Two"))]).unwrap();
        let &two_id = transaction.created_elements().first().unwrap();
        let two_trans_id = transaction.id();

        //Confirm that when we query the collapsed state we only get the one result
        let collapsed_view = quantum_world_state.new_view().collapse(&vec![two_trans_id]).unwrap();
        let queried_view = collapsed_view.query_by_type(QWSElementType::GenericText).unwrap();
        let found_elements : Vec<QWSElementID> = queried_view.elements_iter().collect();
        assert_eq!(found_elements.len(), 1);
        assert_eq!(two_id, found_elements[0]);
    }

    #[test]
    fn parallel_chains() {
        let mut quantum_world_state = QuantumWorldState::new();

        //Add two parallel chains of elements with no entanglement between them
        let &a1_id = quantum_world_state.add_transaction(&[], &[], vec![Box::new(QWSElementWrapper::new(QWSElementType::GenericText, "A1"))])
            .unwrap().created_elements().first().unwrap();
        let &a2_id = quantum_world_state.add_transaction(&vec![a1_id], &[], vec![Box::new(QWSElementWrapper::new(QWSElementType::GenericText, "A2"))])
            .unwrap().created_elements().first().unwrap();
        let &a3_id = quantum_world_state.add_transaction(&vec![a2_id], &[], vec![Box::new(QWSElementWrapper::new(QWSElementType::GenericText, "A3"))])
            .unwrap().created_elements().first().unwrap();
        let &b1_id = quantum_world_state.add_transaction(&[], &[], vec![Box::new(QWSElementWrapper::new(QWSElementType::GenericText, "B1"))])
            .unwrap().created_elements().first().unwrap();
        let &b2_id = quantum_world_state.add_transaction(&vec![b1_id], &[], vec![Box::new(QWSElementWrapper::new(QWSElementType::GenericText, "B2"))])
            .unwrap().created_elements().first().unwrap();
        let &b3_id = quantum_world_state.add_transaction(&vec![b2_id], &[], vec![Box::new(QWSElementWrapper::new(QWSElementType::GenericText, "B3"))])
            .unwrap().created_elements().first().unwrap();

        //Partially collapse around A3, and confirm the other elements are in the state we'd expect
        let a3_trans_id = quantum_world_state.get_creator_transaction(a3_id).unwrap().id();
        let mut collapsed_view = quantum_world_state.new_view().collapse(&vec![a3_trans_id]).unwrap();
        assert_eq!(collapsed_view.get_element_status(a1_id), QWSElementStatus::KnownAbsent);
        assert_eq!(collapsed_view.get_element_status(a2_id), QWSElementStatus::KnownAbsent);
        assert_eq!(collapsed_view.get_element_status(a3_id), QWSElementStatus::KnownPresent);
        assert_eq!(collapsed_view.get_element_status(b1_id), QWSElementStatus::Superposition);
        assert_eq!(collapsed_view.get_element_status(b2_id), QWSElementStatus::Superposition);
        assert_eq!(collapsed_view.get_element_status(b3_id), QWSElementStatus::Superposition);

        //Further collapse around B1, and confirm the other elements are in the state we'd expect
        let b1_trans_id = quantum_world_state.get_creator_transaction(b1_id).unwrap().id();
        collapsed_view = collapsed_view.collapse(&vec![b1_trans_id]).unwrap();
        assert_eq!(collapsed_view.get_element_status(a1_id), QWSElementStatus::KnownAbsent);
        assert_eq!(collapsed_view.get_element_status(a2_id), QWSElementStatus::KnownAbsent);
        assert_eq!(collapsed_view.get_element_status(a3_id), QWSElementStatus::KnownPresent);
        assert_eq!(collapsed_view.get_element_status(b1_id), QWSElementStatus::KnownPresent);
        assert_eq!(collapsed_view.get_element_status(b2_id), QWSElementStatus::KnownAbsent);
        assert_eq!(collapsed_view.get_element_status(b3_id), QWSElementStatus::KnownAbsent);

        //Partially collapse only around B3, and confirm the other elements are in the state we'd expect
        let b3_trans_id = quantum_world_state.get_creator_transaction(b3_id).unwrap().id();
        let collapsed_view = quantum_world_state.new_view().collapse(&vec![b3_trans_id]).unwrap();
        assert_eq!(collapsed_view.get_element_status(a1_id), QWSElementStatus::Superposition);
        assert_eq!(collapsed_view.get_element_status(a2_id), QWSElementStatus::Superposition);
        assert_eq!(collapsed_view.get_element_status(a3_id), QWSElementStatus::Superposition);
        assert_eq!(collapsed_view.get_element_status(b1_id), QWSElementStatus::KnownAbsent);
        assert_eq!(collapsed_view.get_element_status(b2_id), QWSElementStatus::KnownAbsent);
        assert_eq!(collapsed_view.get_element_status(b3_id), QWSElementStatus::KnownPresent);
        
    }

    #[test]
    fn co_created_elements() {
        let mut quantum_world_state = QuantumWorldState::new();

        //This chart shows what the status of each element should be internally at each transaction epoch
        // Af=AbsentFuture, P=Present, S=Superposition, Ap=AbsentPast
        //    |                              | Based from    |  A1  A2  B1  B2  C1
        // T0 |	Create A1 & B1	             | -             |  P   Af  P   Af  Af
        // T1 | Destroy A1, Create A2	     | T0            |  Ap  P   S   S   S
        // T2 |	Destroy B1, Create B2	     | T0            |  S   S   Ap  P   S
        // T3 |	Entangle A2 & B2, Create C1  | T1 & T2       |  Ap	P	Ap	P	P
        //
        //Note: We know T0 happens first, and T3 happens last, but there is no relative ordering between T1 & T2

        //When collapsed on T0 A1 & B1 should both exist, and A2 & B2 should be KnownAbsent
        //But then if we collapse T2, B2 eixists, B1 is absent, and A1 & A2 should be in superposition
        //Similarly, the reverse should occur if I collapse only T1

        //When we collapse both T1 and T2 simultaneously, A1 & B1 should become KnownAbsent and A2 & B2 become KnownPresent

        //Finally we add T3 that creates C1 entangling A2 and B2, and verify that they are both known present, A1 & B1 are known absent, and C1 is present

        //Add two co-created elements
        let t0_id = quantum_world_state.add_transaction(&[], &[], vec![
            Box::new(QWSElementWrapper::new(QWSElementType::GenericText, "A1")),
            Box::new(QWSElementWrapper::new(QWSElementType::GenericText, "B1")),
            ]).unwrap().id();
        let created_element_ids = quantum_world_state.get_transaction(t0_id).unwrap().created_elements();
        let (a1_id, b1_id) = (created_element_ids[0], created_element_ids[1]);
        
        //Add element A2 that destroys A1, and element B2 that destroys b1, in separate transactions.  B2 and A2 shouldn't be entangled with each other
        let &a2_id = quantum_world_state.add_transaction(&vec![a1_id], &[], vec![Box::new(QWSElementWrapper::new(QWSElementType::GenericText, "A2"))])
            .unwrap().created_elements().first().unwrap();
        let &b2_id = quantum_world_state.add_transaction(&vec![b1_id], &[], vec![Box::new(QWSElementWrapper::new(QWSElementType::GenericText, "B2"))])
            .unwrap().created_elements().first().unwrap();
        
        //Just a sanity check, collapse the first transaction and confirm the other elements are in the state we'd expect
        let collapsed_view = quantum_world_state.new_view().collapse(&vec![t0_id]).unwrap();
        assert_eq!(collapsed_view.get_element_status(a1_id), QWSElementStatus::KnownPresent);
        assert_eq!(collapsed_view.get_element_status(b1_id), QWSElementStatus::KnownPresent);
        assert_eq!(collapsed_view.get_element_status(a2_id), QWSElementStatus::KnownAbsent);
        assert_eq!(collapsed_view.get_element_status(b2_id), QWSElementStatus::KnownAbsent);

        //Partially collapse transaction T1, and confirm the other elements are in the state we'd expect
        let t1_id = quantum_world_state.get_creator_transaction(a2_id).unwrap().id();
        let mut collapsed_view = quantum_world_state.new_view().collapse(&vec![t1_id]).unwrap();
        assert_eq!(collapsed_view.get_element_status(a1_id), QWSElementStatus::KnownAbsent);
        assert_eq!(collapsed_view.get_element_status(a2_id), QWSElementStatus::KnownPresent);
        assert_eq!(collapsed_view.get_element_status(b1_id), QWSElementStatus::Superposition);
        assert_eq!(collapsed_view.get_element_status(b2_id), QWSElementStatus::Superposition);

        //Further collapse around B2, and confirm that we don't hit an error, and that everything is in the state we'd expect
        let t2_id = quantum_world_state.get_creator_transaction(b2_id).unwrap().id();
        collapsed_view = collapsed_view.collapse(&vec![t2_id]).unwrap();
        assert_eq!(collapsed_view.get_element_status(a1_id), QWSElementStatus::KnownAbsent);
        assert_eq!(collapsed_view.get_element_status(b1_id), QWSElementStatus::KnownAbsent);
        assert_eq!(collapsed_view.get_element_status(a2_id), QWSElementStatus::KnownPresent);
        assert_eq!(collapsed_view.get_element_status(b2_id), QWSElementStatus::KnownPresent);

        //Create T3, entangling A2 & B2, and confirm everything is the way it ought to be
        let t3_id = quantum_world_state.add_transaction(&vec![], &[a2_id, b2_id], vec![Box::new(QWSElementWrapper::new(QWSElementType::GenericText, "C1"))])
            .unwrap().id();
        let c1_id = quantum_world_state.get_transaction(t3_id).unwrap().created_elements()[0];
        let collapsed_view = quantum_world_state.new_view().collapse(&vec![t3_id]).unwrap();
        assert_eq!(collapsed_view.get_element_status(a1_id), QWSElementStatus::KnownAbsent);
        assert_eq!(collapsed_view.get_element_status(b1_id), QWSElementStatus::KnownAbsent);
        assert_eq!(collapsed_view.get_element_status(a2_id), QWSElementStatus::KnownPresent);
        assert_eq!(collapsed_view.get_element_status(b2_id), QWSElementStatus::KnownPresent);
        assert_eq!(collapsed_view.get_element_status(c1_id), QWSElementStatus::KnownPresent);
    }

    #[test]
    fn conflicting_chains() {
        let mut quantum_world_state = QuantumWorldState::new();

        //This chart shows what the status of each element should be internally at each transaction epoch
        // Af=AbsentFuture, P=Present, S=Superposition, Ap=AbsentPast
        //    |                              | Based from    |  A1  B1  C1  C2  D1
        // T0 |	Create A1    	             | -             |  P   Af  Af  Af  Af
        // T1 | Destroy A1, Create B1	     | T0            |  Ap  P   Ap  Af  Af
        // T2 |	Destroy A1, Create C1	     | T0            |  Ap  Ap  P   Af  Af
        // T3 |	Destroy C1, Create C2	     | T2            |  Ap  Ap  Ap  P   Af
        // T4 |	Destroy A1, Create D1	     | T0            |  Ap  Ap  Ap  Ap  P
        //

        //Add an element to get things started
        let t0_id = quantum_world_state.add_transaction(&[], &[], vec![
            Box::new(QWSElementWrapper::new(QWSElementType::GenericText, "A1")),
            ]).unwrap().id();
        let a1_id = quantum_world_state.get_transaction(t0_id).unwrap().created_elements()[0];
    
        //Now Add B1 that deletes A1
        let t1_id = quantum_world_state.add_transaction(&[a1_id], &[], vec![
            Box::new(QWSElementWrapper::new(QWSElementType::GenericText, "B1")),
            ]).unwrap().id();
        let b1_id = quantum_world_state.get_transaction(t1_id).unwrap().created_elements()[0];

        //Here's the twist, Add C1 that deletes A1 (Not B1).  This should mean that, in effect, B1 & C1 are mutually exclusive
        let t2_id = quantum_world_state.add_transaction(&[a1_id], &[], vec![
            Box::new(QWSElementWrapper::new(QWSElementType::GenericText, "C1")),
            ]).unwrap().id();
        let c1_id = quantum_world_state.get_transaction(t2_id).unwrap().created_elements()[0];
        
        //When we collapse around A1, we should see both B1 & C1 as absent because it's impossible to create either without
        //  destroying A1
        let collapsed_view = quantum_world_state.new_view().collapse(&vec![t0_id]).unwrap();
        assert_eq!(collapsed_view.get_element_status(a1_id), QWSElementStatus::KnownPresent);
        assert_eq!(collapsed_view.get_element_status(b1_id), QWSElementStatus::KnownAbsent);
        assert_eq!(collapsed_view.get_element_status(c1_id), QWSElementStatus::KnownAbsent);

        //Now when we collapse around B1, we should see that A1 is absent (we deleted it), but C1 is also absent because it's
        //  impossible to have created it.
        let collapsed_view = quantum_world_state.new_view().collapse(&vec![t1_id]).unwrap();
        assert_eq!(collapsed_view.get_element_status(a1_id), QWSElementStatus::KnownAbsent);
        assert_eq!(collapsed_view.get_element_status(b1_id), QWSElementStatus::KnownPresent);
        assert_eq!(collapsed_view.get_element_status(c1_id), QWSElementStatus::KnownAbsent);

        //Now test the reverse, collapse around C1, and confirm B1 is absent
        let collapsed_view = quantum_world_state.new_view().collapse(&vec![t2_id]).unwrap();
        assert_eq!(collapsed_view.get_element_status(a1_id), QWSElementStatus::KnownAbsent);
        assert_eq!(collapsed_view.get_element_status(b1_id), QWSElementStatus::KnownAbsent);
        assert_eq!(collapsed_view.get_element_status(c1_id), QWSElementStatus::KnownPresent);

        //Now Create C2, that deletes C1
        let t3_id = quantum_world_state.add_transaction(&[c1_id], &[], vec![
            Box::new(QWSElementWrapper::new(QWSElementType::GenericText, "C2")),
            ]).unwrap().id();
        let c2_id = quantum_world_state.get_transaction(t3_id).unwrap().created_elements()[0];
        
        //Collapse around C2, and confirm everything is the way we'd expect it
        let collapsed_view = quantum_world_state.new_view().collapse(&vec![t3_id]).unwrap();
        assert_eq!(collapsed_view.get_element_status(a1_id), QWSElementStatus::KnownAbsent);
        assert_eq!(collapsed_view.get_element_status(b1_id), QWSElementStatus::KnownAbsent);
        assert_eq!(collapsed_view.get_element_status(c1_id), QWSElementStatus::KnownAbsent);
        assert_eq!(collapsed_view.get_element_status(c2_id), QWSElementStatus::KnownPresent);

        //Now Create D1, forking again from T0 by deleting A1
        let t4_id = quantum_world_state.add_transaction(&[a1_id], &[], vec![
            Box::new(QWSElementWrapper::new(QWSElementType::GenericText, "D1")),
            ]).unwrap().id();
        let d1_id = quantum_world_state.get_transaction(t4_id).unwrap().created_elements()[0];

        //Collapse around D1, and confirm everything is the way we'd expect it
        let collapsed_view = quantum_world_state.new_view().collapse(&vec![t4_id]).unwrap();
        assert_eq!(collapsed_view.get_element_status(a1_id), QWSElementStatus::KnownAbsent);
        assert_eq!(collapsed_view.get_element_status(b1_id), QWSElementStatus::KnownAbsent);
        assert_eq!(collapsed_view.get_element_status(c1_id), QWSElementStatus::KnownAbsent);
        assert_eq!(collapsed_view.get_element_status(c2_id), QWSElementStatus::KnownAbsent);
        assert_eq!(collapsed_view.get_element_status(d1_id), QWSElementStatus::KnownPresent);

        //Collapse T0 again, we should see all other elements as absent
        let collapsed_view = quantum_world_state.new_view().collapse(&vec![t0_id]).unwrap();
        assert_eq!(collapsed_view.get_element_status(a1_id), QWSElementStatus::KnownPresent);
        assert_eq!(collapsed_view.get_element_status(b1_id), QWSElementStatus::KnownAbsent);
        assert_eq!(collapsed_view.get_element_status(c1_id), QWSElementStatus::KnownAbsent);
        assert_eq!(collapsed_view.get_element_status(c2_id), QWSElementStatus::KnownAbsent);
        assert_eq!(collapsed_view.get_element_status(d1_id), QWSElementStatus::KnownAbsent);

        //Now test that we cannot collapse around multiple incompatible transactions
        //In this particular test, all transactions are exclusive to all others
        assert!(quantum_world_state.new_view().collapse(&vec![t1_id, t2_id]).is_err(), "elements should not be allowed to coexist!");
        assert!(quantum_world_state.new_view().collapse(&vec![t0_id, t3_id]).is_err(), "elements should not be allowed to coexist!");
        assert!(quantum_world_state.new_view().collapse(&vec![t3_id, t4_id]).is_err(), "elements should not be allowed to coexist!");

        //Try the same thing through the "continued collapse" call
        let collapsed_view = quantum_world_state.new_view().collapse(&vec![t1_id]).unwrap();
        assert!(collapsed_view.collapse(&vec![t2_id]).is_err(), "elements should not be allowed to coexist!");
        let collapsed_view = quantum_world_state.new_view().collapse(&vec![t0_id]).unwrap();
        assert!(collapsed_view.collapse(&vec![t3_id]).is_err(), "elements should not be allowed to coexist!");
        let collapsed_view = quantum_world_state.new_view().collapse(&vec![t3_id]).unwrap();
        assert!(collapsed_view.collapse(&vec![t4_id]).is_err(), "elements should not be allowed to coexist!");

        //Finally, test that we also cannot create a transaction based on conflicting entangled elements
        assert!(quantum_world_state.add_transaction(&[a1_id], &[b1_id], vec![
            Box::new(QWSElementWrapper::new(QWSElementType::GenericText, "Bogus")),
            ]).is_err(), "transaction should be impossible to create!");
        assert!(quantum_world_state.add_transaction(&[], &[b1_id, c1_id], vec![
            Box::new(QWSElementWrapper::new(QWSElementType::GenericText, "Bogus")),
            ]).is_err(), "transaction should be impossible to create!");
        assert!(quantum_world_state.add_transaction(&[c2_id, d1_id], &[], vec![
            Box::new(QWSElementWrapper::new(QWSElementType::GenericText, "Bogus")),
            ]).is_err(), "transaction should be impossible to create!");
    }

    #[test]
    fn conflicting_transaction_exploration() {
        let mut quantum_world_state = QuantumWorldState::new();

        //This chart shows what the status of each element should be internally at each transaction epoch
        // Af=AbsentFuture, P=Present, S=Superposition, Ap=AbsentPast
        //    |                              | Based from    |  A1  A2  A3  B1  B2  B3
        // T0 |	Create A1 & B1               | -             |  P   Af  Af  P   Af  Af
        // T1 | Delete A1 & B1, Create A2    | T0            |  Ap  P   S   Ap  Ap  Ap
        // T2 | Delete A1 & B1, Create B2    | T0            |  Ap  Ap  Ap  Ap  P   S
        // T3 | Entangle A2, Create A3       | T1            |  Ap  P   P   Ap  Ap  Ap
        // T4 | Entangle B2, Create B3       | T2            |  Ap  Ap  Ap  Ap  P   P

        let t0_id = quantum_world_state.add_transaction(&[], &[], vec![
            Box::new(QWSElementWrapper::new(QWSElementType::GenericText, "A1")),
            Box::new(QWSElementWrapper::new(QWSElementType::GenericText, "B1")),
            ]).unwrap().id();
        let created_element_ids = quantum_world_state.get_transaction(t0_id).unwrap().created_elements();
        let (a1_id, b1_id) = (created_element_ids[0], created_element_ids[1]);
    
        let t1_id = quantum_world_state.add_transaction(&[a1_id, b1_id], &[], vec![
            Box::new(QWSElementWrapper::new(QWSElementType::GenericText, "A2")),
            ]).unwrap().id();
        let a2_id = quantum_world_state.get_transaction(t1_id).unwrap().created_elements()[0];

        let t2_id = quantum_world_state.add_transaction(&[a1_id, b1_id], &[], vec![
            Box::new(QWSElementWrapper::new(QWSElementType::GenericText, "B2")),
            ]).unwrap().id();
        let b2_id = quantum_world_state.get_transaction(t2_id).unwrap().created_elements()[0];

        let t3_id = quantum_world_state.add_transaction(&[], &[a2_id], vec![
            Box::new(QWSElementWrapper::new(QWSElementType::GenericText, "A3")),
            ]).unwrap().id();
        let a3_id = quantum_world_state.get_transaction(t3_id).unwrap().created_elements()[0];

        let t4_id = quantum_world_state.add_transaction(&[], &[b2_id], vec![
            Box::new(QWSElementWrapper::new(QWSElementType::GenericText, "B3")),
            ]).unwrap().id();
        let b3_id = quantum_world_state.get_transaction(t4_id).unwrap().created_elements()[0];

        //Try to fully collapse uncollapsed view, should fail.
        //BORIS, uncomment this when it works
        //assert!(quantum_world_state.new_view().fully_collapse().is_err(), "view contains conflicting states so shouldn't be fully collapsible!");

        //Get the conflicting transactions; there should be 5 results, all transactions.
        //This is because every transactions has a conflict with at least one other uncollapsed transaction
        let mut the_view = quantum_world_state.new_view();
        let conflicting_transactions = the_view.get_conflicting_transactions();
println!("BORIS !! {}", conflicting_transactions.len());

        //Partially Collapse T0
        the_view = the_view.collapse(&[t0_id]).unwrap();
        //Check the conflicting transactions, There should be zero results
        assert_eq!(the_view.get_conflicting_transactions().len(), 0);
//Try to fully collapse, should succeed because all other transactions are incompatible with T0 and have already been excluded BORIS

        //Partially Collapse T1
        //Try to fully collapse, should succeed
//Get an iterator for conflicting transactions, should be empty
//Iterate the elements, shoould give 2 results, A2 & A3

        //Partially Collapse T2
        let mut the_view = quantum_world_state.new_view().collapse(&[t2_id]).unwrap();
        assert_eq!(the_view.get_conflicting_transactions().len(), 0);
//Get an iterator for conflicting transactions, should be empty
//Iterate the elements, B2 should be Present, and B3 should be in Superposition

//Try to fully collapse, should succeed 
//Iterate the elements, should give 2 results, B2 & B3, both present



    }

}



//
// 3 kinds of operations on elements in the QuantumWorldState
//
//Unary operations can be done on the results of queries from the world in superposition.  A Unary operation just accesses one element
//  at a time and makes a determination of what to do next based on that element alone.  It doesn't matter what elements exist at what
//  time.  To implement a unary operation, just loop over your query results from a query against a superposition world.
//
//Binary operatons require a partially collapsed world.  A binary operation performs a determination and takes action depending on two
//  elements relative to each other.  So to implement a binary operation, you would start with one of the elements, collapse the world
//  around that element, run the query on that partially collapsed world, and then loop over those results and compare them to the
//  original element.
//
//N-ary operations require a fully collapsed world.  
//
//
//
//
//BEGIN BORIS
//
//What should the API for asking about uncollapsed transactions look like??????????????????
//Initially, I was thinking I could just iterate all of the superposition elements, and get their creation transaction,
//and provide an iterator that iterates over those transactions, keeping a HashSet, so that I could avoid providing the
//same transaction twice.
//
//Turns out that isn't very useful.  If a client started to iterate that list, they'd still have an uncollapsed view
//after collapsing around each transaction provided by the iterator.  If they used the same iterator to continue collapse,
//some plausible states would not be visited, and just as problematically, some transactions would preclude the collapse of
//some other transactions.  If the client wanted to visit every possible fully-collapsed end-state, they would need to
//recursively perform the "get iterator, collapse transaction" loop recusrively with a new iterator for each recursive call
//That is N FACTORIAL operations.  Not to mention the fact that the same state would be visited countless times because
//the order of collapse doesn't matter to the end state you arrive at, but the algorithm would visit the same end state
//through every possible combinitoric ordering of collapses that get you there.
//
//So... Take a step back... What does the client *Want* to do???  Get to fully collapsed states...  But which ones?
//
//There are two issues to think about.
// 1.) truly unrelated (not entangled anywhere) elements lead to a combinatoric explosion of end-states, as each possible
//state for one subset needs to be matched with each possible state for another subset.  Multiple subsets creates an
//intractible problem.
//
//So maybe we want to be able to iterate over these "subsets" somehow.  But I'm not sure exactly how to define them in
//the API (i.e. what the client would use to refer to them)
//
// 2.) Some transactions are logically grouped into "chains", where one deletes elements from another and onward.  Often
//the client might not care less about the mid-point of a chain than the end point.  So maybe we want to be able to follow
//a transaction forward to the number of "dependent terminal states".  i.e. each terminal transaction (transaction without
//dependents) that has a line of dependency on the specified transaction.  hmmmmmmm.  I'm not sure if this is desireable.
//
//Another perspective is that we're really interested in the conflicts.  i.e. The transactions that are exclusive of other
//transactions.  Some conflicts occur along the same chain, but the more interesting kind is when a chain forks.
//
//------------------------------------------------------------------------------------------------------------
//DECISION: We are going to provide two new APIs.  One API performs "fully_collapse" of a view, where every superposition
//  element becomes a KnownAbsent or KnownPresent.  If that isn't possible, then the call fails.
//The other API provides a "conflicting_transactions_iter"  Conflicting transactions will always come as two or more.
//  If there are no conflicting transactions, then the fully_collapse call will succeed, otherwise it will fail.
//
//To implement the conflicting_transactions list, we want to keep a set with every data_mask.
//  merge_for_base can potentially add to the conflicting transactions.  merge_for_collapse potentially reduces the number of
//  conflicting transactions
//
//
//IDEA: Also, it would be very handy (probably necessary in some cases) to support "OR" entanglements.  An OR entanglement
//  says that any of a set of elements would satisfy the conditions for a transaction.  Therefore, it offers the ability to
//  join two chains that were previously forked.  In plain English: "There are many ways to get to this state, but all that
//  matters is that you got here, not which path you took to do it".  The issue comes up if both (or multiple) of the different 
//  OR-entangled elements conflict with each other.
//
//Counter-Argument: Transactions aren't created in a vaccuum ahead of time.  They are created by a client in response to some
//  processing based on the current state of the world.  By definition, this feature is only needed for elements that DO
//  conflict with each other, because elements that don't conflict could be entangled together by the client at the time the
//  new transaction is created.  So the client chooses which state, among the conflicting states, to carry forward from, and
//  the result is a new element or set of elements representing the end result.  The fact that there were multiple paths that
//  could have lead here but the client chose this path is irrelevant.  In summary, If it matters which path the client took,
//  then that choice matters, and the paths should still evolve separately, and if it doesn't matter then it also doesn't
//  matter that there were multiple ways to get to a given place, only that we got there.
//
//In practical terms, consider using non-conflicting elements instead of conflicting elements.  For example, for the "document-
//  wide date format" problem, instead of replacing every date string element with a "unparsed date + format mask" element, and
//  then comparing all the formats and replaciong them with their parsed equivalents in one transaction, consider instead using
//  a separate transaction to generate an entanged "format mask hint" element from every unparsed date string, and then a
//  transaction that deletes all the hints replacing them with a "document-wide date format" element, which is then entangled
//  by separate transactions that parse each individual date by reading (and thus entangling) the document-wide date format.
//
//
//
//
//
//Paul Sutter (ex apple, founder of quantcast)
//Phil Stilwell (guy from coffee shop)
//
//END BORIS
//