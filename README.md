# quantum_world_state

## Quantum World State

This library implements an in-ram database with relationships between elements inspired by quantum superposition and entanglement.

The QuantumWorldState is useful to represent the state of a system as it evolves into the future.  It allows multiple exclusive "forks" of evolution
to be explored without committing to a single one.  It is useful for planning an optimal sequence of operations.
The word "Quantum" in the QuantumWorldState doesn't refer to anything being quantized, but rather it refers to some metaphores borrowed
from quantum mechanics, namely we borrowed the ideas of superposition, collapse, and entanglement. Both the [Many Worlds Interpretation](https://en.wikipedia.org/wiki/Many-worlds_interpretation)
and the [Copenhagen Interpretation](https://en.wikipedia.org/wiki/Copenhagen_interpretation) provide useful insights, but they are only metaphores and thus have limits.

## Overview
The two fundamantal concepts in the QuantumWorldState are Elements and Transactions.

Each element can be thought of as a database record.  An element can be any Rust type that implements the [QWSElement](QWSElement) trait, but ownership of the object must be given to the QWS.
Each element is assigned a unique [QWSElementID](QWSElementID) that can always be used to locate that element in the world.
The QWS is an *append-only* data store, so you cannot modify an element after it has been added to the world, but a
transaction may delete a given element and replace it with a newer version.  Each version of the element will
have its own unique QWSElementID.

Elements may also supply meta-data values, that can be used to locate the elements through queries.  Currently the query semantics are limited
but they may be extended in the future.

The append-only nature means that the history of the data structure is preserved immutably.  It is possible to destroy an element with
a transaction, and that effectively removes the element from a branch representing one possible reality, but an alternate history in
which that element continues to exist is also possible and can't be removed.

Transactions create and destroy elements.  The process of creating a transaction involves providing 3 sets of elements:
* The elements created by the transaction (i.e. the elements added to the world)
* The elements destroyed by the transaction
* The elements entangled by the transaction

Entanglement occurs when an element is read in order to influence at least one of the elements being created.  Conceptually you can think of entanglement
as expressing a dependency.  In other words, if the entangled element weren't present, it would have been impossible to create at least one of the elements, so therefore
the created elements can't exist in the versions of the world where the entangled elements don't also exist.

The state of the world at any given instant is referred to an Epoch.  In a given epoch, each element will be in one of the states of existance
defined in the [QWSElementStatus](QWSElementStatus) enum.  A transaction exists at an epoch.

## Adding Some Elements and Transactions

```rust
#[macro_use] extern crate maplit;
use quantum_world_state::*;
let mut qws = QuantumWorldState::new();

// Create an object that will become our first element
// MetaData keys without values function like tags
let forty_two_element = QWSElementWrapper::new_with_metadata(hashmap!{md_str!("my_tag") => vec![]}, 42);

// Add it to the QWS with a new transaction, and get back the new ElementID
let forty_two_id = qws.add_transaction(&[], &[], vec![Box::new(forty_two_element)])
    .unwrap().created_elements()[0];

// Create another transaction that destroys 42 and adds 43
qws.add_transaction(&[forty_two_id], &[], vec![Box::new(QWSElementWrapper::new_with_metadata(hashmap!{md_str!("my_tag") => vec![]}, 43))]);
```

## Queries and Views

The QuantumWorldState can be queried to find elements matching a query expression.  For now, the only query expression implemented is
"key contains value", although conceptually this could be extended to allow other more expressive queries, including compound queries
e.g. with join operations, etc.

Queries are issued through the [QWSDataView](QWSDataView) object.  Effectively, a view object is a perspective from which elements are
visible, and a query may limit the visible elements to the subset that match the query.

```rust
// Get a view of the queried elements
let query_view = qws.new_view().query_contains_key(&md_str!("my_tag")).unwrap();

// Iterate over the query results
for element_id in query_view.elements_iter() {
    // Prints "element 42" and "element 43" when run with the QWS from above
    println!("element {}", qws.get_element(element_id).unwrap().get_payload::<i32>().unwrap());
}
```

## Superposition and Collapse

[QWSDataView](QWSDataView)s can also be collapsed to restrict which elements are found by the query.  In our example from above, element 42 and element 43
are mutually exclusive because element 43 was created in a transaction that destroyed element 42.  Our query above returned both elements.
However the elements have a status of [Superposition](QWSElementStatus::Superposition), indicating that they may or may not exist.

```rust
// We see that element 42 is in Superposition, in the query view we created above
println!("status of 42 = {}", query_view.get_element_status(forty_two_id));
```

In order to be sure an element exists, the view must be collapsed with respect to that element.  We do that with one of the `collapse_` methods of the [QWSDataView](QWSDataView).

```rust
// Get the transaction id for the transaction that created element 42
let transaction_id = qws.get_creator_transaction(forty_two_id).unwrap().id();

// Collapse the view further around that transaction
let query_view = query_view.collapse_transactions(&[transaction_id]).unwrap();

// Now we see that element 42 is in KnownPresent, from the perspective of the view
println!("status of 42 = {}", query_view.get_element_status(forty_two_id));

// And we only see "element 42" when we iterate over the query results
for element_id in query_view.elements_iter() {
    println!("element {}", qws.get_element(element_id).unwrap().get_payload::<i32>().unwrap());
}
```

## Conceptual Example

Say I am storing "contents_of_my_backpack" in a QuantumWorldState.  At the start, I have
an [Apple, a BaseballGameTicket and a FiveDollarBill], which are all *Present*, i.e. existant, at the epoch being queried.

Then I conduct a transaction where I spend the FiveDollarBill at a convenience store, and
buy a Sandwich.  At the epoch after that transaction, the query results for everything contained by the contents_of_my_backpack would be:
[Apple, BaseballGameTicket, Sandwich], but at an earlier epoch, the results would be [Apple, BaseballGameTicket, FiveDollarBill].

There is no point in time that the FiveDollarBill and the Sandwich existed together in my backpack.
It can be said that the FiveDollarBill and the Sandwich are entangled with each other.  The different result
sets represent two different states of the world, as it existed at two different Epoches.

If I issue the query without collapsing the epoch, I will get the results: [Apple-P, BaseballGameTicket-P, FiveDollarBill-S, Sandwich-S],
where 'P' denotes that an element is [KnownPresent](QWSElementStatus::KnownPresent) and 'S' denotes that the element is in [Superposition](QWSElementStatus::Superposition).

Now let's consider an alternate reality in which I was less responsible or more thirsty.  Say I chose to spend the FiveDollarBill on
a SixPackOfBeer instead. (It's really cheap beer.)  Now there are 3 possible result sets that could be returned in 3 different Epoches:
1:[Apple, BaseballGameTicket, FiveDollarBill], 2:[Apple, BaseballGameTicket, Sandwich], 3:[Apple, BaseballGameTicket, SixPackOfBeer].

But I can collapse the view such that I only see results in which I have a SixPackOfBeer, thus reducing my possible result sets to only:
[Apple, BaseballGameTicket, SixPackOfBeer].  This is referred to a partially collapsed view, because only results that
are compatible with SixPackOfBeer are included.  However, this it is not a fully collapsed view because no entanglement
exists between SixPackOfBeer and the Apple and BaseballGameTicket.

### Full vs. Partial Collapae ##

Continuing the example, If I traded the BaseballGameTicket for a ZooTicket, I would still be free to decide whether to buy
the SixPackOfBeer or the Sandwich.  In that situation, I can still collapse the view around the SixPackOfBeer and thus the set of possible fully collapsed world states would be:
1:[Apple, BaseballGameTicket, SixPackOfBeer], 2:[Apple, ZooTicket, SixPackOfBeer].

But what if I bought a BallParkHotDog at the stadium with my FiveDollarBill, instead of buying a Sandwich or a SixPackOfBeer at the
convenience store?  In that case, the BallParkHotDog is entangled, not only with FiveDollarBill, but also with BaseballGameTicket.
So then, if I want to query for the "contents_of_my_backpack" in situations where I have the BallParkHotDog, the only possible results
are: [Apple, BaseballGameTicket, BallParkHotDog].

The entanglement between BaseballGameTicket and BallParkHotDog is an asymetric entanglement.  That is, the existance of BallParkHotDog
depends on BaseballGameTicket up to the point that BallParkHotDog is created, but nothing related to the BallParkHotDog has any
effect on BaseballGameTicket.  Asymetric entanglement is created when one element is non-destructively used as input in the process
of creating another element.  This differs from real quantum physics, in which every interaction entangles all particles involved in
that interaction, and nothing can influence something else without being influenced itself.

If the quantum physics metaphor is a little alien, luckily we software people are already acquainted with these concepts through
distributed source control systems such as git.  We can think of a git repository as a QuantumWorldState of the state of all of
the files in the source tree, and a single checkout as being a collapsed WorldState as it exists for one observer.

