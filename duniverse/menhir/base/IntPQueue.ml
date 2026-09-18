(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

module Plain = struct
let fail format =
  Printf.ksprintf invalid_arg format

type priority =
  int

module MyArray = Vector
module MyStack = Vector

(* We use vectors to represent both the main array and the secondary
   stacks. These two uses are in principle independent of one another. *)

(* Hector's vectors have a memory leak: after an element has been extracted by
   [pop], the vector can still contain a pointer to this element. Thus, our
   priority queue, too, has a memory leak. In practice, we do not expect this
   to create a problem. The use of [MyStack.reset] in [extract] partially
   mitigates this problem. *)

(* -------------------------------------------------------------------------- *)

(**A priority queue. *)
type 'a t = {

  (* A priority queue is represented as a vector, indexed by priorities, of
     stacks. There is no bound on the size of the main vector -- its size is
     increased if needed. It is up to the user to use priorities of reasonable
     magnitude. *)
  mutable a: 'a MyStack.t MyArray.t;

  (* The index [best] is comprised between 0 (included) and the length of the
     array [a] (excluded). It can be the index of the lowest nonempty stack,
     if there is one; or it can be lower. In other words, from the index 0
     to the index [best] (excluded), every stack is empty. *)
  mutable best: int;

  (* Current number of elements in the queue. Used in [extract] to stop the
     search for a nonempty bucket. *)
  mutable cardinal: int;

}

(* -------------------------------------------------------------------------- *)

(* Checking well-formedness. (Debugging only.) *)

let check q =
  assert (0 <= q.best && q.best <= MyArray.length q.a);
  for i = 0 to q.best - 1 do
    let xs = MyArray.get q.a i in
    assert (MyStack.length xs = 0);
  done;
  let c = ref 0 in
  for i = q.best to MyArray.length q.a - 1 do
    let xs = MyArray.get q.a i in
    c := !c + MyStack.length xs
  done;
  assert (q.cardinal = !c)

(* -------------------------------------------------------------------------- *)

(* Operations on queues. *)

(* When the main array is created or extended, each level must be initialized
   with a fresh empty stack. [fresh_segment] creates an array of [n] fresh
   empty stacks. *)

let fresh_stack (_j : int) =
  MyStack.create()

let create () =
  let a = MyArray.init 16 fresh_stack in
  { a; best = 0; cardinal = 0 }

let reset q =
  q.a <- MyArray.init 16 fresh_stack;
  q.best <- 0;
  q.cardinal <- 0

let[@inline] grow q i =
  assert (0 <= i);
  let desired = i + 1 in
  let current = MyArray.length q.a in
  if current < desired then begin
    MyArray.ensure_capacity q.a desired;
    MyArray.push_array q.a (Array.init (desired - current) fresh_stack);
  end

let add q x i =
  if i < 0 then fail "add: negative priority (%d)" i;
  q.cardinal <- q.cardinal + 1;
  (* Grow the main array if necessary. *)
  grow q i;
  assert (i < MyArray.length q.a);
  (* Find out which stack we should push into. *)
  let xs = MyArray.unsafe_get q.a i in
  (* Push. *)
  MyStack.push xs x;
  (* Decrease [q.best], if necessary, so as not to miss the new element. In
     the special case of Dijkstra's algorithm or A*, this never happens. *)
  if i < q.best then
    q.best <- i

let[@inline] is_empty q =
  q.cardinal = 0

let[@inline] cardinal q =
  q.cardinal

let rec extract_nonempty q =
  assert (0 < q.cardinal);
  let i = q.best in
  assert (0 <= i && i < MyArray.length q.a);
  (* Look for the next nonempty bucket. We know there is one. This may seem
     inefficient, because it is a linear search. However, in applications
     where [q.best] never decreases, the cumulated cost of this loop is the
     maximum priority ever used, which is good. *)
  let xs = MyArray.unsafe_get q.a i in
  if MyStack.length xs = 0 then begin
    (* As noted below, [MyStack.pop] does not physically shrink the stack.
       When we find that a priority level has become empty, we physically
       empty it, so as to free the (possibly large) space that it takes up.
       This strategy is good when the client is Dijkstra's algorithm or A*. *)
    MyStack.reset xs;
    q.best <- i + 1;
    extract_nonempty q
  end
  else begin
    q.cardinal <- q.cardinal - 1;
    MyStack.pop xs
    (* Note: [MyStack.pop] does not shrink the physical array underlying the
       stack. This is good, because we are likely to push new elements into
       this stack. *)
  end

let[@inline] extract q =
  if q.cardinal = 0 then
    None
  else
    Some (extract_nonempty q)

(* [extract'] is a copy of [extract] where we return a pair [x, i].
   One could define [extract] in terms of [extract'], removing this
   duplication, but this would cause an extra pair allocation. *)

let rec extract'_nonempty q =
  assert (0 < q.cardinal);
  let i = q.best in
  assert (0 <= i && i < MyArray.length q.a);
  let xs = MyArray.unsafe_get q.a i in
  if MyStack.length xs = 0 then begin
    MyStack.reset xs;
    q.best <- i + 1;
    extract'_nonempty q
  end
  else begin
    q.cardinal <- q.cardinal - 1;
    let x = MyStack.pop xs in
    x, i
  end

let[@inline] extract' q =
  if q.cardinal = 0 then
    None
  else
    Some (extract'_nonempty q)

let repeat q yield =
  while q.cardinal > 0 do
    let x = extract_nonempty q in
    yield x
  done

let iter q yield =
  (* It would be preferable to use [MyArray.iter_segment] here. *)
  for i = q.best to MyArray.length q.a - 1 do
    let xs = MyArray.get q.a i in
    MyStack.iter yield xs
  done
end
module Boxed = struct
(* This is a variant of IntPQueue, which supports changing the priority of
   an element while it is in the queue, testing whether an element is in
   the queue, and removing an element from the queue. To this end, instead
   of storing raw elements, the queue stores boxes, which keep track of
   their own position in the queue. This makes it possible to extract a
   specific box out of the queue in constant time. *)

let fail format =
  Printf.ksprintf invalid_arg format

type priority =
  int

module MyArray = Vector
module MyStack = Vector

(* We use vectors to represent both the main array and the secondary
   stacks. These two uses are in principle independent of one another. *)

(* Hector's vectors have a memory leak: after an element has been extracted by
   [pop], the vector can still contain a pointer to this element. Thus, our
   priority queue, too, has a memory leak. In practice, we do not expect this
   to create a problem. The use of [MyStack.reset] in [extract] partially
   mitigates this problem. *)

(* -------------------------------------------------------------------------- *)

(** A box holds a value (its payload), a priority, and a position.

    The [payload] field is immutable.

    A box may or may not be currently part of a queue. The sign bit of the
    [priority] field records this information.

    If a box is currently part of a queue, then its [priority] field contains
    the value [i], where [i] is the box's priority, a nonnegative value. This
    value can be viewed as an index into the queue's main array. The box's
    [position] field holds its position within the stack found at index
    [priority].

    If a box is not currently part of a queue, then its [priority] field
    contains the value [set i], where [i] is the box's priority. This is
    the value [i] on top of which the sign bit has been set. *)
type 'a box = {
  payload: 'a;
  mutable priority: int;
  mutable position: int;
}

(**A priority queue.*)
and 'a t = {

  (* A priority queue is represented as a vector, indexed by priorities, of
     stacks. There is no bound on the size of the main vector -- its size is
     increased if needed. It is up to the user to use priorities of reasonable
     magnitude. *)
  mutable a: 'a box MyStack.t MyArray.t;

  (* The index [best] is comprised between 0 (included) and the length of the
     array [a] (excluded). It can be the index of the lowest nonempty stack,
     if there is one; or it can be lower. In other words, from the index 0
     to the index [best] (excluded), every stack is empty. *)
  mutable best: int;

  (* Current number of elements in the queue. Used in [extract] to stop the
     search for a nonempty bucket. *)
  mutable cardinal: int;

}

(* -------------------------------------------------------------------------- *)

(* Checking well-formedness. (Debugging only.) *)

(* [check q] checks that the queue [q] is well-formed. *)

let check q =
  assert (0 <= q.best && q.best <= MyArray.length q.a);
  for i = 0 to q.best - 1 do
    let xs = MyArray.get q.a i in
    assert (MyStack.length xs = 0);
  done;
  let c = ref 0 in
  for i = q.best to MyArray.length q.a - 1 do
    let xs = MyArray.get q.a i in
    c := !c + MyStack.length xs;
    xs |> MyStack.iteri @@ fun j box ->
      assert (box.priority = i);
      assert (box.position = j)
  done;
  assert (q.cardinal = !c)

(* -------------------------------------------------------------------------- *)

(* Operations on the sign bit. *)

(* [set i] sets the signs bit in [i]. *)

let[@inline] set i =
  i lor min_int

(* [unset i] clears the signs bit in [i]. *)

let[@inline] unset i =
  i land max_int

(* -------------------------------------------------------------------------- *)

(* Operations on boxes. *)

let box x =
  let payload = x
  and priority = set 0 (* box is not a member of any queue *)
  and position = 0     (* dummy *) in
  { payload; priority; position }

let[@inline] payload box =
  box.payload

let[@inline] priority box =
  unset box.priority

let[@inline] busy box =
  0 <= box.priority

let mem q box =
  (* Validate the box's [priority] field. *)
  let i = box.priority in
  0 <= i && i < MyArray.length q.a &&
  let xs = MyArray.unsafe_get q.a i in
  (* Validate the box's [position] field. *)
  let j = box.position in
  assert (0 <= j);
  j < MyStack.length xs &&
  (* Check that this box is found in the queue at the predicted position. *)
  box == MyStack.unsafe_get xs j

(* -------------------------------------------------------------------------- *)

(* Operations on queues. *)

(* When the main array is created or extended, each level must be initialized
   with a fresh empty stack. [fresh_segment] creates an array of [n] fresh
   empty stacks. *)

let fresh_stack (_j : int) =
  MyStack.create()

let create () =
  let a = MyArray.init 16 fresh_stack in
  { a; best = 0; cardinal = 0 }

(* It would be nice if we could implement [reset] in constant time, exactly as
   in [Plain]. However, in constant time, one cannot mark every box in the
   queue as suddenly not busy. As a result, we would then be unable to
   implement the function [busy]. That would be problematic, as [busy] is used
   in [add] to detect a user error. Therefore, we choose to implement [reset]
   in time O(n), where [n] is the number of boxes in the queue. *)

(* One way of implementing [reset] in constant time, while keeping [busy],
   would be to add two fields to each box, namely a pointer to a queue and
   an epoch number. We deem that too costly. *)

let reset q =
  (
    q.a |> MyArray.iteri @@ fun i xs ->
    xs |> MyStack.iter @@ fun box ->
    assert (box.priority = i);
    box.priority <- set i
  );
  q.a <- MyArray.init 16 fresh_stack;
  q.best <- 0;
  q.cardinal <- 0

let[@inline] grow q i =
  assert (0 <= i);
  let desired = i + 1 in
  let current = MyArray.length q.a in
  if current < desired then begin
    MyArray.ensure_capacity q.a desired;
    MyArray.push_array q.a (Array.init (desired - current) fresh_stack);
  end

(* [add' q box i] assumes [0 <= i] and does not increment [q.cardinal]. *)

let add' q box i =
  assert (0 <= i);
  (* Grow the main array if necessary. *)
  grow q i;
  assert (i < MyArray.length q.a);
  (* Find out which stack we should push into. *)
  let xs = MyArray.unsafe_get q.a i in
  (* Push. *)
  let j = MyStack.length xs in
  MyStack.push xs box;
  box.priority <- i;
  box.position <- j;
  (* Decrease [q.best], if necessary, so as not to miss the new element. In
     the special case of Dijkstra's algorithm or A*, this never happens. *)
  if i < q.best then
    q.best <- i

let add q box i =
  if busy box then
    fail "add: this box is already a member of some queue";
  if i < 0 then
    fail "add: negative priority (%d)" i;
  (* Increment the queue's cardinality. *)
  q.cardinal <- q.cardinal + 1;
  (* Continue. *)
  add' q box i

let[@inline] is_empty q =
  q.cardinal = 0

let[@inline] cardinal q =
  q.cardinal

let rec extract_nonempty q =
  assert (0 < q.cardinal);
  let i = q.best in
  assert (0 <= i && i < MyArray.length q.a);
  (* Look for the next nonempty bucket. We know there is one. This may seem
     inefficient, because it is a linear search. However, in applications
     where [q.best] never decreases, the cumulated cost of this loop is the
     maximum priority ever used, which is good. *)
  let xs = MyArray.unsafe_get q.a i in
  if MyStack.length xs = 0 then begin
    (* As noted below, [MyStack.pop] does not physically shrink the stack.
       When we find that a priority level has become empty, we physically
       empty it, so as to free the (possibly large) space that it takes up.
       This strategy is good when the client is Dijkstra's algorithm or A*. *)
    MyStack.reset xs;
    q.best <- i + 1;
    extract_nonempty q
  end
  else begin
    q.cardinal <- q.cardinal - 1;
    let box = MyStack.pop xs in
    (* Note: [MyStack.pop] does not shrink the physical array underlying the
       stack. This is good, because we are likely to push new elements into
       this stack. *)
    assert (box.priority = i);
    assert (box.position = MyStack.length xs);
    (* Mark this box as isolated and return it. *)
    box.priority <- set i;
    box
  end

let[@inline] extract q =
  if q.cardinal = 0 then
    None
  else
    Some (extract_nonempty q)

let repeat q yield =
  while q.cardinal > 0 do
    let x = extract_nonempty q in
    yield x
  done

let iter q yield =
  (* It would be preferable to use [MyArray.iter_segment] here. *)
  for i = q.best to MyArray.length q.a - 1 do
    let xs = MyArray.get q.a i in
    MyStack.iter yield xs
  done

(* [remove' q box fail] does not update [q.cardinal] and does not mark the
   box as isolated (that is, it does not update [box.priority]). *)

(* The failure action [fail] determines what to do if we find that [box] is
   not a member of [q]. It is permitted for [fail] to terminate normally; it
   need not raise an exception. *)

let remove' q box fail =
  (* The following checks resemble [mem q box]. However, we cannot use [mem q
     box] because we wish to bind [i], [xs], [j], [n] for use in the remainder
     of the code. *)
  let i = box.priority in
  if not (0 <= i && i < MyArray.length q.a) then fail() else
  let xs = MyArray.unsafe_get q.a i in
  let j = box.position in
  assert (0 <= j);
  let n = MyStack.length xs in
  if not (j < n) then fail() else
  let box' = MyStack.unsafe_get xs j in
  if not (box == box') then fail() else
  (* We have now verified that this box is a member of this queue. *)
  let box' = MyStack.pop xs in
  if j + 1 < n then begin
    (* We have extracted some other box, which we write at position [j]. *)
    MyStack.unsafe_set xs j box';
    box'.position <- j
  end

let fail_in_remove () =
  fail "remove: this box is not a member of this queue"

let remove q box =
  (* Remove this box (or fail). *)
  remove' q box fail_in_remove;
  (* Update the queue's cardinality. *)
  assert (0 < q.cardinal);
  q.cardinal <- q.cardinal - 1;
  (* Mark this box isolated. *)
  box.priority <- set box.priority

(* [update q box i'] is equivalent to the sequence [remove q box; add q box i'].
   By composing the two operations, we are able to avoid a few memory writes.
   To begin with, if this box's priority is already [i], then there is nothing
   to do. (In this case, for efficiency, we do not verify that [mem q box]
   holds.) If the two priorities differ, then, by using [remove'] and [add']
   instead of [remove] and [add], we save a few accesses to [q.cardinal] and
   [box.priority]. *)

let update q box i' =
  if i' < 0 then
    fail "update: negative priority (%d)" i';
  (* If the current priority and the requested priority match, do nothing.
     Indeed, this implies that [box] is a member of some queue; we assume
     that this must be the queue [q]. *)
  if box.priority <> i' then begin
    remove' q box fail_in_remove;
    add' q box i'
  end

(* [increment box q ()] assumes that [box] is not a member of [q]. (We have
   just determined this.) If [box] is busy, therefore a member of some other
   queue, then it fails. Otherwise, it increments [q.cardinal], in preparation
   for inserting [box] into [q]. *)

let increment box q () =
  if busy box then
    fail "add_or_update: this box is already a member of some queue";
  q.cardinal <- q.cardinal + 1

(* [add_or_update q box i'] is analogous to [update q box i'], except the box
   is expected to be either in the queue [q] or isolated. (It must not be a
   member of some other queue.) *)

let add_or_update q box i' =
  if i' < 0 then
    fail "add_or_update: negative priority (%d)" i';
  (* If the current priority and the requested priority match, do nothing.
     Indeed, this implies that [box] is a member of some queue; we assume
     that this must be the queue [q]. *)
  if box.priority <> i' then begin
    (* Attempt removing this box out of [q]. If we find that it is not in [q],
       then check that it is not busy, and increment [q.cardinal]. *)
    remove' q box (increment box q);
    (* At this point, either the box was in the queue and has been removed,
       without decrementing [q.cardinal]; or the box was not in the queue
       and [q.cardinal] has been incremented. *)
    (* In either case, insert [box] into the queue with priority [i']. *)
    add' q box i'
  end
end
