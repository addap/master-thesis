Verification of the resume_all function in Eio is a bit of a problem for us.

## TODO

- [ ] understand how `resume_all` in Eio works
  - [ ] I think I wrote some comments explaining the function before, but it's on another branch.
- [ ] If we want to verify it, there are two crucial things:
  1. the CAS t.idx start stop. Normally t.idx is only increased by 1 but here we set it to some arbitrary higher value.
  2. The loop that applies a function to all cells between start and stop. If we would just loop it would be a lot easier but we do some very confusing index manipulations. Again, I think the comments in the custom branch explain them.
- [ ] Understand how the CQS Coq code corresponds to the Eio implementation. They often do similar things but differ in the exact datastructures and return types of functions.

### Which functions are roughly equivalent

| Eio                | CQS                    |
| ------------------ | ---------------------- |
| Position.next      | iteratorStep           |
| findAndMoveForward | findCellAndMoveForward |

#### How are they used

CQS:

- iteratorStep returns a "cellPtr", which is a pair of a segment and offset "ix".
- derefCellPtr "cellPtr", returns a reference to the content of the cell
  - getDataLoc takes the reference to the allocation of the segment's cells. They are allocated as a block, and offsets that reference with the offset "ix".

#### iteratorStep

```
Definition iteratorStep: val :=
  λ: "iterator",
  let: "counter" := Fst "iterator" in
  let: "ptr" := Snd "iterator" in
  let: "start" := cutoffGetPointer array_interface "ptr" in
  let: "idx" := FAA "counter" #1%nat in
  (* idx: the idx of the cell
     findCellAndMoveForward: is something like an abstract cell pointer.
        The cell's index can be computed from it via cellPointerId
        A cell can be cancelled by it.   *)
  ("idx", findCellAndMoveForward array_interface "ptr" "idx" "start").
```

#### iteratorStepOrMoveForward

```
Definition iteratorStepOrIncreaseCounter: val :=
  λ: "shouldAdjust" "iterator",
  let: "counter" := Fst "iterator" in
  let: "s" := iteratorStep "iterator" in
  (* TODO when is the cell idx not the same as the cellPointerId? Probably something to do with cancellation. *)
  if: cellPointerId array_interface (Snd "s") = (Fst "s") then SOME (Snd "s")
  else (if: "shouldAdjust"
        then increaseValueTo "counter" (cellPointerId array_interface (Snd "s"))
        else #()) ;; NONE.
```

#### derefCellPointer

```
array_interfaces.derefCellPointer :=
  λ: "ptr", let: "seg" := Fst "ptr" in
            let: "ix" := Snd "ptr" in
            getDataLoc "seg" +ₗ "ix";
```

#### cellPointerId

```
array_interfaces.cellPointerId :=
  λ: "ptr", let: "seg" := Fst "ptr" in
            let: "ix" := Snd "ptr" in
            getIdImpl "seg"
            * #(Pos.to_nat segment_size) + "ix";
```
