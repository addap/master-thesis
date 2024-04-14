type callback = unit -> unit
type t = {
  mutable cbs : (int * callback) list;
  mutable idx : int;
  mutex : Mutex.t;
}
type register_handle = t * int
type register_result = Invoked | Registered of register_handle

let create () = 
  { cbs = [];
    idx = 0;
    mutex = Mutex.create () }

let register bcst cb = 
  Mutex.lock bcst.mutex;
  let idx = bcst.idx in
  bcst.idx <- idx + 1;
  bcst.cbs <- (idx, cb) :: bcst.cbs;
  Mutex.unlock bcst.mutex;
  Registered (bcst, idx)

let try_unregister (bcst, idx) = 
  Mutex.lock bcst.mutex;
  let cbs = bcst.cbs in
  let contained = List.mem_assq idx cbs in
  if contained then
    let new_cbs = List.remove_assq idx cbs in
    bcst.cbs <- new_cbs;
  else ();
  Mutex.unlock bcst.mutex;
  contained

let signal_all bcst = 
  let rec loop () =
    Mutex.lock bcst.mutex;
    match bcst.cbs with
    | [] -> Mutex.unlock bcst.mutex
    | (_, cb) :: cbs -> 
        bcst.cbs <- cbs;
        Mutex.unlock bcst.mutex;
        cb ();
        loop ()
  in
  loop ()    
  

  