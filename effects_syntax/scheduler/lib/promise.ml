open Effect
open Common

type 'a state = Done of 'a | Waiting of Broadcast.t
type 'a t = 'a state Atomic.t

let create () : 'a t = 
  let bcst = Broadcast.create () in
  Atomic.make (Waiting bcst)

let fulfill (p: 'a t) (result: 'a) = 
  match Atomic.get p with
  | Done _ -> error "impossible" 
  | Waiting bcst ->
      Atomic.set p (Done result);
      Broadcast.signal_all bcst 

let make_register (p: 'a t) (bcst: Broadcast.t) : (unit waker -> unit) =
  fun waker ->
    let register_result = Broadcast.register bcst waker in 
    match register_result with
    | Invoked -> ()
    | Registered register_handle ->
      match Atomic.get p with 
      | Done result ->  
          if Broadcast.try_unregister register_handle
          then waker () 
          else ()
      | Waiting _ -> ()

let await (p: 'a t) : 'a =
  match Atomic.get p with 
  | Done result -> result
  | Waiting bcst -> begin
      let register = make_register p bcst in
      perform (Suspend register); 
      match Atomic.get p with 
      | Done result -> result 
      | Waiting _ -> error "impossible" 
    end
