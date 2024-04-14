open Printf
open Effect
open Lib
open Lib.Common

let yield () = 
  perform (Suspend (fun waker -> waker ()))

let work x () = 
  printf "work: start %d.\n%!" x;
  yield (); 
  x

let rec wait_for_data (tlv : tlv ref) =
  printf "Check tlv for data.\n%!";
  match !tlv with
  | None -> yield (); wait_for_data tlv
  | Some data -> tlv := None; data 

let dispatch () =
  printf "dispatch: start.\n%!";
  let tlv = perform (GetContext ()) in
  let data = wait_for_data tlv in 
  printf "dispatch: received data %d.\n%!" data;
  let p1 = Fiber.fork_promise (fun () -> Domain_manager.new_scheduler (work data)) in 
  let p2 = Fiber.fork_promise (fun () -> Domain_manager.new_scheduler (work data)) in
  printf "dispatch: spawned fibers.\n%!";
  let r1 = Promise.await p1 in
  let r2 = Promise.await p2 in
  printf "dispatch: awaited fibers.\n%!";
  r1 + r2

let main_fiber data () =
  printf "main_fiber: start.\n%!";
  let p = Fiber.fork_promise dispatch in 
  let tlv = perform (GetContext ()) in
  printf "main_fiber: send data.\n%!";
  tlv := Some data;
  Promise.await p

let main () = 
  printf "main: start.\n%!";
  let result = Scheduler.run None (main_fiber 21) in
  printf "main: finished with %d.\n%!" result

let _ = main ()