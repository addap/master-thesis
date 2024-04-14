open Effect.Deep
open Common

let run init (main: unit -> 'a) : 'a =
  let result = ref None in
  let run_queue = Queue.create () in 
  let rec next () = 
    match Queue.pop run_queue with
    | None -> begin
        match !result with 
        | None -> next ()
        | Some _ -> ()
      end
    | Some fiber -> fiber () 
  in
  let rec execute fiber tlv = 
    match fiber () with
    | () -> next () 
    | effect (Fork (fiber, new_tlv)), k -> 
        Queue.push run_queue (fun () -> continue k ()); 
        execute fiber new_tlv
    | effect (Suspend register), k -> 
        let waker = fun v -> Queue.push run_queue (fun () -> continue k v) in 
        register waker; 
        next ()
    | effect (GetContext ()), k ->
        continue k tlv
  in
  let tlv = ref init in
  execute (fun () -> result := Some (main ())) tlv; 
  match !result with 
  | None -> error "impossible" 
  | Some result -> result