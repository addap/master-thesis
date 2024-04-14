open Effect
open Common

let thread_fork = Domain.spawn
let thread_join = Domain.join

let new_scheduler (f: unit -> 'a) : 'a = 
  let handle = ref None in 
  let register = (fun waker -> 
    let thread_fun = (fun () ->
      let result = Scheduler.run None f in 
      waker (); 
      result
    ) in
    let join_handle = thread_fork thread_fun in
    handle := Some join_handle 
  ) in
  perform (Suspend register); 
  match !handle with  
  | None -> error "impossible" 
  | Some join_handle -> thread_join join_handle 