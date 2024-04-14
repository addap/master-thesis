open Effect
open Common

let fork_promise (f: unit -> 'a) : 'a Promise.t =
  let tlv = perform (GetContext ()) in
  let p = Promise.create () in 
  let fiber = fun () ->
    let result = f () in
    Promise.fulfill p result 
  in
  perform (Fork (fiber, tlv));
  p