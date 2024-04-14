module Q = Stdlib.Queue

type 'a t = {
  queue : 'a Q.t;
  mutex: Mutex.t;
}

let create () = 
  { queue = Q.create ();
    mutex = Mutex.create () }

let push (q : 'a t) e =
  Mutex.lock q.mutex;
  Q.add e q.queue;
  Mutex.unlock q.mutex

let pop (q : 'a t) = 
  Mutex.lock q.mutex;
  let result = Q.take_opt q.queue in
  Mutex.unlock q.mutex;
  result