type 'a t

val create : unit -> 'a t
val fulfill : 'a t -> 'a -> unit
val await : 'a t -> 'a