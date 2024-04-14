type 'a waker = 'a -> unit
type fiber = unit -> unit
type tlv = int option

type _ eff += Fork : (fiber * tlv ref) -> unit eff
type _ eff += Suspend : ('a waker -> unit) -> 'a eff
type _ eff += GetContext : unit -> tlv ref eff

let error = failwith