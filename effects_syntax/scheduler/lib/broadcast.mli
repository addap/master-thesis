type t
type callback = unit -> unit
type register_handle
type register_result = Invoked | Registered of register_handle

val create : unit -> t
val register : t -> callback -> register_result
val try_unregister : register_handle -> bool
val signal_all : t -> unit