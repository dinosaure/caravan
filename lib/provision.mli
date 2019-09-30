type t

val length : t -> int
val unsafe_of_string : string -> t

val load_bytes : t -> src_off:int -> bytes -> dst_off:int -> len:int -> unit
val load_bigstring : t -> src_off:int -> Bigstringaf.t -> dst_off:int -> len:int -> unit
