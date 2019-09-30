external string_get_int64 : string -> int -> int64 = "%caml_string_get64"
external swap64 : int64 -> int64 = "%bswap_int64"

let string_get_int64_be =
  if Sys.big_endian
  then fun buf off -> string_get_int64 buf off
  else fun buf off -> swap64 (string_get_int64 buf off)

external load : int64 -> bytes -> int -> int -> unit
  = "caml_load_vaddr"

type t = string

let length t = string_get_int64_be t 18 |> Int64.to_int

let load t ~src_off buf ~dst_off ~len =
  let src_len = string_get_int64_be t 18 |> Int64.to_int in
  let dst_len = Bytes.length buf in

  if len < 0 then Fmt.invalid_arg "len must be positive (%d)" len ;
  if src_off < 0 || src_len - src_off < len
  then Fmt.invalid_arg "Invalid source bounds (src_off: %d, src_len: %d)" src_off src_len ;
  if dst_off < 0 || dst_len - dst_off < len
  then Fmt.invalid_arg "Invalid destination bounds (dst_off: %d, dst_len: %d, len: %d)" dst_off dst_len len ;

  let offset = Int64.add (string_get_int64_be t 10) (Int64.of_int src_off) in
  load offset buf dst_off len
