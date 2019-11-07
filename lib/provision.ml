external string_get_int64 : string -> int -> int64 = "%caml_string_get64"
external swap64 : int64 -> int64 = "%bswap_int64"

let string_get_int64_be =
  if Sys.big_endian
  then fun buf off -> string_get_int64 buf off
  else fun buf off -> swap64 (string_get_int64 buf off)

external load_bytes : int64 -> bytes -> int -> int -> unit
  = "caml_load_vaddr_into_bytes" [@@noalloc]

external load_bigstring : int64 -> Bigstringaf.t -> int -> int -> unit
  = "caml_load_vaddr_into_bigstring" [@@noalloc]

external map_bigstring : int64 -> int -> Bigstringaf.t = "caml_map_vaddr"

type t = string

external unsafe_of_string : string -> t = "%identity"

let length t = string_get_int64_be t 18 |> Int64.to_int

let load_bytes t ~src_off buf ~dst_off ~len =
  let src_len = string_get_int64_be t 18 |> Int64.to_int in
  let dst_len = Bytes.length buf in

  if len < 0 then Fmt.invalid_arg "len must be positive (%d)" len ;
  if src_off < 0 || src_len - src_off < len
  then Fmt.invalid_arg "Invalid source bounds (src_off: %d, src_len: %d)" src_off src_len ;
  if dst_off < 0 || dst_len - dst_off < len
  then Fmt.invalid_arg "Invalid destination bounds (dst_off: %d, dst_len: %d, len: %d)" dst_off dst_len len ;

  let offset = Int64.add (string_get_int64_be t 10) (Int64.of_int src_off) in
  load_bytes offset buf dst_off len

let load_bigstring t ~src_off buf ~dst_off ~len =
  let src_len = string_get_int64 t 18 |> Int64.to_int in
  let dst_len = Bigstringaf.length buf in

  if len < 0 then Fmt.invalid_arg "len must be positive (%d)" len ;
  if src_off < 0 || src_len - src_off < len
  then Fmt.invalid_arg "Invalid source bounds (src_off: %d, src_len: %d)" src_off src_len ;
  if dst_off < 0 || dst_len - dst_off < len
  then Fmt.invalid_arg "Invalid destination bounds (dst_off: %d, dst_len: %d, len: %d)" dst_off dst_len len ;

  let offset = Int64.add (string_get_int64_be t 10) (Int64.of_int src_off) in
  load_bigstring offset buf dst_off len

let map_bigstring t ~off ~len =
  let src_len = string_get_int64 t 18 |> Int64.to_int in

  if len < 0 then Fmt.invalid_arg "len must be positive (%d)" len ;
  if off < 0 || src_len - off < len
  then Fmt.invalid_arg "Invalid source bounds (off: %d, src_len: %d)" off src_len ;

  let offset = Int64.add (string_get_int64_be t 10) (Int64.of_int off) in
  map_bigstring offset len
