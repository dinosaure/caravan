let () = Printexc.record_backtrace true

module type FUNCTOR = sig type +'a t end

type ('a, 's) io

module Make (T : FUNCTOR) = struct
  type +'a s = 'a T.t
  type t

  external prj : ('a, t) io -> 'a s = "%identity"
  external inj : 'a s -> ('a, t) io = "%identity"
end

type 's scheduler =
  { bind : 'a 'b. ('a, 's) io -> ('a -> ('b, 's) io) -> ('b, 's) io
  ; return : 'a. 'a -> ('a, 's) io }

module Dllst = struct
  type 'a node =
    { mutable prev : 'a t
    ; mutable next : 'a t
    ; mutable v : 'a }
  and 'a t =
    { mutable tnext : 'a t
    ; mutable tprev : 'a t }

  external t_of_node : 'a node -> 'a t = "%identity"
  external node_of_t : 'a t -> 'a node = "%identity"

  let v () =
    let rec seq = { tprev= seq; tnext= seq; } in seq

  let iter : 'a t -> ?start:'a node -> ('a node -> unit) -> unit = fun t ?(start= node_of_t t.tnext) f ->
    let rec go cur =
      if cur != t then ( f (node_of_t cur) ; go cur.tnext ) in
    go (t_of_node start)

  let iterv : 'a t -> ?start:'a node -> ('a -> unit) -> unit = fun t ?(start= node_of_t t.tnext) f ->
    let rec go cur =
      if cur != t then ( f (node_of_t cur).v ; go cur.tnext ) in
    go (t_of_node start)

  let add_r : 'a -> 'a t -> 'a node = fun v seq ->
    let node = { prev= seq.tprev; next= seq; v; } in
    seq.tprev.tnext <- t_of_node node ;
    seq.tprev <- t_of_node node ;
    node

  let value : 'a node -> 'a = fun { v; _ } -> v
  let update : 'a node -> 'a -> unit = fun node v -> node.v <- v

  let next : anchor:'a t -> 'a node -> 'a node option = fun ~anchor next ->
    if anchor != (t_of_node next) && next.next != anchor
    then Some (node_of_t next.next)
    else None

  let prev : anchor:'a t -> 'a node -> 'a node option = fun ~anchor prev ->
    if anchor != (t_of_node prev) && prev.prev != anchor
    then Some (node_of_t prev.prev)
    else None

  let sets : start:'a node -> anchor:'a t -> f:('a -> 'a) -> unit
    = fun ~start ~anchor ~f ->
      let rec go cur =
        if (t_of_node cur) != anchor
        then ( cur.v <- f cur.v ; go (node_of_t cur.next) )
        else () in
      go start
end

module Bm = struct
  let occ needle nlen =
    let occ = Array.make 256 (-1) in
    for a = 0 to nlen - 2 do
      occ.(Char.code needle.[a]) <- a
    done ; occ

  let memcmp v a b l =
    let pos = ref 0 in
    while !pos < l && v.[a + !pos] = v.[b + !pos] do incr pos done ;
    if !pos = l then true else false

  let boyer_moore_needle_match needle nlen portion offset =
    let va = ref (nlen - offset - portion) in
    let ignore = ref 0 in
    if !va < 0 then ( ignore := - !va ; va := 0; ) ;
    if !va > 0 && needle.[!va - 1] = needle.[nlen - portion - 1]
    then false else memcmp needle (nlen - portion + !ignore) !va (portion - !ignore)

  let skip needle nlen =
    let skip = Array.make nlen 0 in
    for a = 0 to nlen - 1 do
      let value = ref 0 in
      while !value < nlen && not (boyer_moore_needle_match needle nlen a !value) do incr value done ;
      skip.(nlen - a - 1) <- !value
    done ; skip

  let search { bind; return; } ~get text hpos occ skip needle nlen =
    let ( >>= ) x f = bind x (function Ok x -> f x | Error err -> return (Error err)) in

    let npos = ref (nlen - 1) in
    let res = ref hpos in

    let rec go () =
      get text ~pos:Int64.(add hpos (of_int !npos)) >>= fun chr ->
      if needle.[!npos] = chr
      then ( if !npos = 0
             then return (Ok ())
             else ( decr npos ; go () ) )
      else ( res := (-1L) ; return (Ok ()) ) in
    go () >>= fun () ->
    (* while needle.[!npos] = get text Int64.(add hpos (of_int !npos))
       do if !npos = 0 then raise Found ; decr npos done ; res := (-1L) *)
    get text ~pos:Int64.(add hpos (of_int !npos)) >>= fun chr ->
    let shift_a = Int64.of_int skip.(!npos) in
    let shift_b =
      let x = Char.code chr in
      Int64.of_int (!npos - occ.(x)) in
    return (Ok (!res, Int64.add hpos (max shift_a shift_b)))

  let find_all ~pattern:needle =
    let nlen = String.length needle in
    let occ = occ needle nlen in
    let skip = skip needle nlen in
    fun ({ bind; return; } as s) ~ln ~get text ->
      let ( >>= ) x f = bind x (function Ok x -> f x | Error err -> return (Error err)) in

      let top = Int64.sub ln (Int64.of_int nlen) in
      let rec go hpos acc =
        if hpos > top
        then return (Ok acc)
        else search s ~get text hpos occ skip needle nlen >>= function
          | (-1L), hpos -> go hpos acc
          | res, hpos -> go hpos (res :: acc) in
      go 0L []
end

type zone = { pos : int64; len : int; }
type invalid_bounds = [ `Invalid_bounds of zone ]
type invalid_elf = [ `Invalid_ELF ]

type sub = { off : int64
           ; len: int
           ; raw: Bigstringaf.t }

type ('fd, 's, 'error) load = 'fd -> pos:int64 -> len:int -> ((sub, 'error) result, 's) io
type ('fd, 's, 'error) get = 'fd -> pos:int64 -> ((char, 'error) result, 's) io

type iovec =
  | Raw of { raw : Bigstringaf.t
            ; src_off : int
            ; dst_off : int64
            ; len : int }
  | Zero of int64

module Bduff = struct
  let src_bduff = Logs.Src.create ~doc:"bduff" "bduff"

  type 'src hunk =
    | Copy of { source : 'src
              ; src_off : int64
              ; dst_off : int64
              ; len : int64
              ; name : string option }
    | Insert of { off : int64
                ; len : int
                ; sub : sub
                ; name : string option }
    | Zero of { len : int64 }

  let pp ppf lst =
    let cursor = ref 0L in
    let pp_hunk ppf = function
      | Copy { len; name; _ } ->
        Fmt.pf ppf "%08Lx: [+] %8Lx [%a]\n%!" !cursor len Fmt.(option string) name ;
        cursor := Int64.add !cursor len
      | Zero { len; } ->
        Fmt.pf ppf "%08Lx: [0] %8Lx [zero]\n%!" !cursor len ;
        cursor := Int64.add !cursor len
      | Insert { len; name; _ } ->
        Fmt.pf ppf "%08Lx: [-] %8x [%a]\n%!" !cursor len Fmt.(option string) name ;
        cursor := Int64.add !cursor (Int64.of_int len) in
    Fmt.list ~sep:Fmt.nop pp_hunk ppf lst

  type invalid_patch = [ `Invalid_patch ]
  type 'src not_found = [ `Not_found of 'src ]

  type ('fd, 's) writev = 'fd -> iovec -> (int, 's) io

  let apply
    : type ic oc s. s scheduler -> sources:('src * ic) list -> load:(ic, s, [> invalid_bounds ]) load -> writev:(oc, s) writev -> oc -> 'src hunk list ->
      ((int64, [> invalid_patch | 'src not_found ]) result, s) io
    = fun { bind; return; } ~sources ~load ~writev dst hunks ->
      let ( >>| ) = bind in
      let ( >>= ) x f = bind x (function Ok x -> f x | Error err -> return (Error err)) in
      let cursor = ref 0L in

      let rec go = function
        | [] -> return (Ok !cursor)
        | Copy { source; src_off; dst_off; len; name; } :: r ->
          Logs.msg ~src:src_bduff Logs.Debug
            (fun m -> m "%08Lx: [+] %8Lx byte(s), src:%8Lx > dst:%8Lx [%a]"
                !cursor len src_off dst_off Fmt.(option string) name) ;
          if dst_off <> !cursor then return (Error `Invalid_patch)
          else
            ( try
                let fd = List.assoc source sources in
                let len = Int64.to_int len in
                load fd ~pos:src_off ~len >>= fun sub ->
                let sub_off = Int64.sub src_off sub.off |> Int64.to_int in
                let iovec = Raw { raw= sub.raw; src_off= sub_off; dst_off; len; } in
                writev dst iovec >>| fun v -> cursor := Int64.add !cursor (Int64.of_int v) ; go r
              with Not_found -> return (Error (`Not_found source)) )
        | Zero { len; } :: r ->
          Logs.msg ~src:src_bduff Logs.Debug
            (fun m -> m "%08Lx: [0] %8Lx zero byte(s)"
                !cursor len) ;
          let iovec : iovec = Zero len in
          writev dst iovec >>| fun v -> cursor := Int64.add !cursor (Int64.of_int v) ; go r
        | Insert { off; len; sub; name; } :: r ->
          Logs.msg ~src:src_bduff Logs.Debug
            (fun m -> m "%08Lx: [-] %8x byte(s), dst:%8Lx [%a]"
                !cursor len off Fmt.(option string) name) ;
          if off <> !cursor then return (Error `Invalid_patch)
          else
            let iovec = Raw { raw= sub.raw; src_off= Int64.to_int sub.off; dst_off= off; len; } in
            writev dst iovec >>| fun v -> cursor := Int64.add !cursor (Int64.of_int v) ; go r in
      go hunks

  let insert ?name ~off ~len sub = Insert { off; len; sub; name; }
  let copy ?name ~src_off ~dst_off ~len source = Copy { source; src_off; dst_off; len; name; }
  let zero len = Zero { len }
end

type ei_class =
  | ELF_CLASS_NONE
  | ELF_CLASS_32
  | ELF_CLASS_64

let ei_class_of_char = function
  | '\000' -> ELF_CLASS_NONE
  | '\001' -> ELF_CLASS_32
  | '\002' -> ELF_CLASS_64
  | n -> Fmt.invalid_arg "Invalid EI_CLASS value %02x" (Char.code n)

let ei_class_of_e_ident ~e_ident = String.get e_ident 4 |> ei_class_of_char

type ei_data =
  | ELF_DATA_NONE
  | ELF_DATA_2_LSB
  | ELF_DATA_2_MSB

let ei_data_of_char = function
  | '\000' -> ELF_DATA_NONE
  | '\001' -> ELF_DATA_2_LSB
  | '\002' -> ELF_DATA_2_MSB
  | n -> Fmt.invalid_arg "Invalid EI_DATA value %02x" (Char.code n)

let ei_data_of_e_ident ~e_ident = String.get e_ident 5 |> ei_data_of_char

let get_int16 ~ei_data = match ei_data with
  | ELF_DATA_NONE | ELF_DATA_2_LSB -> Bigstringaf.get_int16_le
  | ELF_DATA_2_MSB -> Bigstringaf.get_int16_be

let get_int32 ~ei_data = match ei_data with
  | ELF_DATA_NONE | ELF_DATA_2_LSB -> Bigstringaf.get_int32_le
  | ELF_DATA_2_MSB -> Bigstringaf.get_int32_be

let get_int64 ~ei_data = match ei_data with
  | ELF_DATA_NONE | ELF_DATA_2_LSB -> Bigstringaf.get_int64_le
  | ELF_DATA_2_MSB -> Bigstringaf.get_int64_be

let set_int16 ~ei_data = match ei_data with
  | ELF_DATA_NONE | ELF_DATA_2_LSB -> Bigstringaf.set_int16_le
  | ELF_DATA_2_MSB -> Bigstringaf.set_int16_be

let set_int32 ~ei_data = match ei_data with
  | ELF_DATA_NONE | ELF_DATA_2_LSB -> Bigstringaf.set_int32_le
  | ELF_DATA_2_MSB -> Bigstringaf.set_int32_be

let set_int64 ~ei_data = match ei_data with
  | ELF_DATA_NONE | ELF_DATA_2_LSB -> Bigstringaf.set_int64_le
  | ELF_DATA_2_MSB -> Bigstringaf.set_int64_be

type ehdr =
  { e_ident     : string
  ; e_type      : int
  ; e_machine   : int
  ; e_version   : int32
  ; e_entry     : int64
  ; e_phoff     : int64
  ; e_shoff     : int64
  ; e_flags     : int32
  ; e_ehsize    : int
  ; e_phentsize : int
  ; e_phnum     : int
  ; e_shentsize : int
  ; e_shnum     : int
  ; e_shstrndx  : int }

let pp_ehdr ppf t =
  Fmt.pf ppf "{ @[<hov>e_ident=     %S;@ \
                       e_type=      %d;@ \
                       e_machine=   %d;@ \
                       e_version=   %ld;@ \
                       e_entry=     %Ld;@ \
                       e_phoff=     %8Lx;@ \
                       e_shoff=     %8Lx;@ \
                       e_flags=     %8lx;@ \
                       e_ehsize=    %d;@ \
                       e_phentsize= %d;@ \
                       e_phnum=     %d;@ \
                       e_shentsize= %d;@ \
                       e_shnum=     %d;@ \
                       e_shstrndx=  %d;@] }"
    t.e_ident
    t.e_type
    t.e_machine
    t.e_version
    t.e_entry
    t.e_phoff
    t.e_shoff
    t.e_flags
    t.e_ehsize
    t.e_phentsize
    t.e_phnum
    t.e_shentsize
    t.e_shnum
    t.e_shstrndx

type e_type = ET_NONE | ET_REL | ET_EXEC | ET_DYN | ET_CORE | ET_LOPROC | ET_HIPROC

let e_type_of_ehdr { e_type; _ } = match e_type with
  | 0 -> ET_NONE
  | 1 -> ET_REL
  | 2 -> ET_EXEC
  | 3 -> ET_DYN
  | 4 -> ET_CORE
  | 0xff00 -> ET_LOPROC
  | 0xffff -> ET_HIPROC
  | n -> Fmt.invalid_arg "Invalid E_TYPE value %04x" n

let ehdr
  : type fd s. s scheduler -> load:(fd, s, [> invalid_bounds ]) load -> fd -> ((ehdr, [> invalid_bounds ]) result, s) io
  = fun { bind; return; } ~load fd ->
  let ( >>= ) = bind in

  load fd ~pos:0L ~len:64 >>= function
  | Error _ as err -> return err
  | Ok sub ->
     let sub_off = Int64.sub 0L sub.off |> Int64.to_int in
     let e_ident = Bigstringaf.substring ~off:sub_off ~len:16 sub.raw in
     let ei_data = ei_data_of_e_ident ~e_ident in

     let get_int16 = get_int16 ~ei_data in
     let get_int32 = get_int32 ~ei_data in
     let get_int64 = get_int64 ~ei_data in

     let e_type      = get_int16 sub.raw (sub_off + 16) in
     let e_machine   = get_int16 sub.raw (sub_off + 18) in
     let e_version   = get_int32 sub.raw (sub_off + 20) in
     let e_entry     = get_int64 sub.raw (sub_off + 24) in
     let e_phoff     = get_int64 sub.raw (sub_off + 32) in
     let e_shoff     = get_int64 sub.raw (sub_off + 40) in
     let e_flags     = get_int32 sub.raw (sub_off + 48) in
     let e_ehsize    = get_int16 sub.raw (sub_off + 52) in
     let e_phentsize = get_int16 sub.raw (sub_off + 54) in
     let e_phnum     = get_int16 sub.raw (sub_off + 56) in
     let e_shentsize = get_int16 sub.raw (sub_off + 58) in
     let e_shnum     = get_int16 sub.raw (sub_off + 60) in
     let e_shstrndx  = get_int16 sub.raw (sub_off + 62) in

     let res =
       { e_ident
       ; e_type
       ; e_machine
       ; e_version
       ; e_entry
       ; e_phoff
       ; e_shoff
       ; e_flags
       ; e_ehsize
       ; e_phentsize
       ; e_phnum
       ; e_shentsize
       ; e_shnum
       ; e_shstrndx } in

     return (Ok res)

let ehdr_to_bigstring ehdr =
  let raw = Bigstringaf.create ehdr.e_ehsize in
  let ei_data = ei_data_of_e_ident ~e_ident:ehdr.e_ident in
  let set_int16 = set_int16 ~ei_data in
  let set_int32 = set_int32 ~ei_data in
  let set_int64 = set_int64 ~ei_data in

  Bigstringaf.blit_from_string ehdr.e_ident ~src_off:0 raw ~dst_off:0 ~len:16 ;
  set_int16 raw 16 ehdr.e_type ;
  set_int16 raw 18 ehdr.e_machine ;
  set_int32 raw 20 ehdr.e_version ;
  set_int64 raw 24 ehdr.e_entry ;
  set_int64 raw 32 ehdr.e_phoff ;
  set_int64 raw 40 ehdr.e_shoff ;
  set_int32 raw 48 ehdr.e_flags ;
  set_int16 raw 52 ehdr.e_ehsize ;
  set_int16 raw 54 ehdr.e_phentsize ;
  set_int16 raw 56 ehdr.e_phnum ;
  set_int16 raw 58 ehdr.e_shentsize ;
  set_int16 raw 60 ehdr.e_shnum ;
  set_int16 raw 62 ehdr.e_shstrndx ;

  raw

type phdr =
  { p_type : int32
  ; p_flags : int32
  ; p_offset : int64
  ; p_vaddr : int64
  ; p_paddr : int64
  ; p_filesz : int64
  ; p_memsz : int64
  ; p_align : int64 }

type p_type =
  | PT_NULL
  | PT_LOAD
  | PT_DYNAMIC
  | PT_INTERP
  | PT_NOTE
  | PT_SHLIB
  | PT_LOPROC
  | PT_HIPROC
  | PT_GNU_STACK
  | PT_OTHER of int32

let p_type_of_phdr phdr = match phdr.p_type with
  | 0l -> PT_NULL
  | 1l -> PT_LOAD
  | 2l -> PT_DYNAMIC
  | 3l -> PT_INTERP
  | 4l -> PT_NOTE
  | 5l -> PT_SHLIB
  | 0x70000000l -> PT_LOPROC
  | 0x7fffffffl -> PT_HIPROC
  | 0x6474e551l -> PT_GNU_STACK
  | v -> PT_OTHER v

let phdr_equal a b =
  a.p_type = b.p_type
  && a.p_flags = b.p_flags
  && a.p_offset = b.p_offset
  && a.p_vaddr = b.p_vaddr
  && a.p_paddr = b.p_paddr
  && a.p_filesz = b.p_filesz
  && a.p_memsz = b.p_memsz
  && a.p_align = b.p_align

let pp_phdr ppf t =
  Fmt.pf ppf "{ @[<hov>p_type=   %8lx;@ \
                       p_flags=  %8lx;@ \
                       p_offset= %8Lx;@ \
                       p_vaddr=  %8Lx;@ \
                       p_paddr=  %8Lx;@ \
                       p_filesz= %8Lx;@ \
                       p_memsz=  %8Lx;@ \
                       p_align=  %8Lx;@] }"
    t.p_type
    t.p_flags
    t.p_offset
    t.p_vaddr
    t.p_paddr
    t.p_filesz
    t.p_memsz
    t.p_align

let phdr
  : type fd s. s scheduler -> load:(fd, s, [> invalid_bounds ]) load -> fd -> ehdr:ehdr -> ((phdr list, [> invalid_bounds | invalid_elf ]) result, s) io
  = fun { bind; return; } ~load fd ~ehdr ->
    let ( >>= ) = bind in

    let segment n =
      let pos = Int64.(add ehdr.e_phoff (of_int (ehdr.e_phentsize * n))) in
      load fd ~pos ~len:ehdr.e_phentsize >>= function
      | Error _ as err -> return err
      | Ok sub ->
        let sub_off = Int64.sub pos sub.off |> Int64.to_int in
        let ei_data = ei_data_of_e_ident ~e_ident:ehdr.e_ident in
        let ei_class = ei_class_of_e_ident ~e_ident:ehdr.e_ident in

        let get_int32 = get_int32 ~ei_data in
        let get_int64 = get_int64 ~ei_data in

        match ei_class with
        | ELF_CLASS_NONE -> return (Error `Invalid_ELF)
        | ELF_CLASS_32 ->
          let p_type   = get_int32 sub.raw (sub_off + 0) in
          let p_offset = get_int32 sub.raw (sub_off + 4) in
          let p_vaddr  = get_int32 sub.raw (sub_off + 8) in
          let p_paddr  = get_int32 sub.raw (sub_off + 12) in
          let p_filesz = get_int32 sub.raw (sub_off + 16) in
          let p_memsz  = get_int32 sub.raw (sub_off + 20) in
          let p_flags  = get_int32 sub.raw (sub_off + 24) in
          let p_align  = get_int32 sub.raw (sub_off + 28) in

          let res =
            { p_type
            ; p_offset= Int64.of_int32 p_offset
            ; p_vaddr= Int64.of_int32 p_vaddr
            ; p_paddr= Int64.of_int32 p_paddr
            ; p_filesz= Int64.of_int32 p_filesz
            ; p_memsz= Int64.of_int32 p_memsz
            ; p_flags
            ; p_align= Int64.of_int32 p_align } in

          return (Ok res)
        | ELF_CLASS_64 ->
          let p_type   = get_int32 sub.raw (sub_off + 0) in
          let p_flags  = get_int32 sub.raw (sub_off + 4) in
          let p_offset = get_int64 sub.raw (sub_off + 8) in
          let p_vaddr  = get_int64 sub.raw (sub_off + 16) in
          let p_paddr  = get_int64 sub.raw (sub_off + 24) in
          let p_filesz = get_int64 sub.raw (sub_off + 32) in
          let p_memsz  = get_int64 sub.raw (sub_off + 40) in
          let p_align  = get_int64 sub.raw (sub_off + 48) in

          let res =
            { p_type
            ; p_offset
            ; p_vaddr
            ; p_paddr
            ; p_filesz
            ; p_memsz
            ; p_flags
            ; p_align } in

          return (Ok res) in
    let rec go n a =
      if n >= 0
      then segment n >>= function Error _ as err -> return err | Ok v -> go (pred n) (v :: a)
      else return (Ok a) in
    go (pred ehdr.e_phnum) []

let phdr_to_bigstring ~ehdr phdr =
  let raw = Bigstringaf.create ehdr.e_phentsize in
  let ei_data = ei_data_of_e_ident ~e_ident:ehdr.e_ident in
  let ei_class = ei_class_of_e_ident ~e_ident:ehdr.e_ident in
  let set_int32 = set_int32 ~ei_data in
  let set_int64 = set_int64 ~ei_data in

  match ei_class with
  | ELF_CLASS_NONE -> Fmt.invalid_arg "Invalid Program Header"
  | ELF_CLASS_32 ->
    set_int32 raw 0 phdr.p_type ;
    set_int32 raw 4 (Int64.to_int32 phdr.p_offset) ;
    set_int32 raw 8 (Int64.to_int32 phdr.p_vaddr) ;
    set_int32 raw 12 (Int64.to_int32 phdr.p_paddr) ;
    set_int32 raw 16 (Int64.to_int32 phdr.p_filesz) ;
    set_int32 raw 20 (Int64.to_int32 phdr.p_memsz) ;
    set_int32 raw 24 phdr.p_flags ;
    set_int32 raw 28 (Int64.to_int32 phdr.p_align) ;

    raw
  | ELF_CLASS_64 ->
    set_int32 raw 0 phdr.p_type ;
    set_int32 raw 4 phdr.p_flags ;
    set_int64 raw 8 phdr.p_offset ;
    set_int64 raw 16 phdr.p_vaddr ;
    set_int64 raw 24 phdr.p_paddr ;
    set_int64 raw 32 phdr.p_filesz ;
    set_int64 raw 40 phdr.p_memsz ;
    set_int64 raw 48 phdr.p_align ;

    raw

type shdr =
  { sh_name : int32
  ; sh_type : int32
  ; sh_flags : int64
  ; sh_addr : int64
  ; sh_offset : int64
  ; sh_size : int64
  ; sh_link : int32
  ; sh_info : int32
  ; sh_addralign : int64
  ; sh_entsize : int64 }

let pp_shdr ppf t =
  Fmt.pf ppf "{ @[<hov>sh_name=      %8ld;@ \
                       sh_type=      %8lx;@ \
                       sh_flags=     %8Lx;@ \
                       sh_addr=      %8Lx;@ \
                       sh_offset=    %8Lx;@ \
                       sh_size=      %8Lx;@ \
                       sh_link=      %8lx;@ \
                       sh_info=      %8lx;@ \
                       sh_addralign= %8Lx;@ \
                       sh_entsize=   %8Lx;@] }"
    t.sh_name
    t.sh_type
    t.sh_flags
    t.sh_addr
    t.sh_offset
    t.sh_size
    t.sh_link
    t.sh_info
    t.sh_addralign
    t.sh_entsize

let shdr
  : type fd s. s scheduler -> load:(fd, s, [> invalid_bounds ]) load -> fd -> ehdr:ehdr -> ((shdr list, [> invalid_bounds | invalid_elf ]) result, s) io
  = fun { bind; return; } ~load fd ~ehdr ->
    let ( >>= ) = bind in

    let section n =
      let pos = Int64.(add ehdr.e_shoff (of_int (ehdr.e_shentsize * n))) in
      load fd ~pos ~len:ehdr.e_phentsize >>= function
      | Error _ as err -> return err
      | Ok sub ->
        let sub_off = Int64.sub pos sub.off |> Int64.to_int in
        let ei_data = ei_data_of_e_ident ~e_ident:ehdr.e_ident in
        let ei_class = ei_class_of_e_ident ~e_ident:ehdr.e_ident in

        let get_int32 = get_int32 ~ei_data in
        let get_int64 = get_int64 ~ei_data in

        match ei_class with
        | ELF_CLASS_NONE -> return (Error `Invalid_ELF)
        | ELF_CLASS_32 ->
          let sh_name      = get_int32 sub.raw (sub_off + 0) in
          let sh_type      = get_int32 sub.raw (sub_off + 4) in
          let sh_flags     = get_int32 sub.raw (sub_off + 8) in
          let sh_addr      = get_int32 sub.raw (sub_off + 12) in
          let sh_offset    = get_int32 sub.raw (sub_off + 16) in
          let sh_size      = get_int32 sub.raw (sub_off + 20) in
          let sh_link      = get_int32 sub.raw (sub_off + 24) in
          let sh_info      = get_int32 sub.raw (sub_off + 28) in
          let sh_addralign = get_int32 sub.raw (sub_off + 32) in
          let sh_entsize   = get_int32 sub.raw (sub_off + 36) in

          let res =
            { sh_name
            ; sh_type
            ; sh_flags= Int64.of_int32 sh_flags
            ; sh_addr= Int64.of_int32 sh_addr
            ; sh_offset= Int64.of_int32 sh_offset
            ; sh_size= Int64.of_int32 sh_size
            ; sh_link
            ; sh_info
            ; sh_addralign= Int64.of_int32 sh_addralign
            ; sh_entsize= Int64.of_int32 sh_entsize } in

          return (Ok res)
        | ELF_CLASS_64 ->
          let sh_name      = get_int32 sub.raw (sub_off + 0) in
          let sh_type      = get_int32 sub.raw (sub_off + 4) in
          let sh_flags     = get_int64 sub.raw (sub_off + 8) in
          let sh_addr      = get_int64 sub.raw (sub_off + 16) in
          let sh_offset    = get_int64 sub.raw (sub_off + 24) in
          let sh_size      = get_int64 sub.raw (sub_off + 32) in
          let sh_link      = get_int32 sub.raw (sub_off + 40) in
          let sh_info      = get_int32 sub.raw (sub_off + 44) in
          let sh_addralign = get_int64 sub.raw (sub_off + 48) in
          let sh_entsize   = get_int64 sub.raw (sub_off + 56) in

          let res =
            { sh_name
            ; sh_type
            ; sh_flags
            ; sh_addr
            ; sh_offset
            ; sh_size
            ; sh_link
            ; sh_info
            ; sh_addralign
            ; sh_entsize } in

          return (Ok res) in
    let rec go n a =
      if n < ehdr.e_shnum
      then section n >>= function Error _ as err -> return err | Ok v -> go (succ n) (v :: a)
      else return (Ok (List.rev a)) in
    go 0 []

let shdr_to_bigstring ~ehdr shdr =
  let raw = Bigstringaf.create ehdr.e_shentsize in
  let ei_data = ei_data_of_e_ident ~e_ident:ehdr.e_ident in
  let ei_class = ei_class_of_e_ident ~e_ident:ehdr.e_ident in

  let set_int32 = set_int32 ~ei_data in
  let set_int64 = set_int64 ~ei_data in

  match ei_class with
  | ELF_CLASS_NONE -> Fmt.invalid_arg "Invalid Section Header"
  | ELF_CLASS_32 ->
    set_int32 raw 0 shdr.sh_name ;
    set_int32 raw 4 shdr.sh_type ;
    set_int32 raw 8 (Int64.to_int32 shdr.sh_flags) ;
    set_int32 raw 12 (Int64.to_int32 shdr.sh_addr) ;
    set_int32 raw 16 (Int64.to_int32 shdr.sh_offset) ;
    set_int32 raw 20 (Int64.to_int32 shdr.sh_size) ;
    set_int32 raw 24 shdr.sh_link ;
    set_int32 raw 28 shdr.sh_info ;
    set_int32 raw 32 (Int64.to_int32 shdr.sh_addralign) ;
    set_int32 raw 36 (Int64.to_int32 shdr.sh_entsize) ;

    raw
  | ELF_CLASS_64 ->
    set_int32 raw 0 shdr.sh_name ;
    set_int32 raw 4 shdr.sh_type ;
    set_int64 raw 8 shdr.sh_flags ;
    set_int64 raw 16 shdr.sh_addr ;
    set_int64 raw 24 shdr.sh_offset ;
    set_int64 raw 32 shdr.sh_size ;
    set_int32 raw 40 shdr.sh_link ;
    set_int32 raw 44 shdr.sh_info ;
    set_int64 raw 48 shdr.sh_addralign ;
    set_int64 raw 56 shdr.sh_entsize ;

    raw

type t =
  { hdr : ehdr
  ; sht : shdr list
  ; pht : phdr list
  ; pscts : psct list }
and psct =
  | Zero of int64
  | Section of { name : string option
               ; shdr : int
               ; off : int64
               ; len : int64 }

let pp_psct ppf = function
  | Zero len -> Fmt.pf ppf "@[<1>(Zero %Ld)@]" len
  | Section { name; shdr; off; len; } ->
    Fmt.pf ppf "@[<1>(Section { @[<hov>name= @[<hov>%a@];@\n\
                                       shdr= %2d;@ \
                                       off=  %8Lx;@ \
                                       len=  %8Lx;@] })@]"
      Fmt.(Dump.option string) name
      shdr off len

let pp ppf t =
  Fmt.pf ppf "{ @[<hov>hdr= @[<hov>%a@];@ \
                       sht= @[<hov>%a@];@ \
                       pht= @[<hov>%a@];@ \
                       pscts= @[<hov>%a@];@] }"
    pp_ehdr t.hdr
    Fmt.(Dump.list pp_shdr) t.sht
    Fmt.(Dump.list pp_phdr) t.pht
    Fmt.(Dump.list pp_psct) t.pscts

let offset ?(start= 0L) psct =
  let rec go acc = function
    | [] -> acc
    | Zero len :: r -> go (Int64.add len acc) r
    | Section { len; _ } :: r ->
      go (Int64.add len acc) r in
  go start psct

let interval ~start ~x ~len =
  x >= start && x < Int64.add start len

let segment_of_sh_addr phdr ~sh_addr =
  match List.find (fun phdr -> interval ~start:phdr.p_vaddr ~x:sh_addr ~len:phdr.p_memsz) phdr with
  | x -> Ok x
  | exception Not_found -> Error `Phdr_not_found

let name_of_sh_name
  : type fd s. s scheduler -> load:(fd, s, [> invalid_bounds ]) load -> get:(fd, s, [> invalid_bounds ]) get ->
               fd -> ehdr:ehdr -> shdr:shdr list -> sh_name:int32 -> ((string, [> invalid_bounds ]) result, s) io
  = fun { bind; return; } ~load ~get fd ~ehdr ~shdr ~sh_name ->
    let shstr = List.nth shdr ehdr.e_shstrndx in
    let pos = Int64.add shstr.sh_offset (Int64.of_int32 sh_name) in

    let ( >>= ) x f = bind x (function Ok x -> f x | Error _ as err -> return err) in

    let rec length pos =
      get ~pos fd >>= function
      | '\000' -> return (Ok pos)
      | _ -> length (Int64.succ pos) in
    length pos >>= fun len ->
    let len = Int64.to_int (Int64.sub len pos) in
    load ~pos fd ~len >>= fun sub ->
    let sub_off = Int64.sub pos sub.off |> Int64.to_int in
    let res = Bigstringaf.substring ~off:sub_off ~len sub.raw in
    return (Ok res)

type sh_type =
  | SHT_NULL
  | SHT_PROGBITS
  | SHT_SYMTAB
  | SHT_STRTAB
  | SHT_RELA
  | SHT_HASH
  | SHT_DYNAMIC
  | SHT_NOTE
  | SHT_NOBITS
  | SHT_REL
  | SHT_SHLIB
  | SHT_DYNSYM
  | SHT_LOPROC
  | SHT_HIPROC
  | SHT_LOUSER
  | SHT_HIUSER

  | SHT_OTHER of int32

let sh_type_of_shdr shdr = match shdr.sh_type with
  | 0l -> SHT_NULL
  | 1l -> SHT_PROGBITS
  | 2l -> SHT_SYMTAB
  | 3l -> SHT_STRTAB
  | 4l -> SHT_RELA
  | 5l -> SHT_HASH
  | 6l -> SHT_DYNAMIC
  | 7l -> SHT_NOTE
  | 8l -> SHT_NOBITS
  | 9l -> SHT_REL
  | 10l -> SHT_SHLIB
  | 11l -> SHT_DYNSYM
  | 0x70000000l -> SHT_LOPROC
  | 0x7fffffffl -> SHT_HIPROC
  | 0x80000000l -> SHT_LOUSER
  | 0x8fffffffl -> SHT_HIUSER
  | n -> SHT_OTHER n

let sh_size shdr =
  match sh_type_of_shdr shdr with
  | SHT_NOBITS -> 0L
  | _ -> shdr.sh_size

let make
  : type fd s. s scheduler -> load:(fd, s, [> invalid_bounds ]) load -> get:(fd, s, [> invalid_bounds ]) get ->
               fpath:Fpath.t -> fd -> ((t, [> invalid_bounds | invalid_elf ]) result, s) io
  = fun ({ bind; return; } as s) ~load ~get ~fpath:_ fd ->
    let ( >>= ) x f = bind x (function Ok x -> f x | Error _ as err -> return err) in

    ehdr s ~load fd >>= fun ehdr ->
    phdr s ~load fd ~ehdr >>= fun phdr ->
    shdr s ~load fd ~ehdr >>= fun shdr ->

    let start = Int64.add ehdr.e_phoff (Int64.of_int (ehdr.e_phentsize * ehdr.e_phnum)) in
    let compare_shdr_offset a b = Int64.compare a.sh_offset b.sh_offset in

    let rec go acc = function
      | [] -> return (Ok (List.rev acc))
      | (n, x) :: r when sh_type_of_shdr x <> SHT_NULL ->
         name_of_sh_name s ~load ~get fd ~ehdr ~shdr ~sh_name:x.sh_name >>= fun name ->
         let cur = offset ~start acc in
         let acc =
           if cur >= x.sh_offset
           then (Section { shdr= n
                         ; off= x.sh_offset
                         ; len= sh_size x
                         ; name= Some name } :: acc)
           else
             ( let pad = Zero (Int64.sub x.sh_offset cur) in
               let sct = Section { shdr= n
                                 ; off= x.sh_offset
                                 ; len= sh_size x
                                 ; name= Some name } in
               sct :: pad :: acc ) in
         go acc r
      | _ :: r -> go acc r in
    let nshdr = List.mapi (fun i x -> (i, x)) shdr in
    go [] (List.sort (fun (_, a) (_, b) -> compare_shdr_offset a b) nshdr) >>= fun pscts ->

    let res =
      { hdr= ehdr
      ; sht= shdr
      ; pht= phdr
      ; pscts } in

    return (Ok res)

let nobits_scts t =
  let assoc n pscts = List.find (function Section { shdr; _ } -> shdr = n | _ -> false) pscts in
  let res, _ = List.fold_left
    (fun (acc, n) shdr -> match sh_type_of_shdr shdr with
       | SHT_NOBITS -> (shdr, assoc n t.pscts) :: acc, succ n
       | _ -> acc, succ n)
    ([], 0) t.sht in
  res

let pscts_of_offset ~sh_offset t =
  let rec go cur (bf, af) = function
    | [] -> List.rev bf, List.rev af
    | (Section { len; _ } | Zero len) as x :: r ->
      let cur' = Int64.add cur len in
      if cur >= sh_offset
      then go cur' (bf, x :: af) r
      else go cur' (x :: bf, af) r in
  let start = Int64.add t.hdr.e_phoff (Int64.of_int (t.hdr.e_phentsize * t.hdr.e_phnum)) in
  go start ([], []) t.pscts

module Int64 = struct include Int64 let ( + ) = add let ( - ) = sub let ( * ) = mul let ( / ) = div end

(* XXX FIXUP BSS XXX *)

let fix_nobits_sct t bss psct =
  let n_bss, shifts = match psct with
    | Section { shdr= n; _ } ->
      let _, pscts = List.partition
          (function
            | Section { shdr= shdr'; _ } -> n = shdr'
            | _ -> false)
          (pscts_of_offset ~sh_offset:bss.sh_offset t |> snd) in
      n, pscts
    | _ -> Fmt.invalid_arg "Invalid physical section" in
  let ssize, rsize = match psct with
    | Section { shdr= n_bss; _ } ->
      let size0 =
        if n_bss > 0
        then
          let prev = List.nth t.sht (pred n_bss) in
          (* XXX(dinosaure): fixup file offset of BSS section and makes it
             matches the previous section file offset added to the previous
             size. fixup phdr size. *)
          let lpad = let open Int64 in bss.sh_offset - (prev.sh_offset + prev.sh_size) in
          (* XXX(dinosaure): make sure the virtual address difference and the
             file offset difference is identical from the section before BSS,
             and BSS. *)
          let diff = let open Int64 in (bss.sh_addr - prev.sh_addr) - (bss.sh_offset - prev.sh_offset) in
          Fmt.epr ">>> lpad: %08Lx\n%!" lpad ;
          Fmt.epr ">>> diff: %08Lx\n%!" diff ;
          Int64.sub diff lpad
        else 0L in
      let size1 =
        if succ n_bss < t.hdr.e_shnum
        then
          let next = List.nth t.sht (succ n_bss) in
          if next.sh_offset <= Int64.add bss.sh_offset bss.sh_size
          then Int64.sub bss.sh_offset next.sh_offset else 0L
        else 0L in
      Fmt.epr ">>> %08Lx = %08Lx + %08Lx\n%!" Int64.(size0 + size1) size0 size1 ;
      Int64.(size0 + size1), bss.sh_size
    | _ -> 0L, 0L in
  (n_bss, bss), shifts, ssize, rsize

let shift_shdr ~shift shdr =
  { shdr with sh_offset= Int64.add shdr.sh_offset shift }

let pad_shdr t =
  let start = Int64.add t.hdr.e_phoff (Int64.of_int (t.hdr.e_phentsize * t.hdr.e_phnum)) in
  let top = offset ~start t.pscts in
  if top < t.hdr.e_shoff then Int64.sub t.hdr.e_shoff top else 0L

let patch_of_fix_nobits t (n_bss, bss) shifts (ssize, rsize) =
  let open Rresult.R in
  let old_e_shoff = t.hdr.e_shoff in
  let ehdr = { t.hdr with e_shoff= Int64.(t.hdr.e_shoff + ssize + rsize) } in
  segment_of_sh_addr t.pht ~sh_addr:bss.sh_addr >>= fun segment ->
  let hunk_ehdr =
    Bduff.insert ~name:"ELF header" ~off:0L ~len:ehdr.e_ehsize { off= 0L; len= ehdr.e_ehsize; raw= ehdr_to_bigstring ehdr } in
  let hunks_phdr =
    List.mapi
      (fun i phdr ->
         let off = Int64.add ehdr.e_phoff (Int64.of_int (i * ehdr.e_phentsize)) in
         let len = ehdr.e_phentsize in

         if phdr_equal phdr segment
         then
           let phdr = { phdr with p_filesz= Int64.(phdr.p_filesz + ssize + rsize) } in
           Bduff.insert ~name:"Segment of BSS" ~off ~len { off= 0L; len; raw= phdr_to_bigstring ~ehdr phdr }
         else Bduff.copy ~name:(Fmt.strf "Segment %2d" i) ~src_off:off ~dst_off:off ~len:(Int64.of_int len) `Binary)
      t.pht in
  let pscts0 = pscts_of_offset ~sh_offset:bss.sh_offset t |> fst in
  let hunks_psct0 =
    List.map (function
        | Section { off; len; name; _ } ->
          Bduff.copy ?name ~src_off:off ~dst_off:off ~len `Binary
        | Zero len -> Bduff.zero len)
      pscts0 in
  let hunks_psct1 =
    List.mapi (fun n psct -> match n, psct with
        | _, Section { off; len; name; _ } ->
          Bduff.copy ?name ~src_off:off ~dst_off:Int64.(off + rsize + ssize) ~len `Binary
        | _, Zero len -> Bduff.zero len)
      shifts in
  let hunks_psct1 = Bduff.zero Int64.(ssize + rsize) :: hunks_psct1 in
  let pad_shdr =
    let len = pad_shdr t in if len = 0L then [] else [ Bduff.zero len ] in
  let hunks_shdr =
    List.mapi (fun n shdr ->
        let off = Int64.add ehdr.e_shoff (Int64.of_int (n * ehdr.e_shentsize)) in
        let src_off = Int64.add old_e_shoff (Int64.of_int (n * ehdr.e_shentsize)) in
        let len = ehdr.e_shentsize in
        if n = n_bss then Bduff.insert ~name:".bss" ~off ~len { off= 0L; len; raw= shdr_to_bigstring ~ehdr bss }
        else if n > n_bss then
          let shdr = shift_shdr ~shift:Int64.(ssize + rsize) shdr in
          Bduff.insert ~name:(Fmt.strf "Section %2d" n) ~off ~len { off= 0L; len; raw= shdr_to_bigstring ~ehdr shdr }
        else Bduff.copy ~name:(Fmt.strf "Section %2d" n) ~src_off ~dst_off:off ~len:(Int64.of_int len) `Binary)
      t.sht in
  Ok (hunk_ehdr :: hunks_phdr @ hunks_psct0 @ hunks_psct1 @ pad_shdr @ hunks_shdr)

(* XXX INJECT SECTION XXX *)

let last_section_loaded_of_phdr t phdr =
  let rec go last = function
    | [] -> last
    | Zero _ :: r -> go last r
    | Section { off; len; _ } as sct :: r ->
      if off < phdr.p_offset
      then go last r
      else if off >= phdr.p_offset && Int64.(off + len) <= Int64.(phdr.p_offset + phdr.p_filesz)
      then
        ( match last with
        | None -> go (Some sct) r
        | Some (Section { off= off0; len= len0; _ }) ->
          if Int64.(off0 + len0) > Int64.(off + len)
          then go last r
          else go (Some sct) r
        | Some _ -> go (Some sct) r)
      else go last r in
  match go None t.pscts with
  | Some psct -> Ok psct
  | None -> Error (`Unaligned_or_unexistant_psct_into phdr)

let last_pt_load pht =
  try let v = List.find (fun phdr -> p_type_of_phdr phdr = PT_LOAD) (List.rev pht) in Ok v
  with Not_found -> Error `Phdr_not_found

module Name : sig type t = private string val v : string -> t val length : t -> int val to_string : t -> string end = struct
  type t = string

  let v x = x ^ "\000"
  let length = String.length
  let to_string x = String.sub x 0 (String.length x - 1)
end

let inject_shstr_name ~(name:Name.t) t =
  let shstr = List.nth t.sht t.hdr.e_shstrndx in

  Int64.to_int32 shstr.sh_size,
  (fun ~dst_off ->
     let hunk_shstr = Bduff.copy ~src_off:shstr.sh_offset ~dst_off ~len:shstr.sh_size ~name:".shstrtab" `Binary in
     let hunk_name = Bduff.insert ~off:Int64.(dst_off + shstr.sh_size) ~len:(Name.length name)
         { raw= Bigstringaf.of_string ~off:0 ~len:(Name.length name) (name :> string)
         ; off= 0L; len= Name.length name } in
     [ hunk_shstr; hunk_name ]),
  (fun ~sh_offset ~dst_off ->
     let shstr = { shstr with sh_offset= sh_offset
                            ; sh_size= Int64.(shstr.sh_size + (of_int (Name.length name))) } in
     let raw = shdr_to_bigstring ~ehdr:t.hdr shstr in
     [ Bduff.insert ~name:".shstrtab" ~off:dst_off ~len:t.hdr.e_shentsize { raw; off= 0L; len= t.hdr.e_shentsize } ])

let size_of_addr ~ehdr = match ei_class_of_e_ident ~e_ident:ehdr.e_ident with
  | ELF_CLASS_32 -> 4L
  | ELF_CLASS_64 -> 8L
  | ELF_CLASS_NONE -> Fmt.invalid_arg "Invalid ELF binary"

let shift_sh_link n_shdr shdr = match sh_type_of_shdr shdr with
  | SHT_DYNAMIC
  | SHT_HASH
  | SHT_REL | SHT_RELA
  | SHT_SYMTAB | SHT_DYNSYM
  | SHT_OTHER 0x200l ->
    if shdr.sh_link >= Int32.of_int n_shdr
    then { shdr with sh_link= Int32.succ shdr.sh_link }
    else shdr
  | _ -> shdr

let craft_new_data_section t ~name:sect_name sh_size =
  let open Rresult.R in
  last_pt_load t.pht >>= fun phdr ->
  last_section_loaded_of_phdr t phdr >>= function
  | Zero _ -> Fmt.invalid_arg "Invalid last section of last PT_LOAD segment"
  | Section { shdr= n_last; _ } ->
    let last = List.nth t.sht n_last in
    let vpad =
      if Int64.(rem (last.sh_addr + last.sh_size) (size_of_addr ~ehdr:t.hdr)) <> 0L
      then let open Int64 in (size_of_addr ~ehdr:t.hdr) - (rem (last.sh_addr + last.sh_size) (size_of_addr ~ehdr:t.hdr))
      else 0L in
    let ppad =
      if Int64.(rem (last.sh_offset + last.sh_size) last.sh_addralign) <> 0L
      then let open Int64 in last.sh_addralign - (rem (last.sh_offset + last.sh_size) last.sh_addralign)
      else 0L in
    (* XXX(dinosaure): even if [sh_size] should be equal to 0, (it's a .bss
       section), we fixed it on the previous pass. *)
    let last_sh_size =
      Int64.(last.sh_size + (rem last.sh_size last.sh_addralign)) in
    let lpad =
      let open Int64 in
      phdr.p_filesz - (last.sh_offset - phdr.p_offset) - last_sh_size in
    Fmt.epr "PHDR FILEZ: %08Lx\n%!" phdr.p_filesz ;
    Fmt.epr "P_OFFSET: %08Lx, SH_OFFSET: %08Lx\n%!" phdr.p_offset last.sh_offset ;
    Fmt.epr "LPAD: %08Lx\n%!" lpad ;
    let sh_addr = let open Int64 in last.sh_addr + last.sh_size + vpad in
    Fmt.epr ">>> last.sh_size: %08Lx, last_sh_size: %08Lx.\n%!" last.sh_size last_sh_size ;
    Fmt.epr ">>> last.sh_offset: %08Lx, last_sh_size: %08Lx, ppad: %08Lx.\n%!" last.sh_offset last_sh_size ppad ;
    let sh_offset = let open Int64 in last.sh_offset + last_sh_size + lpad in
    let sh_name, _, _ = inject_shstr_name ~name:sect_name t in
    let shdr =
      { sh_name
      ; sh_type= 1l (* SHT_LOAD *)
      ; sh_flags= 3L (* SHT_WRITE | SHT_ALLOC *)
      ; sh_addr
      ; sh_offset
      ; sh_size
      ; sh_link= 0l
      ; sh_info= 0l
      ; sh_addralign= 1L
      ; sh_entsize= 0L } in
    let psct = Section { name= Some (Name.to_string sect_name)
                       ; shdr= succ n_last
                       ; off= sh_offset
                       ; len= sh_size } in
    let phdr = { phdr with p_filesz= Int64.add phdr.p_filesz sh_size
                         ; p_memsz= Int64.add phdr.p_memsz sh_size } in
    Ok (phdr, succ n_last, shdr, psct)

let inject_new_data_section t ~name:sect_name sh_size (new_pt_load, n_shdr, new_shdr, _psct) =
  let _, craft_strtab, craft_shstrtab = inject_shstr_name ~name:sect_name t in
  let old_e_shoff = t.hdr.e_shoff in
  let ehdr =
    { t.hdr with e_shstrndx= if n_shdr <= t.hdr.e_shstrndx then succ t.hdr.e_shstrndx else t.hdr.e_shstrndx
               ; e_shnum= succ t.hdr.e_shnum
               ; e_shoff= Int64.(t.hdr.e_shoff + sh_size + (Int64.of_int (Name.length sect_name))) } in
  let open Rresult.R in
  last_pt_load t.pht >>= fun last_pt_load ->
  let _hunk_ehdr =
    Bduff.insert ~name:"ELF header" ~off:0L ~len:ehdr.e_ehsize { off= 0L; len= ehdr.e_ehsize; raw= ehdr_to_bigstring ehdr } in
  let _hunks_phdr =
    List.mapi
      (fun i phdr ->
         let off = Int64.add ehdr.e_phoff (Int64.of_int (i * ehdr.e_phentsize)) in
         let len = ehdr.e_phentsize in

         if phdr_equal phdr last_pt_load
         then Bduff.insert ~name:"Segment of injection" ~off ~len { off= 0L; len; raw= phdr_to_bigstring ~ehdr new_pt_load }
         else Bduff.copy ~name:(Fmt.strf "Segment %2d" i) ~src_off:off ~dst_off:off ~len:(Int64.of_int len) `Binary)
      t.pht in
  let pscts0, shifts = pscts_of_offset ~sh_offset:new_shdr.sh_offset t in
  (* XXX(dinosaure): assume that .shstrtab is __after__ [shdr]. *)
  let _hunks_psct0s =
    List.map (function
        | Section { off; len; name; _ } ->
          Bduff.copy ?name ~src_off:off ~dst_off:off ~len `Binary
        | Zero len -> Bduff.zero len)
      pscts0 in
  Fmt.epr ">>> @[<hov>%a@]\n%!" Bduff.pp _hunks_psct0s ;
  let shstrtab_sh_offset = ref 0L in
  let _hunks_psct1s =
    List.map (function
        | Section { off; len; name; shdr= n } ->
          if n = t.hdr.e_shstrndx
          then ( shstrtab_sh_offset := Int64.(off + sh_size) ; craft_strtab ~dst_off:Int64.(off + sh_size) )
          else [ Bduff.copy ?name ~src_off:off ~dst_off:Int64.(off + sh_size) ~len `Binary ]
        | Zero len -> [ Bduff.zero len ])
      shifts |> List.concat in
  Fmt.epr ">>> %08Lx\n%!" new_shdr.sh_offset ;
  let _new_psct = [ Bduff.copy ~src_off:0L ~dst_off:new_shdr.sh_offset ~name:".provision" ~len:sh_size `Provision ] in
  let _pad_shdr =
    let len = pad_shdr t in if len = 0L then [] else [ Bduff.zero len ] in
  let _hunks_shdr =
    List.fold_left (fun (acc, rn, on) shdr ->
        let off = Int64.add ehdr.e_shoff (Int64.of_int (rn * ehdr.e_shentsize)) in
        let src_off = Int64.add old_e_shoff (Int64.of_int (on * ehdr.e_shentsize)) in
        let len = ehdr.e_shentsize in
        let shdr = shift_sh_link n_shdr shdr in

        if rn = n_shdr
        then
          let new_hunk = Bduff.insert ~name:(Name.to_string sect_name) ~off ~len { off= 0L; len; raw= shdr_to_bigstring ~ehdr new_shdr } in
          let old_hunk = Bduff.insert ~name:(Fmt.strf "Section %2d" on) ~off:Int64.(add off (of_int len)) ~len
              { off= 0L; len; raw= shdr_to_bigstring ~ehdr shdr } in
          (old_hunk :: new_hunk :: acc, succ (succ rn), (succ on))
        else if rn = ehdr.e_shstrndx
        then (craft_shstrtab ~sh_offset:!shstrtab_sh_offset ~dst_off:off @ acc, succ rn, succ on)
        else if rn > n_shdr
        then
          let shdr = shift_shdr ~shift:sh_size shdr in
          let hunk = Bduff.insert ~name:(Fmt.strf "Section %2d" on) ~off ~len { off= 0L; len; raw= shdr_to_bigstring ~ehdr shdr } in
          (hunk :: acc, succ rn, succ on)
        else
          let hunk = Bduff.copy ~name:(Fmt.strf "Section %2d" on) ~src_off ~dst_off:off ~len:(Int64.of_int len) `Binary in
          (hunk :: acc, succ rn, succ on))
      ([], 0, 0) t.sht |> fun (lst, _, _) -> List.rev lst in
  let hunks = _hunk_ehdr :: _hunks_phdr @ _hunks_psct0s @ _new_psct @ _hunks_psct1s @ _pad_shdr @ _hunks_shdr in
  Ok hunks

module Us = Make(struct type 'a t = 'a end)
module U = Unix

module Unix = struct
  type 'fd v = { fd : 'fd; max : int64; }

  let unix =
    { bind= (fun x f -> f (Us.prj x))
    ; return= (fun x -> Us.inj x) }

  module Window = struct
    let src_window = Logs.Src.create ~doc:"window" "window"

    type t =
      { w : sub Weak.t
      ; mutable c : int }

    let make () =
      { w= Weak.create 0x100
      ; c= 0 }

    let max = 1024 * 1024
    let maxl = Int64.of_int max

    let heavy_load { fd; max; } cache ~pos ~len =
      let lenl = min Int64.(max - pos) maxl in

      if Int64.of_int len > lenl
      then Us.inj (Error (`Invalid_bounds { pos; len; }))
      else
        ( Logs.msg ~src:src_window Logs.Debug
            (fun m -> m "load %Lx byte(s) at %08Lx" lenl pos) ;
          let res = Mmap.V1.map_file fd ~pos Bigarray.char Bigarray.c_layout false [| Int64.to_int lenl |] in
          let res = { off= pos; len= Int64.to_int lenl; raw= Bigarray.array1_of_genarray res; } in
          ( Weak.set cache.w (cache.c land 0xff) (Some res) ; cache.c <- succ cache.c ; Us.inj (Ok res) ) )

    let load t cache ~pos ~len =
      let exception Found in
      let res = ref (Error (`Invalid_bounds { pos; len; })) in

      try
        for i = 0 to 0xff do
          match Weak.get cache.w i with
          | Some v ->
            if v.off <= pos && v.len - Int64.(to_int (pos - v.off)) >= len
            then ( Logs.msg ~src:src_window Logs.Info (fun m -> m "Window off:%8Lx, len:%x cached." pos len)
                 ; res := Ok v
                 ; raise Found )
          | None -> ()
        done ; heavy_load t cache ~pos ~len
      with Found -> Us.inj !res

    let get t cache ~pos = load t cache ~pos ~len:1
  end

  type 'fd t = { v : 'fd v; cache : Window.t; }

  let ro ~fpath =
    try
      let fd = Unix.openfile (Fpath.to_string fpath) Unix.[ O_RDONLY ] 0o644 in
      let max = let st = Unix.LargeFile.fstat fd in st.Unix.LargeFile.st_size in
      Us.inj (Ok { v= { fd; max; }; cache= Window.make (); })
    with _ -> Us.inj (Error (`Invalid_file fpath))

  let rwx ~fpath =
    try
      let fd = Unix.openfile (Fpath.to_string fpath) Unix.[ O_RDWR ] 0o744 in
      let max = let st = Unix.LargeFile.fstat fd in st.Unix.LargeFile.st_size in
      Us.inj (Ok { v= { fd; max; }; cache= Window.make (); })
    with _ -> Us.inj (Error (`Invalid_file fpath))

  let make ~fpath =
    let fd = Unix.openfile (Fpath.to_string fpath) Unix.[ O_RDWR; O_CREAT ] 0o644 in
    Us.inj (Ok fd)

  let close { v= { fd; _ }; _ } = Unix.close fd ; Us.inj (Ok ())

  let load t ~pos ~len = Window.load t.v t.cache ~pos ~len

  let get t ~pos =
    match Us.prj (Window.get t.v t.cache ~pos) with
    | Ok { off; raw; _ } ->
      let sub_off = Int64.(to_int (pos - off)) in
      Us.inj (Ok (Bigstringaf.get raw sub_off))
    | Error err -> Us.inj (Error err)

  external bytes_set_int64 : bytes -> int -> int64 -> unit = "%caml_bytes_set64"
  external swap64 : int64 -> int64 = "%bswap_int64"

  let bytes_set_int64_be =
    if Sys.big_endian
    then fun buf off v -> bytes_set_int64 buf off v
    else fun buf off v -> bytes_set_int64 buf off (swap64 v)

  let set_int64_be t ~pos v =
    let bytes = Bytes.create 8 in
    bytes_set_int64_be bytes 0 v ;
    try
      let _ = Unix.LargeFile.lseek t.v.fd pos Unix.SEEK_SET in
      let _ = Unix.write t.v.fd bytes 0 8 in Us.inj (Ok ())
    with _ -> Us.inj (Error (`Invalid_offset pos))

  let writev fd = function
    | Raw { raw; src_off; len; _ } ->
      let raw = Bigstringaf.substring raw ~off:src_off ~len in
      let len = Unix.write fd (Bytes.unsafe_of_string raw) 0 len in Us.inj len
    | Zero len ->
      let len = Int64.to_int len in
      let raw = Bytes.make len '\000' in
      let len = Unix.write fd raw 0 len in Us.inj len

  let writev_of_output output () = function
    | Raw { raw; src_off; len; _ } ->
      let raw = Bigstringaf.substring raw ~off:src_off ~len in
      output (Some (Bytes.unsafe_of_string raw, 0, len)) ; Us.inj len
    | Zero len ->
      let len = Int64.to_int len in
      let raw = Bytes.make len '\000' in
      output (Some (raw, 0, len)) ; Us.inj len
end

let pp_source ppf = function
  | `Binary -> Fmt.pf ppf "<binary>"
  | `Provision -> Fmt.pf ppf "<provision>"

let pp_error ~pp_source ppf = function
  | `Invalid_bounds { pos; len; } -> Fmt.pf ppf "Invalid bounds (pos: %Ld, len: %d)" pos len
  | `Invalid_file fpath -> Fmt.pf ppf "Invalid file: <%a>" Fpath.pp fpath
  | `Invalid_ELF -> Fmt.pf ppf "Invalid ELF"
  | `Phdr_not_found -> Fmt.pf ppf "Program header not found"
  | `Invalid_patch -> Fmt.pf ppf "Invalid patch"
  | `Not_found src -> Fmt.pf ppf "Source not found: %a" pp_source src
  | `Msg err -> Fmt.pf ppf "%s" err
  | `Unaligned_or_unexistant_psct_into _ -> Fmt.pf ppf "Did not find any valid physical section into program header"
  | `Invalid_offset pos -> Fmt.pf ppf "Invalid offset %8Lx" pos
  | `Multiple_occurence_of_provision -> Fmt.pf ppf "Multiple occurence of provision"
  | `No_occurence_of_provision -> Fmt.pf ppf "No occurence of provision"

let src_caravan = Logs.Src.create ~doc:"caravan" "caravan"

let ( >>= ) x f = match Us.prj x with
  | Ok v -> f v
  | Error err -> Us.inj (Error err)

let ( <* ) x f = match Us.prj x with
  | Error err -> Us.inj (Error err)
  | Ok v -> match Us.prj (f ()) with
    | Ok () -> Us.inj (Ok v)
    | Error err -> Us.inj (Error err)

let fiber0 fpath =
  Unix.ro ~fpath >>= fun src ->
  make Unix.unix ~load:Unix.load ~get:Unix.get ~fpath src >>= fun t ->
  let nobits_scts = nobits_scts t in
  let shdr, psct = List.hd nobits_scts in
  let bss, shifts, ssize, rsize = fix_nobits_sct t shdr psct in
  Fmt.pr "[%a] BSS section (offset: %08Lx, len: %Ld).\n%!" Fmt.(styled `Yellow string) "." (snd bss).sh_addr (snd bss).sh_size ;
  patch_of_fix_nobits t bss shifts (ssize, rsize) |> Unix.unix.return >>= fun bduff ->
  Fmt.epr "@[<hov>%a@]\n%!" Bduff.pp bduff ;
  Bos.OS.File.tmp "a.out-%s" |> Us.inj >>= fun fpath ->
  Bos.OS.File.with_output fpath
    (fun output fpath ->
       Logs.msg ~src:src_caravan Logs.Info (fun m -> m "Output a.out is %a" Fpath.pp fpath) ;
       let writev = Unix.writev_of_output output in
       let res = Bduff.apply Unix.unix ~sources:[ `Binary, src ] ~load:Unix.load ~writev () bduff |> Us.prj in
       Rresult.R.map (fun len -> (fpath, len)) res)
    fpath
  |> Rresult.R.join |> Us.inj <* fun () ->
      Fmt.pr "[%a] BSS section fixed.\n%!" Fmt.(styled `Green string) "x" ;
      Unix.close src

let fiber1 fpath fpath_provision =
  let name = Name.v ".provision" in

  Unix.ro ~fpath >>= fun src ->
  Bos.OS.Path.stat fpath_provision |> Unix.unix.return >>= fun stat ->
  Unix.ro ~fpath:fpath_provision >>= fun provision ->
  make Unix.unix ~load:Unix.load ~get:Unix.get ~fpath src >>= fun t ->
  craft_new_data_section t ~name (Int64.of_int stat.U.st_size) |> Us.inj >>= fun (phdr, n, shdr, psct) ->
  Fmt.pr "[%a] new <.provision> section (offset: %08Lx, len: %Ld).\n%!" Fmt.(styled `Yellow string) "." shdr.sh_offset shdr.sh_size ;
  inject_new_data_section t ~name (Int64.of_int stat.U.st_size) (phdr, n, shdr, psct) |> Unix.unix.return >>= fun bduff ->
  Fmt.epr "@[<hov>%a@]\n%!" Bduff.pp bduff ;
  Bos.OS.File.tmp "a.out-%s" |> Us.inj >>= fun fpath ->
  Bos.OS.File.with_output ~mode:0o744 fpath
    (fun output fpath ->
        Logs.msg ~src:src_caravan Logs.Info (fun m -> m "Output a.out is %a" Fpath.pp fpath) ;
        let writev = Unix.writev_of_output output in
        let res = Bduff.apply Unix.unix ~sources:[ `Binary, src; `Provision, provision ] ~load:Unix.load ~writev () bduff |> Us.prj in
        Rresult.R.map (fun len -> (fpath, len, (shdr.sh_addr, stat.U.st_size))) res)
    fpath
  |> Rresult.R.join |> Us.inj <* fun () ->
      Fmt.pr "[%a] <.provision> section added (virtual addr: %08Lx, len: %d).\n%!" Fmt.(styled `Green string) "x" shdr.sh_addr stat.U.st_size ;
      Unix.close src <* fun () -> Unix.close provision

let fiber2 fpath vaddr len =
  Unix.rwx ~fpath >>= fun src ->
  Bm.find_all ~pattern:"PROVISION" Unix.unix ~get:Unix.get ~ln:src.Unix.v.Unix.max src >>= function
  | [ occ ] ->
    Fmt.pr "[%a] provision occurence found at %08Lx.\n%!" Fmt.(styled `Yellow string) "." occ ;
    Unix.set_int64_be src ~pos:Int64.(occ + 10L) vaddr >>= fun () ->
    Unix.set_int64_be src ~pos:Int64.(occ + 18L) len >>= fun () ->
    Fmt.pr "[%a] provision value updated.\n%!" Fmt.(styled `Green string) "x" ;
    Unix.unix.return (Ok fpath)
  | [] -> Unix.unix.return (Error `No_occurence_of_provision)
  | _ -> Unix.unix.return (Error `Multiple_occurence_of_provision)

let setup style_renderer log_level cwd =
  Fmt_tty.setup_std_outputs ?style_renderer () ;
  Logs.set_level log_level ;
  Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ()) ;
  match cwd with
  | None -> `Ok ()
  | Some dir ->
    match Bos.OS.Dir.set_current dir with
    | Ok () -> `Ok ()
    | Error err -> `Error (false, Fmt.strf "%a" Rresult.R.pp_msg err)

let copy ~src ~dst =
  Bos.OS.File.with_output dst
  @@ fun output -> Bos.OS.File.with_input src
  @@ fun input ->
  let rec transmit () = match input () with
    | None -> output None
    | v -> output v ; transmit () in
  transmit

let run () a_out provision result =
  let fiber =
    fiber0 a_out
    >>= fun (a_out, _) -> copy ~src:a_out ~dst:(Fpath.v "a.out.bss") () |> Rresult.R.join |> Us.inj
    >>= fun () -> fiber1 a_out provision
    >>= fun (a_out, _, (vaddr, len)) -> fiber2 a_out vaddr (Int64.of_int len)
    >>= fun a_out ->
    let res = Bos.OS.Path.move ~force:true a_out result in
    Unix.unix.return res in
  Us.prj fiber |> function
  | Ok () ->
    Fmt.pr "[%a] output ELF binary <%a>.\n%!" Fmt.(styled `Green string) "x" Fpath.pp result ; `Ok ()
  | Error err ->
    Fmt.epr "[%a] %a.\n%!" Fmt.(styled `Red string) "ERROR" (pp_error ~pp_source) err ;
    `Error (false, Fmt.strf "%a" (pp_error ~pp_source) err)

open Cmdliner

let existing_file =
  let parser x = match Fpath.of_string x with
    | Ok v -> if Sys.file_exists x then Ok v else Rresult.R.error_msgf "<%a> does not exist" Fpath.pp v
    | Error _ as err -> err in
  let pp = Fpath.pp in
  Arg.conv ~docv:"<file>" (parser, pp)

let existing_directory =
  let parser x = match Fpath.of_string x with
    | Ok v -> if Sys.is_directory x then Ok v else Rresult.R.error_msgf "Expected directory <%a>" Fpath.pp v
    | Error _ as err -> err in
  let pp = Fpath.pp in
  Arg.conv ~docv:"<directory>" (parser, pp)

let output_file =
  let parser x = Fpath.of_string x in
  let pp = Fpath.pp in
  Arg.conv ~docv:"<file>" (parser, pp)

let elf_binary =
  let doc = "Input ELF binary file." in
  Arg.(required & opt (some existing_file) None & info [ "i"; "input" ] ~doc)

let provision =
  let doc = "Input provision file." in
  Arg.(required & opt (some existing_file) None & info [ "p"; "provision" ] ~doc)

let output_file =
  let doc = "Output ELF binary file." in
  Arg.(required & pos ~rev:true 0 (some output_file) None & info [] ~doc)

let setup =
  let style_renderer =
    let env = Arg.env_var "CARAVAN_COLOR" in
    Fmt_cli.style_renderer ~docs:Manpage.s_common_options ~env () in
  let log_level =
    let env = Arg.env_var "CARAVAN_VERBOSITY" in
    Logs_cli.level ~docs:Manpage.s_common_options ~env () in
  let cwd =
    let doc = "Change directory $(docv) before doing anything." in
    let docv = "DIR" in
    Arg.(value & opt (some existing_directory) None & info [ "C"; "cwd" ] ~docs:Manpage.s_common_options ~doc ~docv) in
  Term.(ret (const setup $ style_renderer $ log_level $ cwd))

let cmd =
  let doc = "$(tname) inject a new <.provision> section into the given ELF binary file." in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "Generate a new ELF binary file from a given one with a new <.provision> section filled with a given $(i,data) file." ] in
  Term.(const run $ setup $ elf_binary $ provision $ output_file),
  Term.info "caravan" ~version:"%%VERSION%%" ~doc ~exits ~man

let () = Term.(exit @@ eval cmd)
