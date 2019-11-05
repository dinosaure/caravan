module type PROVISION = sig end

module Make (P : PROVISION) (Console : Mirage_types_lwt.CONSOLE)
= struct
  let log console fmt = Fmt.kstrf (Console.log console) fmt

  let start provision console =
    let len = Provision.length provision in
    let res = Bytes.create len in
    Provision.load_bytes provision ~src_off:0 res ~dst_off:0 ~len ;
    match Digestif.SHA1.of_hex_opt (Bytes.to_string res) with
    | Some hash -> log console "%a%!" Digestif.SHA1.pp hash
    | None -> log console ">>> Invalid provision %S (length: %d)%!" (Bytes.unsafe_to_string res) len
end
