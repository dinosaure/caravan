module type PROVISION = sig end

module Make (_ : PROVISION) (Console : Mirage_console.S)
= struct
  let log console fmt = Fmt.kstr (Console.log console) fmt

  let start provision console =
    let len = Provision.length provision in
    let res = Bytes.create len in
    Provision.load_bytes provision ~src_off:0 res ~dst_off:0 ~len ;
    let res = Bytes.unsafe_to_string res in
    let map = Provision.map_bigstring provision ~off:0 ~len in
    assert (String.equal (Bigstringaf.to_string map) res) ;
    match Digestif.SHA1.of_hex_opt res with
    | Some hash -> log console "%a%!" Digestif.SHA1.pp hash
    | None -> log console ">>> Invalid provision %S (length: %d)%!" res len
end
