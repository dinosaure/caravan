let p =
  Provision.unsafe_of_string
    "PROVISION_\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let () =
  let len = Provision.length p in
  let buf = Bytes.create len in
  Provision.load_bytes p ~src_off:0 buf ~dst_off:0 ~len ;
  Fmt.pr "@[<hov>%a@]\n%!" (Hxd_string.pp Hxd.O.default) (Bytes.unsafe_to_string buf)
