let p =
  Provision.unsafe_of_string
    "PROVISION_\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let () =
  let len = Provision.length p in
  let buf = Bytes.create len in
  Provision.load_bytes p ~src_off:0 buf ~dst_off:0 ~len ;
  let map = Provision.map_bigstring p ~off:0 ~len in
  let res = Bytes.unsafe_to_string buf in
  assert (String.equal (Bigstringaf.to_string map) res) ;
  Fmt.pr "@[<hov>%a@]\n%!" (Hxd_string.pp Hxd.default) res
