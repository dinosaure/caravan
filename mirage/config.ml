open Mirage

type provision = Provision
let provision = Functoria.Type Provision

let filled =
  foreign "Unikernel.Make"
    (provision @-> console @-> job)

let packages =
  [ package "provision"
  ; package "fmt"
  ; package ~sublibs:["c"] "digestif" ]

let provision =
  let open Functoria in
  let open Functoria_app in
  impl @@ object
    inherit base_configurable

    method ty = provision
    val name = Name.create ~prefix:"caravan" "prvs"
    method name = name
    method module_name = String.capitalize_ascii name
    method! packages =
      Key.pure [ package "provision" ]
    method! connect _ modname _ =
      Fmt.str "return (%s.provision)" modname
    method! clean _info =
      Bos.OS.File.delete Fpath.(v name + "ml")
    method! build _info =
      let contents = Fmt.str "let provision = Provision.unsafe_of_string \
                              \"PROVISION_\\000\\000\\000\\000\\000\\000\\000\\000\\000\\000\\000\\000\\000\\000\\000\\000\"" in
      let output_string oc v = output_string oc v ; Ok () in
      let res = Bos.OS.File.with_oc Fpath.(v name + "ml") output_string contents in
      Rresult.R.join res
  end

let () =
  register "filled"
    ~packages [ filled $ provision $ default_console ]
