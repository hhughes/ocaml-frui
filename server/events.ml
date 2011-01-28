open Printf

let count = ref 20

let next_count () =
  let c = !count in
  count := !count + 1; c

let _create_msg () = sprintf
"{
  tid: 0,
  ty: ['msg'],
  ts: %d,
  name: 'some name',
  desc: 'message1',
  misc: {}
}" (next_count ())

let create_msg () = sprintf "[%s]" (_create_msg ())
