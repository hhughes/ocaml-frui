let h = Hashtbl.create 10

let _ = Hashtbl.replace h 0 1
let one = Hashtbl.find h 0

let f a b = a+b
let three = f one 2;;
