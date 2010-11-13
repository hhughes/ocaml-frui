module D = Dom

let onload () =
  let body = D.document#getElementById "main" in
  let text = (D.document#createTextNode "hw_text" : D.text) in
  text#_set_data "Hello World!";
  ignore (body#appendChild text)
;;

D.window#_set_onload onload
