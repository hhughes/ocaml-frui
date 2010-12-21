module D = Dom

let onload () =
  let main = (D.document#getElementById "main" : D.element) in
  let text = (D.document#createTextNode "hw_text" : D.text) in
  text#_set_data "Hello World!";
  ignore (main#appendChild text)
;;

D.window#_set_onload onload
