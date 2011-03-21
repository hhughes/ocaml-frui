open Ocamljs.Inline

class logger =
object
  method log (msg : string) =  ignore(<:exp< console.log($msg$) >>)
end

let debug = (new logger)#log
