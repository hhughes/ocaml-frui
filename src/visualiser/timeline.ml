class timeline (e : Dom.element) t0 t1 =
object (self)
  method init =
    Spinner.create e t0;
    Spinner.create e t1
  initializer self#init
end

let init (e : Dom.element) t0 t1 =
  let t = new timeline e t0 t1 in
  t
