let second (a,b) = b

class ['a] fvar (v:'a) =
object (self)
  val mutable e = Froc.never
  val mutable s = second (Froc.make_event ())
  val mutable b = Froc.return v
  initializer
    begin
      let _e,_s = Froc.make_event () in
      let _b = Froc.hold v _e in
      e <- _e;
      s <- _s;
      b <- _b;
      Froc.notify_b b (fun v -> Logger.debug (Printf.sprintf "new value %d" v))
    end
  method e = e
  method s = s
  method b = b
  method get = Froc.sample b
  method set v = Froc.send s v
end
