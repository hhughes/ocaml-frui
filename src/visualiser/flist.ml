class ['a] flist =
object (self)
  val mutable first = []
  val mutable last = []
  val mutable fs = []
  method lift (f : 'a -> unit) = 
    begin
      let l o = ignore (Froc.lift f o) in
      fs <- f :: fs;
      List.iter l first;
      List.iter l (List.rev last)
    end
  method lift_all o =
    begin
      let l f = ignore (Froc.lift f o) in
      List.iter l fs
    end
  method list = List.rev_append (List.rev first) (List.rev last)
  method push o = self#lift_all o; first <- o :: first
  method push_end o = self#lift_all o; last <- o :: last
  method pop = let hd = List.hd first in first <- List.tl first; hd
  method pop_end = let hd = List.hd last in last <- List.tl last; hd
end
