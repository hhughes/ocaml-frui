class counter =
object (self)
  val counters = Hashtbl.create 10
  method add (name : string) = Hashtbl.add counters name (new Fvar.fvar 0)
  method get name = (Hashtbl.find counters name)#get
  method set name v = (Hashtbl.find counters name)#set v 
  method inc name = let v = self#get name in (*Logger.debug (Printf.sprintf "inc to %d" (v+1));*) self#set name (v+1)
  method behavior name = (Hashtbl.find counters name)#b
  method kvps = Hashtbl.fold (fun k v a -> (k,v#get) :: a) counters []
  method values = let kvps = self#kvps in snd (List.split kvps)
  method mem = Hashtbl.mem counters
end
