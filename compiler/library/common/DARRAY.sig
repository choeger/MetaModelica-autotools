(* DArray:>DARRAY implements a lightweight dynamic int array refof integers.
   The default element is 0.
   *)
signature DARRAY=
sig
   val array:int->int array ref(* creates a new int array refwith the suggested size. *)
   val sub:int array ref *int->int 
   val update:int array ref *int*int->unit
   val clear:int array ref-> unit
   val bound:int array ref-> int
   val toList:int array ref-> int list
   val fromList:int list -> int array ref  
end

