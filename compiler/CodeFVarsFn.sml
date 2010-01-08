(* code/code-fvars.sml *)

functor CodeFVarsFn(structure Util : UTIL
		    structure Code : CODE 
		      ) : CODE_FVARS =
  struct

    structure Code = Code

    fun bug s = Util.bug("CodeFVars."^s)

    datatype VarFrame	= FREE of Code.lvar list ref
			| BOUND of Code.lvar

	fun lvarEq(Code.LVAR{tag=tag1,...}, Code.LVAR{tag=tag2,...}) = tag1 = tag2

    fun frame_enter(frame, var : Code.lvar) =
      let fun loop([]) = frame := var::(!frame)
	    | loop(var'::vars) = if lvarEq(var,var') then () else loop vars
      in
		loop(!frame)
      end

    fun stack_enter(stack, var as Code.LVAR{tag,...}) =
      let fun loop([]) = bug("stack_enter: "^(Code.lvarString var)^" unbound?")
	    | loop((FREE frame)::stack) = (frame_enter(frame,var); loop stack)
	    | loop((BOUND var')::stack) = if lvarEq(var,var') then () else loop stack
      in
	loop stack
      end

    fun scan_value stack =
      let fun scan(Code.VAR(Code.GLOvar _)) = ()
	    | scan(Code.VAR(Code.LOCvar var)) = stack_enter(stack, var)
	    | scan(Code.LITERAL _) = ()
	    | scan(Code.OFFSET(v,_)) = scan v
	    | scan(Code.FETCH(v)) = scan v
	    | scan(Code.UNTAGPTR(v)) = scan v
	    | scan(Code.TAGPTR(v)) = scan v
	    | scan(Code.CALL(_,vs)) = List.app scan vs
      in
	   scan
      end

    fun bind(Code.GLOvar _, stack)	= stack
      | bind(Code.LOCvar var, stack)	= (BOUND var)::stack

    fun scan_code stack (Code.CODE{fvars,code,...}) =
      let val stack = (FREE fvars)::stack
	  val scanv = scan_value stack
      in
	case code
	  of Code.GOTO(Code.LOCALg _, _, _, _, _) => ()
	   | Code.GOTO(Code.EXTERNg _, _, _, _, _) => ()
	   | Code.GOTO(Code.VALUEg v, _, _, _, _) => scanv v
	   | Code.STORE(v1,v2,c)	=> (scanv v1; scanv v2; scan_code stack c)
	   | Code.BIND(NONE,v,c)	=> (scanv v; scan_code stack c)
	   | Code.BIND(SOME var,v,c)	=> (scanv v; scan_code (bind(var, stack)) c)
	   | Code.SWITCH(v,cases,def)	=>
		(scanv v; List.app (scan_case stack) cases; scanDefault stack def)
      end

    and scanDefault _ NONE = ()
      | scanDefault stack (SOME code) = scan_code stack code

    and scan_case stack (_,code) = scan_code stack code

    fun upd_labdef(Code.LABDEF{code,varHP,...}) = scan_code [BOUND varHP] code

    fun update(Code.MODULE{labdefs,...}) = List.app upd_labdef labdefs

  end (* functor CodeFVarsFn *)
