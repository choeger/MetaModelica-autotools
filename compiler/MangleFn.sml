(* code/mangle.sml *)

functor MangleFn(structure Util : UTIL) : MANGLE =
  struct

    fun error s = Util.error("Mangle."^s)

    datatype name	= NAME of string

    fun letter_or_digit c =
      (c >= #"A" andalso c <= #"Z") orelse
      (c >= #"a" andalso c <= #"z") orelse
      (c >= #"0" andalso c <= #"9")

    fun to_hex i = String.sub("0123456789abcdef", i)

    fun encode s =
      let val siz = size s
	  fun e(indx, out) =
	    if indx >= siz then NAME(String.implode(rev out))
	    else
	      let val c = String.sub(s, indx)
		  val indx = indx+1
	      in
		if letter_or_digit c then e(indx, c::out)
		else if c = #"." then e(indx, #"_":: #"_"::out)
		else
		  let val c = Char.ord c
		      val hi = c div 16 and lo = c mod 16
		  in
		    e(indx, (to_hex lo)::(to_hex hi):: #"_"::out)
		  end
	      end
      in
	e(0, [])
      end

    fun from_hex c =
      let val c = Char.ord c
	  val c' = if c >= 97 then c - 87
		   else if c >= 65 then c - 55
		   else c - 48
      in
	if c' >= 0 andalso c' < 16 then c'
	else error("from_hex: bad xdigit: "^(String.str(Char.chr c)))
      end

    fun decode s =
      let fun d([], out)	= String.implode(rev out)
	    | d(#"_"::cs, out)	=
		(case cs
		   of #"_"::cs	=> d(cs, #"."::out)
		    | hi::lo::cs=>
			let val hi = from_hex hi
			    val lo = from_hex lo
			    val c = hi * 16 + lo
			in
			  d(cs, Char.chr c :: out)
			end
		    | _		=> error("decode: bad name: "^s))
	    | d(c::cs, out)	= d(cs, c::out)
      in
	d(String.explode s, [])
      end

  end (* functor MangleFn *)
