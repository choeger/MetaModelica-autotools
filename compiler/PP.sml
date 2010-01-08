(* util/pp.sml
 * A pretty-printing engine based on Oppen's MESA pretty-printing code
 * published in TOPLAS 2(4).
 * Converted to SML by Mikael Pettersson
 *)

structure PP : PP =
  struct

    (*
     * error handling
     *)

    exception PP
    fun error msg =
      (TextIO.output(TextIO.stdErr, "PP."^msg^"\n");
       raise PP)

    (*
     * datatypes
     *)

    datatype breaks	= CONSISTENT | INCONSISTENT
    datatype token	= STRING of string
			| BREAK of
			    {blankSpace	: int,	(* number of spaces per blank *)
			     offset	: int}	(* indent for overflow lines *)
			| BEGIN of
			    {offset	: int,	(* indent for this group *)
			     breakType	: breaks}
			| END

    datatype print_break = FITSpb | INCONSISTENTpb | CONSISTENTpb
    datatype print_entry = ENTRY of {offset: int, break: print_break}

    (*
     * constants
     *)

    val lineBreak = BREAK{blankSpace=200, offset=0}
    val sizeInfinity = 100000000	(* 100 million *)

    (*
     * the collective state of a PP stream
     *)

    datatype state	= STATE of {
				(* PRINT state *)
				printOut	: TextIO.outstream,
				printMargin	: int,
				printSpace	: int ref,
				printStack	: print_entry list ref,
				(* STREAM state *)
				streamLeft	: int ref,
				streamRight	: int ref,
				streamTokens	: token Array.array,
				streamSizes	: int Array.array,
				streamLeftTot	: int ref,
				streamRightTot	: int ref,
				(* SCAN state *)
				scanStack	: int Array.array,
				scanStackEmpty	: bool ref,
				scanStackTop	: int ref,
				scanStackBot	: int ref	}

    (*
     * the PRINT process
     *)

    fun indent(st as STATE{printOut,...}, amount) =
      if amount <= 0 then ()
      else (TextIO.output1(printOut, #" "); indent(st, amount-1))

    fun newLineIndent(st as STATE{printOut,...}, amount) =
      let val _ = TextIO.output1(printOut, #"\n")
	  fun tabto amount =
	    if amount < 8 then indent(st, amount)
	    else (TextIO.output1(printOut, #"\t"); tabto(amount-8))
      in
	tabto amount
      end

    fun printStackPush(STATE{printStack,...}, pse) =
      printStack := pse :: (!printStack)

    fun printStackPop(STATE{printStack,...}) =
      case !printStack
	of (_::stk)	=> printStack := stk
	 | []		=> error "printStackPop"

    fun printStackTop(STATE{printStack,...}) =
      case !printStack
	of (x::_)	=> x
	 | []		=> error "printStackTop"

    fun printToken(st as STATE{printSpace,printMargin,printOut,...}, tkn, len) =
      case tkn
	of BEGIN{offset,breakType}	=>
	    if len > !printSpace then
	      let val break = case breakType
				of CONSISTENT	=> CONSISTENTpb
				 | INCONSISTENT	=> INCONSISTENTpb
	      in
		printStackPush(st, ENTRY{offset = !printSpace-offset, break = break})
	      end
	    else printStackPush(st, ENTRY{offset = 0, break = FITSpb})
	 | END	=> printStackPop(st)
	 | BREAK{blankSpace,offset}	=>
	    let val ENTRY{offset=offset', break} = printStackTop(st)
	    in
	      case break
		of FITSpb		=>
		    (printSpace := !printSpace - blankSpace;
		     indent(st, blankSpace))
		 | CONSISTENTpb	=>
		    (printSpace := offset' - offset;
		     newLineIndent(st, printMargin - !printSpace))
		 | INCONSISTENTpb	=>
		    if len > !printSpace then
		      (printSpace := offset' - offset;
		       newLineIndent(st, printMargin - !printSpace))
		    else
		      (printSpace := !printSpace - blankSpace;
		       indent(st, blankSpace))
	    end
	 | STRING str => (printSpace := !printSpace - len; TextIO.output(printOut, str))

    (*
     * Token & Size streams management
     *)

    fun streamReset(STATE{streamLeft,streamRight,streamLeftTot,streamRightTot,...}) =
      (streamLeft := 0; streamRight := 0; streamLeftTot := 1; streamRightTot := 1)

    fun streamAdvanceRight(STATE{streamRight,streamLeft,streamTokens,...}) =
      (streamRight := (!streamRight + 1) mod Array.length(streamTokens);
       if !streamRight = !streamLeft then error "streamAdvanceRight" else ())

    fun streamSetRight(STATE{streamTokens,streamSizes,streamRight,...}, tkn, siz) =
      (Array.update(streamTokens, !streamRight, tkn);
       Array.update(streamSizes, !streamRight, siz))

    fun streamAdvanceLeft(st as STATE{streamTokens,streamSizes,streamLeft,
				      streamRight,streamLeftTot,...}) =
      let val tkn = Array.sub(streamTokens, !streamLeft)
	  and siz = Array.sub(streamSizes, !streamLeft)
      in
	if siz >= 0 then
	  (printToken(st, tkn, siz);
	   (case tkn
	      of BREAK{blankSpace,...}	=> streamLeftTot := !streamLeftTot+blankSpace
	       | STRING _		=> streamLeftTot := !streamLeftTot+siz
	       | _			=> ());
	   if !streamLeft <> !streamRight then
	     (streamLeft := (!streamLeft + 1) mod Array.length(streamTokens);
	      streamAdvanceLeft(st))
	   else ())
	else ()
      end

    (*
     * Scan "stack" management
     *)

    fun scanStackPush(STATE{scanStackEmpty,scanStackTop,scanStackBot,scanStack,...},
		      x) =
      (if !scanStackEmpty then scanStackEmpty := false
       else
	 (scanStackTop := (!scanStackTop + 1) mod Array.length(scanStack);
	  if !scanStackTop = !scanStackBot then error "scanStackPush" else ());
       Array.update(scanStack, !scanStackTop, x))

    fun scanStackTop(STATE{scanStackEmpty,scanStack,scanStackTop,...}) =
      if !scanStackEmpty then error "scanStackTop"
      else Array.sub(scanStack, !scanStackTop)

    fun scanStackPop(STATE{scanStackEmpty,scanStack,scanStackTop,scanStackBot,...}) =
      if !scanStackEmpty then error "scanStackPop"
      else
	let val x = Array.sub(scanStack, !scanStackTop)
	in
	  if !scanStackTop = !scanStackBot then scanStackEmpty := true
	  else scanStackTop := (!scanStackTop + Array.length(scanStack) - 1)
			       mod Array.length(scanStack);
	  x
	end

    fun scanStackPopBottom(STATE{scanStackEmpty,scanStack,scanStackBot,
				 scanStackTop,...}) =
      if !scanStackEmpty then error "scanStackPopBottom"
      else
	let val x = Array.sub(scanStack, !scanStackBot)
	in
	  if !scanStackTop = !scanStackBot then scanStackEmpty := true
	  else scanStackBot := (!scanStackBot + 1) mod Array.length(scanStack);
	  x
	end

    (*
     * Stuff that manipulates both the streams and the scan stack
     *)

    fun checkStack(st as STATE{scanStackEmpty,streamTokens,streamSizes,
			       streamRightTot,...}, k) =
      if not (!scanStackEmpty) then
	let val x = scanStackTop(st)
	in
	  case Array.sub(streamTokens, x)
	    of BEGIN _	=>
		if k > 0 then
		  (Array.update(streamSizes, scanStackPop(st),
				Array.sub(streamSizes, x) + !streamRightTot);
		   checkStack(st, k-1))
		else ()
	     | END	=>
		(Array.update(streamSizes, scanStackPop(st), 1); checkStack(st, k+1))
	     | _	=>
		(Array.update(streamSizes, scanStackPop(st),
			      Array.sub(streamSizes, x) + !streamRightTot);
		 if k > 0 then checkStack(st, k) else ())
	end
      else ()

    fun checkStream(st as STATE{streamRightTot,streamLeftTot,streamSizes,streamRight,
				streamLeft,scanStackEmpty,scanStack,scanStackBot,
				printSpace,...}) =
      if !streamRightTot - !streamLeftTot > !printSpace then
	(if not (!scanStackEmpty) then
	   if !streamLeft = Array.sub(scanStack, !scanStackBot) then
	     Array.update(streamSizes, scanStackPopBottom(st), sizeInfinity)
	   else ()
	 else ();
	 streamAdvanceLeft(st);
	 if !streamLeft <> !streamRight then checkStream(st) else ())
      else ()

    fun emit(st as STATE{scanStackEmpty,streamRightTot,streamRight,...}, tkn) =
      case tkn
	of BEGIN _	=>
	    (if !scanStackEmpty then streamReset(st)
	     else streamAdvanceRight(st);
	     streamSetRight(st, tkn, ~(!streamRightTot));
	     scanStackPush(st, !streamRight))
	 | END	=>
	    if !scanStackEmpty then printToken(st, tkn, 0)
	    else
	      (streamAdvanceRight(st);
	       streamSetRight(st, tkn, ~1);
	       scanStackPush(st, !streamRight))
	 | BREAK{blankSpace,...}	=>
	    (if !scanStackEmpty then streamReset(st)
	     else streamAdvanceRight(st);
	     checkStack(st, 0);	(* cannot be moved before streamReset(st) *)
	     streamSetRight(st, tkn, ~(!streamRightTot));
	     scanStackPush(st, !streamRight);
	     streamRightTot := !streamRightTot + blankSpace)
	 | STRING str	=>
	    let val siz = String.size(str)
	    in
	      if !scanStackEmpty then printToken(st, tkn, siz)
	      else
		(streamAdvanceRight(st);
		 streamSetRight(st, tkn, siz);
		 streamRightTot := !streamRightTot + siz;
		 checkStream(st))
	    end

    fun close(st as STATE{scanStackEmpty,...}) =
      if !scanStackEmpty then ()
      else (checkStack(st, 0); streamAdvanceLeft(st))

    fun init(os, lineWidth) =
      let val n = 3 * lineWidth
      in
	STATE{	printOut	= os,
		printMargin	= lineWidth,
		printSpace	= ref lineWidth,
		printStack	= ref [],
		streamLeft	= ref 0,
		streamRight	= ref 0,
		streamTokens	= Array.array(n, END),
		streamSizes	= Array.array(n, 0),
		streamLeftTot	= ref 1,
		streamRightTot	= ref 1,
		scanStack	= Array.array(n, 0),
		scanStackEmpty	= ref true,
		scanStackTop	= ref 0,
		scanStackBot	= ref 0		}
      end

  end (* structure PP *)

(* test:
 *	begin
 *	  stmt1
 *	  stmt2
 *	end
 *)

(*val test =
  [PP.BEGIN{breakType=PP.CONSISTENT, offset=0},
   PP.BEGIN{breakType=PP.CONSISTENT, offset=2},
   PP.STRING "begin",
   PP.lineBreak,
   PP.STRING "stmt1;",
   PP.lineBreak,
   PP.STRING "stmt2;",
   PP.END,
   PP.lineBreak,
   PP.STRING "end;",
   PP.lineBreak,
   PP.END]	*)
