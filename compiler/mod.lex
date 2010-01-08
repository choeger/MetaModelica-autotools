(*******************************************************************************
 * @author Adrian Pop [adrpo@ida.liu.se, http://www.ida.liu.se/~adrpo]
 * Copyright (c) 2002-2006, Adrian Pop [adrpo@ida.liu.se],
 * Programming Environments Laboratory (PELAB),
 * Department of Computer and Information Science (IDA), 
 * Linkopings University (LiU). 
 * All rights reserved.
 *
 * http://www.ida.liu.se/~adrpo/license/
 *
 * NON-COMMERCIAL terms and conditions [NON-COMMERCIAL setting]:
 * Permission to use, copy, modify, and distribute this software and
 * its documentation in source or binary form (including products 
 * developed or generated using this software) for NON-COMMERCIAL 
 * purposes and without fee is hereby granted, provided that this 
 * copyright notice appear in all copies and that both the copyright 
 * notice and this permission notice and warranty disclaimer appear 
 * in supporting documentation, and that the name of The Author is not 
 * to be used in advertising or publicity pertaining to distribution 
 * of the software without specific, prior written permission.
 * 
 * COMMERCIAL terms and conditions [COMMERCIAL setting]:
 * COMMERCIAL use, copy, modification and distribution in source 
 * or binary form (including products developed or generated using
 * this software) is NOT permitted without prior written agreement 
 * from Adrian Pop [adrpo@ida.liu.se].
 * 
 * THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
 * OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
 * OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE
 * USE OR PERFORMANCE OF THIS SOFTWARE.
 *****************************************************************************)
(* lexer/mo.lex -- ML-Lex lexical specification for Modelica+ *)

(* parameters for ML-Yacc *)
type arg = LexArg.lexarg
type pos = Cache.poz
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue, pos) token

fun mkPos (pos, lexarg) = 
	(pos, 
	 !(LexArg.currLine lexarg), 
	  (LexArg.fixColumn (lexarg,pos); !(LexArg.currColumn lexarg)))

fun eof(lexarg) =
  let val pos        = LexArg.readPos  lexarg
	  val newPos     = mkPos(pos, lexarg)
  in
    if !(LexArg.eofFlag lexarg) 
    then 
		Tokens.EOF_HARD(newPos, newPos)
    else
      let val pos1 = !(LexArg.leftPos lexarg)
	  val _ = LexArg.eofFlag lexarg := true
      in
		if !(LexArg.comLev lexarg) > 0 then
		LexArg.error2 lexarg ("unterminated comment at EOF", pos1, newPos)
		else if !(LexArg.strList lexarg) <> [] then
		LexArg.error2 lexarg ("unterminated string at EOF", pos1, newPos)
		else ();
		Tokens.EOF_SOFT(newPos, newPos)
      end
  end
  
  
fun makeDECINT(pos1, str, lexarg) =
  let val pos2 = pos1 + String.size str (* - 1 *)
  in
    Tokens.ICON(LexUtil.decint str, mkPos(pos1, lexarg), mkPos(pos2, lexarg))
  end

fun makeHEXINT(pos1, str, lexarg) =
  let val pos2 = pos1 + String.size str (* - 1 *)
  in
    Tokens.ICON(LexUtil.hexint str, mkPos(pos1, lexarg), mkPos(pos2, lexarg))
  end

fun makeRCON(pos1, str, lexarg) =
  let val pos2 = pos1 + String.size str (* - 1 *)
  in
    Tokens.RCON(LexUtil.rcon str, mkPos(pos1, lexarg), mkPos(pos2, lexarg))
  end

(* The regular expression for a string literal used to be:
 *	white=[\ \t\n\013\012];
 *	gap=\\{white}+\\;
 *	sitem={cdesc}|{gap};
 *	scon=\"{sitem}*\";
 * but due to error handling and position reporting reasons,
 * we now scan strings piecemeal using several lex start states.
 *)
fun addString(lexarg, yytext) =
  let val strList = LexArg.strList lexarg
  in
    strList := yytext :: !strList
  end

fun errBadStr(lexarg, pos, text) =
  let val msg = "illegal character in string " ^ MakeString.scvt text
  in
    LexArg.error2 lexarg (msg, mkPos(pos, lexarg), mkPos(pos, lexarg))
  end

fun makeSCON(lexarg, pos2) =
  let val strList = LexArg.strList lexarg
      val str = String.concat(List.rev("\"" :: !strList))
      val _ = strList := []
  in
    Tokens.SCON(LexUtil.scon str, !(LexArg.leftPos lexarg), mkPos(pos2, lexarg))
  end

val kwds = [
	    ("and",	Tokens.AND),
	    ("as",	Tokens.AS),
	    ("end",	Tokens.END),
	    ("not",	Tokens.NOT),
	    ("of",	Tokens.OF),
	    ("type",	Tokens.TYPE),	    
	    (* adrpo added MODELICA keywords *) 
		("algorithm", Tokens.ALGORITHM),
		("annotation", Tokens.ANNOTATION),
		("block", Tokens.BLOCK),
		("boundary", Tokens.BOUNDARY),
		(* not needed ("Code", Tokens.CODE), *)
		("class", Tokens.CLASS),
		("connect", Tokens.CONNECT),
		("connector", Tokens.CONNECTOR),
		("constant", Tokens.CONSTANT),
		("discrete", Tokens.DISCRETE),
		("each", Tokens.EACH),
		("else", Tokens.ELSE),
		("elseif", Tokens.ELSEIF),
		("elsewhen", Tokens.ELSEWHEN),
		("enumeration", Tokens.ENUMERATION),
		("equation", Tokens.EQUATION),
		("encapsulated", Tokens.ENCAPSULATED),
		("extends", Tokens.EXTENDS),
		("external", Tokens.EXTERNAL),
		("false", Tokens.FALSE), 
		("final", Tokens.FINAL),
		("flow", Tokens.FLOW),
		("for", Tokens.FOR),
		("function", Tokens.FUNCTION),
		("if", Tokens.IF),
		("import", Tokens.IMPORT),
		("in", Tokens.IN),
		("inner", Tokens.INNER),
		("input", Tokens.INPUT),
		("loop", Tokens.LOOP),
		("model", Tokens.MODEL),
		("outer", Tokens.OUTER),
		(*
		("overload", Tokens.OVERLOAD),
		*)
		("or", Tokens.OR),
		("output", Tokens.OUTPUT),
		("package", Tokens.PACKAGE),
		("parameter", Tokens.PARAMETER),
		("partial", Tokens.PARTIAL),
		("protected", Tokens.PROTECTED),
		("public", Tokens.PUBLIC),
		("record", Tokens.RECORD),
		("failure", Tokens.FAILURE),
		("equality", Tokens.EQUALITY),		
		("tuple", Tokens.TUPLE),		
		("redeclare", Tokens.REDECLARE),
		("replaceable", Tokens.REPLACEABLE),
		("results", Tokens.RESULTS),
		("then", Tokens.THEN),
		("true", Tokens.TRUE),
		("when", Tokens.WHEN),
		("while", Tokens.WHILE),
		("within", Tokens.WITHIN),
		(* new modelica+ constructs *)
		("uniontype", Tokens.UNIONTYPE),
		("match", Tokens.MATCH),
		("matchcontinue", Tokens.MATCHCONTINUE),		
		("case", Tokens.CASE),
		("local", Tokens.LOCAL),
		("list", Tokens.LIST),
		("fail", Tokens.FAIL),
		("subtypeof", Tokens.SUBTYPEOF)				
		]

fun makeIDENT(pos1, str, lexarg) =
	let val pos2 = pos1 + String.size str (* - 1 *)
		fun loop [] = Tokens.IDENT(str, mkPos(pos1, lexarg), mkPos(pos2, lexarg))
		| loop((str',mktok)::rest) =
			if str=str' 
			then mktok(mkPos(pos1, lexarg),mkPos(pos2, lexarg)) 
			else loop rest
	in				
		loop kwds
	end

fun inc(ri) = ri := !ri + 1
fun dec(ri) = let val j = !ri - 1 in ri := j; j end

(* Not used in MetaModelica

fun makeCCON(pos1, str, lexarg) =
  let val pos2 = pos1 + String.size str (* - 1 *)
  in
    Tokens.CCON(LexUtil.ccon str, mkPos(pos1, lexarg), mkPos(pos2, lexarg))
  end

ccon="#\""{cdesc}\";
tyvar=tyVarStr+{alpha}{alnum}*;

<INITIAL>{ccon}		=>
	(makeCCON(yypos, yytext, lexarg));

*)

%%

%header (functor MODLexFn(
			  structure Tokens : MOD_TOKENS
			  structure Cache: CACHE
			  structure LexArg : LEXARG where type poz = Cache.poz
			  structure MakeString : MAKESTRING
			  structure LexUtil : LEXUTIL
			  sharing type Cache.visibility = LexArg.visibility
			  ) : ARG_LEXER);
			  
%S STRING GAP COMMENT COMMENTLINE;
%arg (lexarg);
%full

white=[\ \013\012];
eol="\010";
tab=\t;

ddigit=[0-9];
decint={ddigit}+;
xdigit=[0-9a-fA-F];
hexint="0x"{xdigit}+;

fraction="."{ddigit}+;
exponent=[eE][+-]?{decint};
rcon={decint}({fraction}{exponent}?|{exponent});

echar=[\\"nrtfabv];
cntrl=[?-_];
escseq={ddigit}{3}|"^"{cntrl}|{echar};
pchar=[\ -!#-[^-~\128-\255]|"]";
cdesc={pchar}|\\{escseq}|"\\";

alpha=[A-Za-z];
alnum={alpha}|[_0-9];
id={alpha}{alnum}*;

%%

<INITIAL>{eol}		=>
	(LexArg.newLine(lexarg, mkPos(yypos, lexarg));
	 continue());
<INITIAL>{tab}		=>
	(LexArg.newTab(lexarg, yygone, mkPos(yypos, lexarg));
	 continue());
<INITIAL>{white}	=>
	(continue());
<INITIAL>\"		=>
	(LexArg.strList lexarg := [yytext];
	 LexArg.leftPos lexarg := mkPos(yypos, lexarg);
	 YYBEGIN STRING;
	 continue());
<INITIAL>"(*"|"/*"		=>
	(LexArg.comLev lexarg := 1;
	 LexArg.leftPos lexarg := mkPos(yypos, lexarg);
	 YYBEGIN COMMENT;
	 continue());
<INITIAL>"//"		=>
	(LexArg.comLev lexarg := 1;
	 LexArg.leftPos lexarg := mkPos(yypos, lexarg);
	 YYBEGIN COMMENTLINE;
	 continue());
<INITIAL>"("		=>
	(Tokens.LPAREN(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>")"		=>
	(Tokens.RPAREN(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>"*)"|"*/"		=>
	(LexArg.error2 lexarg ("unmatched close comment",
		mkPos(yypos, lexarg),
		mkPos(yypos+1, lexarg)); continue());
<INITIAL>"*"		=>
	(Tokens.STAR(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>","		=>
	(Tokens.COMMA(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>"."		=>
	(Tokens.DOT(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>"::"		=>
	(Tokens.COLONCOLON(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg)));
<INITIAL>":"		=>
	(Tokens.COLON(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>";"		=>
	(Tokens.SEMICOLON(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>"{"		=>
	(Tokens.LBRACE(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>"}"		=>
	(Tokens.RBRACE(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>":="		=>
	(Tokens.ASSIGN(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg)));			
<INITIAL>"="		=>
	(Tokens.EQ(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>"["		=>
	(Tokens.LBRACK(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>"]"		=>
	(Tokens.RBRACK(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>"_"		=>
	(Tokens.WILD(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>{decint}	=>
	(makeDECINT(yypos, yytext, lexarg));
<INITIAL>{hexint}	=>
	(makeHEXINT(yypos, yytext, lexarg));
<INITIAL>{rcon}		=>
	(makeRCON(yypos, yytext, lexarg));
<INITIAL>{id}		=>
	(makeIDENT(yypos, yytext, lexarg));
		
<INITIAL>"+"	=> 
	(Tokens.ADD_INT(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>"-"	=>
	(Tokens.SUB_INT(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>"~"	=>
	(Tokens.NEG_INT(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));	
<INITIAL>"/"	=>
	(Tokens.DIV_INT(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>"%"	=>
	(Tokens.MOD_INT(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>"=="	=>
	(Tokens.EQEQ_INT(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg)));
<INITIAL>">="	=>
	(Tokens.GE_INT(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg)));
<INITIAL>">"	=>
	(Tokens.GT_INT(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>"<="	=>
	(Tokens.LE_INT(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg)));
<INITIAL>"<"	=>
	(Tokens.LT_INT(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>"!="	=>
	(Tokens.NE_INT(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg)));
<INITIAL>"<>"	=>
	(Tokens.NE_INT(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg)));

<INITIAL>"+."	=>
	(Tokens.ADD_REAL(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg)));
<INITIAL>"-."	=>
	(Tokens.SUB_REAL(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg)));
<INITIAL>"~."	=>
	(Tokens.NEG_REAL(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg)));	
<INITIAL>"*."	=>
	(Tokens.MUL_REAL(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg)));
<INITIAL>"/."	=>
	(Tokens.DIV_REAL(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg))); 
<INITIAL>"%."	=>
	(Tokens.MOD_REAL(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg)));
<INITIAL>"^."	=>
	(Tokens.POW_REAL(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg)));		
<INITIAL>"==."	=>
	(Tokens.EQEQ_REAL(mkPos(yypos, lexarg), mkPos(yypos+2, lexarg)));
<INITIAL>">=."	=>
	(Tokens.GE_REAL(mkPos(yypos, lexarg), mkPos(yypos+2, lexarg)));
<INITIAL>">."	=>
	(Tokens.GT_REAL(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg)));
<INITIAL>"<=."	=>
	(Tokens.LE_REAL(mkPos(yypos, lexarg), mkPos(yypos+2, lexarg)));
<INITIAL>"<."	=>
	(Tokens.LT_REAL(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg)));
<INITIAL>"!=."	=>
	(Tokens.NE_REAL(mkPos(yypos, lexarg), mkPos(yypos+2, lexarg)));
<INITIAL>"<>."	=>
	(Tokens.NE_REAL(mkPos(yypos, lexarg), mkPos(yypos+2, lexarg)));

<INITIAL>"+&"	=>
	(Tokens.ADD_STRING(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg)));
<INITIAL>"==&"	=>
	(Tokens.EQEQ_STRING(mkPos(yypos, lexarg), mkPos(yypos+2, lexarg)));
	
<INITIAL>"@"	=>
	(Tokens.ADD_LIST(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));

<INITIAL>"^"	=>
	(Tokens.POWER(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));

<INITIAL>".*"		=>
	(Tokens.DOTSTAR(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg)));

		
<INITIAL>.		=>
	(let val msg = "illegal character " ^ MakeString.scvt yytext
	 in
	   LexArg.error2 lexarg (msg,mkPos(yypos, lexarg),mkPos(yypos, lexarg));
	   continue()
	 end);

<STRING>\"		=>
	(YYBEGIN INITIAL; makeSCON(lexarg, yypos));
<STRING>\\{eol}		=>
	(LexArg.newLine(lexarg, mkPos(yypos+1, lexarg));
	 YYBEGIN GAP; continue());
<STRING>\\{tab}		=>
	(LexArg.newTab(lexarg, yygone, mkPos(yypos+1, lexarg));
	 YYBEGIN GAP; continue());
<STRING>\\{white}	=>
	(YYBEGIN GAP; continue());
<STRING>{cdesc}		=>
	(addString(lexarg, yytext); continue());
<STRING>{eol}		=>
	(LexArg.newLine(lexarg, mkPos(yypos, lexarg));
	 addString(lexarg, yytext); continue());
<STRING>"\013"|"\012"	=>
	(continue());
<STRING>{tab}		=>
	(LexArg.newTab(lexarg, yygone, mkPos(yypos, lexarg));
	 addString(lexarg, yytext); continue());
<STRING>.		=>
	(errBadStr(lexarg, yypos, yytext); continue());

<GAP>\\			=>
	(YYBEGIN STRING; continue());
<GAP>{eol}		=>
	(LexArg.newLine(lexarg, mkPos(yypos, lexarg));
	 continue());
<GAP>{tab}		=>
	(LexArg.newTab(lexarg, yygone, mkPos(yypos, lexarg));
	 continue());
<GAP>{white}		=>
	(continue());
<GAP>.			=>
	(let val msg = "illegal character in string gap " ^ MakeString.scvt yytext
	 in
	   LexArg.error2 lexarg (msg,mkPos(yypos, lexarg),mkPos(yypos, lexarg));
	   continue()
	 end);

<COMMENT>"(*"|"/*"		=>
	(inc(LexArg.comLev lexarg); continue());
<COMMENT>"*)"|"*/"		=>
	(if dec(LexArg.comLev lexarg) > 0 then () else YYBEGIN INITIAL; continue());
<COMMENT>{eol}		=>
	(LexArg.newLine(lexarg, mkPos(yypos, lexarg));
	 continue());
<COMMENT>{tab}		=>
	(LexArg.newTab(lexarg, yygone, mkPos(yypos, lexarg));
	 continue());
<COMMENT>.		=>
	(continue());

<COMMENTLINE>{eol}		=>
	(LexArg.newLine(lexarg, mkPos(yypos, lexarg));
	 if dec(LexArg.comLev lexarg) > 0 then () else YYBEGIN INITIAL; continue());
<COMMENTLINE>{tab}		=>
	(LexArg.newTab(lexarg, yygone, mkPos(yypos, lexarg));
	 continue());
<COMMENTLINE>.		=>
	(continue());
