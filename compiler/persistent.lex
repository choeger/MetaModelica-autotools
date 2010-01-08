(* lexer/rml.lex -- ML-Lex lexical specification for the .mo persistency layer *)

(* parameters for ML-Yacc *)
type arg = LexArgSimple.lexarg
type pos = LexArgSimple.pos
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token

exception PersistentLexError

fun eof(lexarg) =
  let val pos = LexArgSimple.readPos lexarg 
  in
    if !(LexArgSimple.eofFlag lexarg) then Tokens.EOF_HARD(pos, pos)
    else
      let val pos1 = !(LexArgSimple.leftPos lexarg)
	  val _ = LexArgSimple.eofFlag lexarg := true
      in
		if !(LexArgSimple.comLev lexarg) > 0 then
		 LexArgSimple.errorMsg lexarg ("unterminated comment at EOF", pos1, pos)
		else if !(LexArgSimple.strList lexarg) <> [] then
		 LexArgSimple.errorMsg lexarg ("unterminated string at EOF", pos1, pos)
		else ();
		Tokens.EOF_SOFT(pos, pos)
      end
  end

fun makeDECINT(pos1, str) =
  let val pos2 = pos1 + String.size str - 1
  in
    Tokens.ICON(LexUtil.decint str, pos1, pos2)
  end

fun makeHEXINT(pos1, str) =
  let val pos2 = pos1 + String.size str - 1
  in
    Tokens.ICON(LexUtil.hexint str, pos1, pos2)
  end

fun makeRCON(pos1, str) =
  let val pos2 = pos1 + String.size str - 1
  in
    Tokens.RCON(LexUtil.rcon str, pos1, pos2)
  end

fun makeCCON(pos1, str) =
  let val pos2 = pos1 + String.size str - 1
  in
    Tokens.CCON(LexUtil.ccon str, pos1, pos2)
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
  let val strList = LexArgSimple.strList lexarg
  in
    strList := yytext :: !strList
  end

fun errBadStr(lexarg, pos, yytext) =
  let val msg = "illegal character in string " ^ MakeString.scvt yytext
  in
    LexArgSimple.errorMsg lexarg (msg,pos,pos)
  end

fun makeSCON(lexarg, pos2) =
  let val strList = LexArgSimple.strList lexarg
      val str = String.concat(List.rev("\"" :: !strList))
      val _ = strList := []
  in
    Tokens.SCON(LexUtil.scon str, !(LexArgSimple.leftPos lexarg), pos2)
  end

fun makeTYVAR(pos1, str) =
  let val len_1 = String.size str - 1
  in
    Tokens.TYVAR(
		substring(str, 1, len_1), pos1 + 1, pos1 + len_1)
  end

val kwds = 
[
  ("$L", Tokens.LONGID),
  ("$Vt", Tokens.VARty),
  ("$Ct", Tokens.CONSty),
  ("$Rt", Tokens.RELty),
  ("$Nt", Tokens.NAMEDty),
  ("$Tt", Tokens.TUPLEty),
  ("$S", Tokens.SOM),
  ("$N", Tokens.NON),
  ("$Wp", Tokens.WILDpat),
  ("$Lp", Tokens.LITpat),
  ("$Cp", Tokens.CONpat),
  ("$Sp", Tokens.STRUCTpat),
  ("$Bp", Tokens.BINDpat),
  ("$Ip", Tokens.IDENTpat),
  ("$Np", Tokens.NAMEDpat),
  ("$Le", Tokens.LITexp),
  ("$Ce", Tokens.CONexp),
  ("$Ve", Tokens.VARexp),
  ("$Se", Tokens.STRUCTexp),
  ("$Ie", Tokens.IDENTexp),
  ("$Cg", Tokens.CALLgoal),
  ("$Eg", Tokens.EQUALgoal),
  ("$Lg", Tokens.LETgoal),
  ("$Ng", Tokens.NOTgoal),
  ("$Ag", Tokens.ANDgoal),
  ("$Ig", Tokens.CONDgoal),
  ("$Rr", Tokens.RETURN),
  ("$Fr", Tokens.FAIL),
  ("$C1", Tokens.CLAUSE1),
  ("$C2", Tokens.CLAUSE2),
  ("$COcb", Tokens.CONcb),
  ("$CTcb", Tokens.CTORcb),
  ("$DA", Tokens.DATBIND),
  ("$TY", Tokens.TYPBIND),
  ("$RE", Tokens.RELBIND),
  ("$R", Tokens.REF),
  ("$Ws", Tokens.WITHspec),
  ("$As", Tokens.ABSTYPEspec),
  ("$Ts", Tokens.TYPEspec),
  ("$Ds", Tokens.DATAspec),
  ("$Vs", Tokens.VALspec),
  ("$Rs", Tokens.RELspec),
  ("$Wd", Tokens.WITHdec),
  ("$Td", Tokens.TYPEdec),
  ("$Dd", Tokens.DATAdec),
  ("$Vd", Tokens.VALdec),
  ("$Rd", Tokens.RELdec),
  ("$INT", Tokens.INTERFACE),
  ("$SRC", Tokens.SOURCE),
  ("$MODULE", Tokens.MODULE),
  ("$DEC", Tokens.DECLARATIONS),
  ("$DSRC", Tokens.DUMMY_SOURCE),
  ("$DINT", Tokens.DUMMY_INTERFACE),
  ("$SerializationInfo", Tokens.SERIALIZATION_INFO),
  ("$LV", Tokens.LV),
  ("$ATR", Tokens.ATTR)      
]

fun makeIDENT(pos1, str) =
  let val pos2 = pos1 + String.size str - 1
  in
    Tokens.IDENT(str, pos1, pos2)
  end


fun makeKEY(lexarg, pos1, str) =
  let val pos2 = pos1 + String.size str - 1
      fun loop [] = 
      (
       LexArgSimple.errorMsg 
			lexarg 
			("This shouldn't happen! Undefined key in persistent.lex:"^str,pos1,pos2);
       raise PersistentLexError
      )
	| loop((str',mktok)::rest) =
	    if str=str' then mktok(pos1, pos2) else loop rest
  in
    loop kwds
  end

fun inc(ri) = ri := !ri + 1
fun dec(ri) = let val j = !ri - 1 in ri := j; j end

%%

%header (functor PERSISTENTLexFn(
			  structure Tokens : PERSISTENT_TOKENS
			  structure Cache : CACHE
			  structure LexArgSimple : LEXARG_SIMPLE where type poz = Cache.poz
			  structure MakeString : MAKESTRING
			  structure LexUtil : LEXUTIL
			  sharing type Cache.visibility = LexArgSimple.visibility
			  ) : ARG_LEXER);
			  
%S STRING GAP;
%arg (lexarg);
%full

white=[\ \013\012];
eol="\010";
tab=\t;

ddigit=[0-9];
decint=[-~]?{ddigit}+;
xdigit=[0-9a-fA-F];
hexint=[-~]?"0x"{xdigit}+;

fraction="."{ddigit}+;
exponent=[eE]{decint};
rcon={decint}({fraction}{exponent}?|{exponent});

echar=[\\"nrtfabv];
cntrl=[?-_];
escseq={ddigit}{3}|"^"{cntrl}|{echar};
pchar=[\ -!#-[^-~\128-\255]|"]";
cdesc={pchar}|\\{escseq};
ccon="#\""{cdesc}\";

alpha=[A-Za-z];
alnum={alpha}|[_'0-9];
kwd="$"{alpha}{alnum}*;
id={alpha}({alnum}|".")*;

tyvar="'"+{alpha}{alnum}*;

%%

<INITIAL>{eol}		=>
	(LexArgSimple.newLine(lexarg, yypos);
	 continue());
<INITIAL>{tab}		=>
	(LexArgSimple.newTab(lexarg, yygone, yypos);
	 continue());
<INITIAL>{white}	=>
	(continue());
<INITIAL>\"		=>
	(LexArgSimple.strList lexarg := [yytext];
	 LexArgSimple.leftPos lexarg := yypos;
	 YYBEGIN STRING;
	 continue());
<INITIAL>"("		=>
	(Tokens.LPAR(yypos, yypos));
<INITIAL>")"		=>
	(Tokens.RPAR(yypos, yypos));
<INITIAL>","		=>
	(Tokens.COMMA(yypos, yypos));
<INITIAL>";"		=>
	(Tokens.SEMICOLON(yypos, yypos));
<INITIAL>":"		=>
	(Tokens.COLON(yypos, yypos));
<INITIAL>"{"		=>
	(Tokens.LBRACE(yypos, yypos));
<INITIAL>"}"		=>
	(Tokens.RBRACE(yypos, yypos));
<INITIAL>{decint}	=>
	(makeDECINT(yypos, yytext));
<INITIAL>{hexint}	=>
	(makeHEXINT(yypos, yytext));
<INITIAL>{rcon}		=>
	(makeRCON(yypos, yytext));
<INITIAL>{ccon}		=>
	(makeCCON(yypos, yytext));
<INITIAL>{id}		=>
	(makeIDENT(yypos, yytext));
<INITIAL>{kwd}		=>
	(makeKEY(lexarg, yypos, yytext));
<INITIAL>{tyvar}	=>
	(makeTYVAR(yypos, yytext));
		
<INITIAL>.		=>
	(let val msg = "illegal character " ^ MakeString.scvt yytext
	 in
	   LexArgSimple.errorMsg lexarg (msg,yypos,yypos);
	   continue()
	 end);

<STRING>\"		=>
	(YYBEGIN INITIAL; makeSCON(lexarg, yypos));
<STRING>\\{eol}		=>
	(LexArgSimple.newLine(lexarg, yypos+1);
	 YYBEGIN GAP; continue());
<STRING>\\{tab}		=>
	(LexArgSimple.newTab(lexarg, yygone, yypos+1);
	 YYBEGIN GAP; continue());
<STRING>\\{white}	=>
	(YYBEGIN GAP; continue());
<STRING>{cdesc}		=>
	(addString(lexarg, yytext); continue());
<STRING>{eol}		=>
	(LexArgSimple.newLine(lexarg, yypos);
	 addString(lexarg, yytext); continue());
<STRING>{tab}		=>
	(LexArgSimple.newTab(lexarg, yygone, yypos);
	 addString(lexarg, yytext); continue());
<STRING>.		=>
	(errBadStr(lexarg, yypos, yytext); continue());

<GAP>\\			=>
	(YYBEGIN STRING; continue());
<GAP>{eol}		=>
	(LexArgSimple.newLine(lexarg, yypos);
	 continue());
<GAP>{tab}		=>
	(LexArgSimple.newTab(lexarg, yygone, yypos);
	 continue());
<GAP>{white}		=>
	(continue());
<GAP>.			=>
	(let val msg = "illegal character in string gap " ^ MakeString.scvt yytext
	 in
	   LexArgSimple.errorMsg lexarg (msg,yypos,yypos);
	   continue()
	 end);
