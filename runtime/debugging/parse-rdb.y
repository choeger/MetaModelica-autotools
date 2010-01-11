%{
#include <string.h>
#include <stdlib.h>
#include "rml.h"

extern int aarmldblex(void);

#define YYMAXDEPTH 100000000
#define YYDEBUG 1             

%}


/* The different semantic values that can be $$  = ed within the AST. */
 
%union
{
  int vint;
  char *vid;
  rmldb_type_t *vtyp;
  rmldb_LISTty_t *vtyplist;
  rmldb_range_db_t *vrangep;
}

/* uncomment this shit on linux and comment it on solaris
   damn incompatibilities
*/
/* %name-prefix="aarmldb" */

/* Here are $$  =  types for all productions that $$  =  nodes (ie, the
   $$  =  type of the bison $$ construct).
   (hint: maybe $$ has the type YYSTYPE?). */
%token T_EOF T_EOL
%token T_VAL T_VAR T_CON T_TYPE T_RELATION 
%token T_LBRACK T_RBRACK T_FATARROW T_DOT T_MORE T_COLON
%token T_LPAR T_RPAR T_COMMA T_STAR

%token <vid>T_id
%token <vid>T_tyvar
%token <vid>T_file
%token <vint>T_number

%type <vrangep> range
/*
%nonterm ty of Absyn.ty
	   | ty_sans_star of Absyn.ty
	   | tuple_ty of Absyn.ty list
	   | ty_comma_seq2 of Absyn.ty list
	   | seq_ty of Absyn.ty list
*/
%type <vtyp> ty ty_sans_star
%type <vtyplist> tuple_ty ty_comma_seq2 seq_ty  

/* %type <vvoidp> tyvarseq tyvar_comma_plus */

/* v:<main.rml>:99.53.99.57|[97.9.99.86]|buildNormalAst[stmts:Absyn.Stmt list]
 * t:<values.rml>:40.12.40.16|Values.Value
 * c:<values.rml>:47.20.47.25|Values.RECORD:(Values.Value list,string list) => Values.Value
 * r:<values.rml>:428.10.428.20|Values.mult_matrix:(Values.Value list,Values.Value list) => Values.Value list 
 * l:<Builtin.rml>:660.5.660.20|n2real2arrayreal:Types.TType*'a option 
 */

%%

lines:
	| line T_EOL lines
	| T_EOF
	{
		YYACCEPT;
	}
    ;
    
line:
T_VAL T_file T_COLON range T_MORE T_id T_COLON ty 
{
	/* l:<Builtin.rml>:660.5.660.20|n2real2arrayreal:Types.TType*'a option */
	rmldb_var_db_t* rmldb_var_db_node = 
		rmldb_make_rmldb_var_db($2, $4, $4, "_VL_", $6, 
			rmldb_make_rmldb_type_db_t($8));		
	rmldb_var_db_add(&rmldb_var_db_start, &rmldb_var_db_end, rmldb_var_db_node);
#ifdef _DEBUG_RDB_
	fprintf(stderr, "file[%s],relation[%s],variable[%s] -> ",$2,"_VL_",$6);
	rmldb_print_type($8, 1, stderr);
	fprintf(stderr,"\n");
#endif
}
| T_VAR T_file T_COLON range T_MORE T_LBRACK range T_RBRACK T_MORE T_id T_LBRACK T_id T_COLON ty T_RBRACK 
{
	/* v:<main.rml>:99.53.99.57|[97.9.99.86]|buildNormalAst[stmts:Absyn.Stmt list] */
	rmldb_var_db_t* rmldb_var_db_node = 
		rmldb_make_rmldb_var_db($2, $4, $7, $10, $12, 
			rmldb_make_rmldb_type_db_t($14));		
	rmldb_var_db_add(&rmldb_var_db_start, &rmldb_var_db_end, rmldb_var_db_node);
#ifdef _DEBUG_RDB_
	fprintf(stderr, "file[%s],relation[%s],variable[%s] -> ",$2,$10,$12);
	rmldb_print_type($14, 1, stderr);
	fprintf(stderr,"\n");
#endif
}
| T_CON T_file T_COLON range T_MORE T_id T_COLON ty
{
	/* c:<values.rml>:47.20.47.25|Values.RECORD:(Values.Value list,string list) => Values.Value */
	rmldb_con_db_t* rmldb_con_db_node = 
		rmldb_make_rmldb_con_db($2, $4, $6, 
			rmldb_make_rmldb_type_db_t($8),0);		
	rmldb_con_db_add(&rmldb_con_db_start, &rmldb_con_db_end, rmldb_con_db_node); 
#ifdef _DEBUG_RDB_
	fprintf(stderr,"file[%s],con[%s],type: ",$2,$6);
	rmldb_print_type($8, 1, stderr);
	fprintf(stderr,"\n"); 
#endif
}
| T_TYPE T_file T_COLON range T_MORE T_id
{
	/* t:<values.rml>:40.12.40.16|Values.Value */
	rmldb_type_db_t* rmldb_type_db_node = rmldb_make_rmldb_type_db($2, $4, $6);	
	rmldb_type_db_add(&rmldb_type_db_start, &rmldb_type_db_end, rmldb_type_db_node); 
#ifdef _DEBUG_RDB_
	fprintf(stderr,"file[%s],type[%p]\n",$2,$6); 
#endif
}
| T_RELATION T_file T_COLON range T_MORE T_id T_COLON ty
{
	/* r:<values.rml>:428.10.428.20|Values.mult_matrix:(Values.Value list,Values.Value list) => Values.Value list */
	rmldb_relation_db_t* rmldb_relation_db_node = 
		rmldb_make_rmldb_relation_db($2, $4, $6, 
			rmldb_make_rmldb_type_db_t($8));
	rmldb_relation_db_add(&rmldb_relation_db_start, &rmldb_relation_db_end, rmldb_relation_db_node); 
#ifdef _DEBUG_RDB_
	fprintf(stderr, "file[%s],relation[%s],type: ",$2,$6);  
	rmldb_print_type($8, 1, stderr);
	fprintf(stderr,"\n");
#endif
}
;

range: 
	T_number T_DOT T_number T_DOT T_number T_DOT T_number
	{
	  rmldb_range_db_t *pRange;
	  pRange = (rmldb_range_db_t*) malloc (sizeof(rmldb_range_db_t));
	  /* printf ("%d,%d,%d,%d", $1, $3, $5, $7); */
	  pRange->sl = $1; pRange->sc = $3;
	  pRange->el = $5; pRange->ec = $7;
	  $$  = pRange;
	}
	;
	
/*
    excerpt from rml.grm
    datatype ident	= IDENT of string * info ref
    type tyvar		= ident
    datatype longid	= LONGID of ident option * ident
    datatype ty	= VARty of tyvar
				| CONSty of ty list * longid
				| TUPLEty of ty list
				| RELty of ty list * ty list
				| NAMEDty of ident * ty
%nonterm ty of Absyn.ty
	   | ty_sans_star of Absyn.ty
	   | tuple_ty of Absyn.ty list	   
	   | ty_comma_seq2 of Absyn.ty list
	   | seq_ty of Absyn.ty list
*/

ty
	: seq_ty T_FATARROW seq_ty
	{
	  /* 
	  Absyn.RELty(seq_ty1, seq_ty2) 
	  printf ("Ty.REL(%p,%p)",$1,$3);
	  */
	  $$  = rmldb_make_rmldb_type(RMLDB_eRELty, (void*)rmldb_make_rmldb_RELty($1, $3));
    }
	| tuple_ty
	{
		/* 
		mktuplety(tuple_ty)
		printf ("Ty.TUPLE(%p)",$1);
		*/
		if ($1->length > 1)
			$$ = rmldb_make_rmldb_type(RMLDB_eTUPLEty, (void*)rmldb_make_rmldb_TUPLEty($1));
		else 
			$$  = $1->list_start;
	}
	;

tuple_ty
	: ty_sans_star T_STAR tuple_ty
	{
	   /* 
	   ty_sans_star :: tuple_ty 
	   printf ("Ty.LIST(%p,%p)",$1,$3);
	   */
	  $$ = rmldb_make_cons_rmldb_LISTty($1,$3);
	}
	| ty_sans_star
	{
		/* 
		[ty_sans_star]
	    printf ("Ty.LIST(%p)",$1);
	    */		
		$$  =  rmldb_make_rmldb_LISTty($1,NULL);
	}
	| T_id T_COLON ty_sans_star 
	{
		/*
		( [ Absyn.NAMEDty(ident, ty_sans_star, makeInfo lexArg (identleft, ty_sans_starright)) ] )
		*/
		$3->name = $1;
		$$  =  rmldb_make_rmldb_LISTty($3,NULL);
	}
	| T_id T_COLON ty_sans_star T_STAR tuple_ty
	{
		/*
		( Absyn.NAMEDty(ident, ty_sans_star, makeInfo lexArg (identleft, tuple_tyright)) :: tuple_ty )
		*/
		$3->name = $1;
		$$  =  rmldb_make_cons_rmldb_LISTty($3,$5);		
	}
	;

ty_sans_star
	: ty_sans_star T_id
	{
	  /* 
	  ( Absyn.CONSty([ty_sans_star], longorshortid) 
	  printf ("Ty.CONSty(list:%p,%s)",$1,$2);		
	  */
	  $$  =  rmldb_make_rmldb_type(RMLDB_eCONSty, (void*)rmldb_make_rmldb_CONSty(rmldb_make_rmldb_LISTty($1,NULL), $2));
	}
	| T_LPAR ty_comma_seq2 T_RPAR T_id
	{
		/*
		printf ("(Ty.CONSty(list:%p,%s))",$2,$4); 
		( Absyn.CONSty(ty_comma_seq2, longorshortid) 
		*/	
	    $$  =  rmldb_make_rmldb_type(RMLDB_eCONSty, (void*)rmldb_make_rmldb_CONSty($2,$4));
	}
	| T_LPAR ty T_RPAR
	{
	  /* 
	  ty 
	  printf ("Ty(:%p)",$2);
	  */	  
	  $$  =  $2;
	}
	| T_tyvar
	{
		/* 
	    printf ("Tyvar(:%p)",$1);	  
		( Absyn.VARty(tyvar, makeInfo myLoc (tyvarleft, tyvarright)) ) 
		*/
		$$  =  rmldb_make_rmldb_type(RMLDB_eVARty, (void*)rmldb_make_rmldb_VARty($1));
	}	
	| T_id
	{
		/* 		
		( Absyn.CONSty([], longorshortid,
	    printf ("Ty.CONSty(:%s)",$1);	  	
	    */
		$$  =  rmldb_make_rmldb_type(
				RMLDB_eCONSty, 
				(void*)rmldb_make_rmldb_CONSty(
					rmldb_make_rmldb_LISTty(NULL,NULL), $1));
	}
	;

ty_comma_seq2
	: ty T_COMMA ty_comma_seq2
	{	  	
		/* 
		( ty :: ty_comma_seq2 )
		printf ("TyList(:%p,%p)",$1,$3); 
		*/
	  	$$ = rmldb_make_cons_rmldb_LISTty($1,$3);
	}
	| ty T_COMMA ty
	{
		/* 
		( [ty1, ty2] ) 
		printf ("TyList(:%p,%p)",$1,$3);
		*/
	  	$$ = rmldb_make_rmldb_LISTty($1,$3);
	}
	;

seq_ty
	: T_LPAR T_RPAR
	{ 
		/*
		([])
		printf ("seq_ty_empty");
		*/		
		$$ = rmldb_make_rmldb_LISTty(NULL, NULL); 
	}
	| T_LPAR ty_comma_seq2 T_RPAR
	{
		/* 
		printf ("(Ty_comma_seq2(%p))",$2);
		( ty_comma_seq2 ) 
		*/
		$$  =  $2;
	}
	| tuple_ty
	{
		/*
		printf ("Ty.TUPLE(%p)",$1);	
		( [mktuplety(tuple_ty)] ) 
		*/
		if ($1->length > 1)
			$$ = rmldb_make_rmldb_LISTty(
						rmldb_make_rmldb_type(
							RMLDB_eTUPLEty, 
							(void*)rmldb_make_rmldb_TUPLEty($1)),NULL);
		else 
			$$  = rmldb_make_rmldb_LISTty($1->list_start,NULL);
	}
	;


%%

int aarmldberror(char *s) 
{
  fprintf(stderr, "%s at line: %d\n", s, aarmldb_lineno);
  return  -1;
}
