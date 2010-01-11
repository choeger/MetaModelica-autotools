/***********************************************************
 [ load.h ] 
  - Adrian Pop, adrpo@ida.liu.se, http://www.ida.liu.se/~adrpo
  - creation 2005-12-27
    + interface for rml debugging 
    + this file contains all the functions for loading the program database. 
	+ all things in this file start with rmldb_
  - last modified 2007-06-01
************************************************************/
#ifndef _RMLDB_LOAD_H_
#define _RMLDB_LOAD_H_

#define RMLDB_MAX_STRING  1000

/* debugger typedefs */

typedef struct rmldb_range_db
{
  int sl;
  int sc;
  int el;
  int ec;
} rmldb_range_db_t;

typedef struct rmldb_ex_loc
{
   char* file;
   rmldb_range_db_t range;
   char* relation;
   char* goal;
   unsigned long SP;
} rmldb_current_execution_loc_t;

typedef struct rmldb_var_node
{
   char* var_name;
   void* var;
   int depth;
   int direction; /* RMLDB_VAR_IN or RMLDB_VAR_OUT */
   struct rmldb_var_node *prev;
   struct rmldb_var_node *next;
} rmldb_var_node_t;

/* types */

typedef struct rmldb_str
{
   char *name;
   int depth;
   struct rmldb_str *prev;
   struct rmldb_str *next;
} rmldb_str_t;

typedef struct rmldb_str_list
{
	rmldb_str_t* list_start;
	rmldb_str_t* list_end;
} rmldb_str_list_t;

/*
    excerpt from rml.grm
    datatype ident	= IDENT of string
    type tyvar		= ident
    datatype longid	= LONGID of ident option * ident
    datatype ty	= VARty of tyvar
				| CONSty of ty list * longid
				| TUPLEty of ty list
				| RELty of ty list * ty list
%nonterm ty of Absyn.ty
	   | tuple_ty of Absyn.ty list
	   | ty_sans_star of Absyn.ty
	   | ty_comma_seq2 of Absyn.ty list
	   | seq_ty of Absyn.ty list
*/

/* define type ahead, as is it mutually recursive
 * with type components 
 */
struct rmldb_type;

typedef struct rmldb_LISTty
{
   int length;
   struct rmldb_type* list_start; 
   struct rmldb_type* list_end;
} rmldb_LISTty_t;

typedef struct rmldb_VARty { char* id; } rmldb_VARty_t;

typedef struct rmldb_CONSty 
{ 
	rmldb_LISTty_t* list;
	char* id;
} rmldb_CONSty_t;

typedef struct rmldb_TUPLEty 
{ 
	rmldb_LISTty_t* list;
} rmldb_TUPLEty_t;

typedef struct rmldb_RELty 
{ 
	rmldb_LISTty_t* list1;
	rmldb_LISTty_t* list2;
} rmldb_RELty_t;


typedef enum rmldb_tyKind 
{
	RMLDB_eNORMAL,
	RMLDB_eLISTty,
	RMLDB_eVARty,
	RMLDB_eCONSty,
	RMLDB_eTUPLEty,
	RMLDB_eRELty
} rmldb_tyKind_t;

typedef struct rmldb_type
{
   rmldb_tyKind_t kind; /* selects the stufs in the union or no union at all */
   union component_t
   {
	 rmldb_LISTty_t*  l;
	 rmldb_VARty_t*   v;
	 rmldb_CONSty_t*  c;
	 rmldb_TUPLEty_t* t;
	 rmldb_RELty_t*   r;
   } component;
   
   /* this is used to name the component */
   char* name;
   
   /* these are used in rmldb_LISTty */
   int depth;
   struct rmldb_type *prev;
   struct rmldb_type *next;
} rmldb_type_t;

typedef struct rmldb_type_db
{
   char *file;
   char *name;
   rmldb_range_db_t* range;
   rmldb_type_t *type;
   int depth;
   struct rmldb_type_db *prev;
   struct rmldb_type_db *next;
} rmldb_type_db_t;

typedef struct rmldb_var_db
{
   char *file;
   rmldb_range_db_t *range;
   rmldb_range_db_t *clause_range; /* for val declarations this will be set to range */
   char *relation; /* for val declarations this will be set to "_VAL_DELCLARATION_" */
   char *name;
   rmldb_type_db_t* type_db;

   int depth;
   struct rmldb_var_db *prev;
   struct rmldb_var_db *next;
} rmldb_var_db_t;

typedef struct rmldb_con_db
{
   char *file;
   int constructor;
   int is_transparent;
   rmldb_range_db_t* range;
   char *name;
   rmldb_type_db_t* type_db;

   int depth;
   struct rmldb_con_db *prev;
   struct rmldb_con_db *next;
} rmldb_con_db_t;

typedef struct rmldb_relation_db
{
   char *file;
   rmldb_range_db_t* range;
   char *name;
   rmldb_type_db_t* type_db;

   int depth;
   struct rmldb_relation_db *prev;
   struct rmldb_relation_db *next;
} rmldb_relation_db_t;

/* ---- components ---- */
/* create a list from two tys */
extern rmldb_LISTty_t* rmldb_make_rmldb_LISTty(rmldb_type_t* type1, rmldb_type_t* type2);
/* appends ty in front of the list */
extern rmldb_LISTty_t* rmldb_make_cons_rmldb_LISTty(rmldb_type_t* type, rmldb_LISTty_t* list);
/* make a VARty */
extern rmldb_VARty_t* rmldb_make_rmldb_VARty(char* id);
/* make a CONSty */
extern rmldb_CONSty_t* rmldb_make_rmldb_CONSty(rmldb_LISTty_t* list, char* id);
/* make a TUPLEty */
extern rmldb_TUPLEty_t* rmldb_make_rmldb_TUPLEty(rmldb_LISTty_t* list);
/* make a RELty */
extern rmldb_RELty_t* rmldb_make_rmldb_RELty(rmldb_LISTty_t* list1, rmldb_LISTty_t* list2);

/* makes a type from a component, and casts it according to kind */
extern rmldb_type_t* rmldb_make_rmldb_type(rmldb_tyKind_t kind, void* component);

extern rmldb_var_db_t* rmldb_make_rmldb_var_db(
		char* file,
		rmldb_range_db_t* range,
		rmldb_range_db_t* clause_range,
		char* relation,
		char* name,
		rmldb_type_db_t* type_db);

extern rmldb_con_db_t* rmldb_make_rmldb_con_db(
		char* file,
		rmldb_range_db_t* range,
		char* name,
		rmldb_type_db_t* type_db,
		int constructor);

extern rmldb_type_db_t* rmldb_make_rmldb_type_db(
		char* file,
		rmldb_range_db_t* range,
		char* name);

extern rmldb_type_db_t* rmldb_make_rmldb_type_db_t(
		rmldb_type_t *type);

extern rmldb_relation_db_t* rmldb_make_rmldb_relation_db(
		char* file,
		rmldb_range_db_t* range,
		char* name,
		rmldb_type_db_t* type_db);

/* variabiles to hold the program database */
/* variables in relations and variables in val declarations */
extern rmldb_var_db_t*      rmldb_var_db_start;
extern rmldb_var_db_t*      rmldb_var_db_end;
/* types */
extern rmldb_type_db_t*     rmldb_type_db_start;
extern rmldb_type_db_t*     rmldb_type_db_end;
/* datatype constructors */
extern rmldb_con_db_t*      rmldb_con_db_start;
extern rmldb_con_db_t*      rmldb_con_db_end;
/* relations */
extern rmldb_relation_db_t* rmldb_relation_db_start;
extern rmldb_relation_db_t* rmldb_relation_db_end;

/* function to work with the program database */
extern int  rmldb_load_db(char** programdb);
extern void rmldb_type_list_push_front(
					rmldb_type_t **start_node, 
				    rmldb_type_t **end_node, 
					rmldb_type_t *node);

extern void rmldb_type_list_push_back(
					rmldb_type_t **start_node, 
				    rmldb_type_t **end_node, 
					rmldb_type_t *node);

extern void rmldb_str_list_add(
					rmldb_str_t **start_node, 
				    rmldb_str_t **end_node, 
					rmldb_str_t *node);

extern void rmldb_var_db_add(
					rmldb_var_db_t **start_node, 
				    rmldb_var_db_t **end_node, 
					rmldb_var_db_t *node);

extern void rmldb_con_db_add(
					rmldb_con_db_t **start_node, 
				    rmldb_con_db_t **end_node, 
					rmldb_con_db_t *node);

extern void rmldb_type_db_add(
					rmldb_type_db_t **start_node, 
				    rmldb_type_db_t **end_node, 
					rmldb_type_db_t *node);

extern void rmldb_relation_db_add(
					rmldb_relation_db_t **start_node, 
				    rmldb_relation_db_t **end_node, 
					rmldb_relation_db_t *node);

/* program database stream */
extern int   aarmldberror(char*);
extern int   aarmldbparse(void);
extern int   aarmldbdebug;
extern int   aarmldb_lineno;

#endif /* _RMLDB_LOAD_H_  */
