/* 
 * This file is part of OpenModelica.
 * 
 * It implements the Relational Meta-Language (RML) and MetaModelica
 * http://www.ida.liu.se/~pelab/rml
 * 
 * Copyright (c) 1998-2008, Linköpings University,
 * Department of Computer and Information Science, 
 * SE-58183 Linköping, Sweden. 
 * 
 * All rights reserved.
 * 
 * THIS PROGRAM IS PROVIDED UNDER THE TERMS OF THIS OSMC PUBLIC 
 * LICENSE (OSMC-PL). ANY USE, REPRODUCTION OR DISTRIBUTION OF 
 * THIS PROGRAM CONSTITUTES RECIPIENT'S ACCEPTANCE OF THE OSMC 
 * PUBLIC LICENSE. 
 * 
 * The OpenModelica software and the Open Source Modelica 
 * Consortium (OSMC) Public License (OSMC-PL) are obtained 
 * from Linköpings University, either from the above address, 
 * from the URL: http://www.ida.liu.se/projects/OpenModelica
 * and in the OpenModelica distribution.
 * 
 * This program is distributed  WITHOUT ANY WARRANTY; without 
 * even the implied warranty of  MERCHANTABILITY or FITNESS 
 * FOR A PARTICULAR PURPOSE, EXCEPT AS EXPRESSLY SET FORTH 
 * IN THE BY RECIPIENT SELECTED SUBSIDIARY LICENSE CONDITIONS 
 * OF OSMC-PL. 
 * 
 * See the full OSMC Public License conditions for more details.
 * 
 * @author Adrian Pop [adrpo@ida.liu.se, http://www.ida.liu.se/~adrpo]
 * 
 */

/***********************************************************
[ rml-db-load.c ] 
- Adrian Pop, adrpo@ida.liu.se, http://www.ida.liu.se/~adrpo 
- creation: 2005-12-27
  + implementation for rml debugging 
    this file contains all the functions for loading the program database.
    look also into runtime/debugging for:
    * debug-debug.c 
    * debug-print.c
    * debug-push-vars.c
	* rml-debug.*
  + all things in this file starts with rmldb_
- last modified: 2007-06-13
************************************************************/

#ifndef  _RML_DB_LOAD_H_
#include "rml.h"
#endif
#include <stdio.h>
#include <stdlib.h>

#define RMLDB_RANGE_EQ  0
#define RMLDB_RANGE_GT  1
#define RMLDB_RANGE_LT -1

extern char* rmldb_get_type_id(rmldb_con_db_t* con_db);
extern rmldb_LISTty_t* rmldb_get_type_components(rmldb_con_db_t* con_db);

rmldb_var_db_t *rmldb_var_db_start = NULL;
rmldb_var_db_t *rmldb_var_db_end   = NULL;
rmldb_type_db_t *rmldb_type_db_start = NULL;
rmldb_type_db_t *rmldb_type_db_end   = NULL;
rmldb_con_db_t *rmldb_con_db_start = NULL;
rmldb_con_db_t *rmldb_con_db_end   = NULL;
rmldb_relation_db_t *rmldb_relation_db_start = NULL;
rmldb_relation_db_t *rmldb_relation_db_end   = NULL;

/* returns: (we compare only the first line/column of each range
  0 if equal
 +1 if [overlapping] bigger
 -1 if [overlapping] smaller
*/
int rmldb_range_cmp(rmldb_range_db_t *r1, rmldb_range_db_t *r2)
{
	int factor = 1000;
	int t1s,t1e,t2s,t2e;
	t1s = r1->sl*factor + r1->sc; t1e = r1->el*factor + r1->ec; 
	t2s = r2->sl*factor + r2->sc; t2e = r2->el*factor + r2->ec;
	if (t1s == t2s && t1e == t2e) return RMLDB_RANGE_EQ;
	if (t1s > t2s) return RMLDB_RANGE_GT;
	if (t1s < t2s) return RMLDB_RANGE_LT;
}

/* ---- components ---- */
/* create a list from two tys */
extern rmldb_LISTty_t* rmldb_make_rmldb_LISTty(rmldb_type_t* type1, rmldb_type_t* type2)
{
	rmldb_LISTty_t* rmlLISTty_node = (rmldb_LISTty_t*)malloc(sizeof(rmldb_LISTty_t));
	rmlLISTty_node->list_start = NULL;
	rmlLISTty_node->list_end = NULL;
	if (type1)
		rmldb_type_list_push_back(
			&(rmlLISTty_node->list_start),
			&(rmlLISTty_node->list_end),
			type1);
	if (type2)
		rmldb_type_list_push_back(
			&(rmlLISTty_node->list_start),
			&(rmlLISTty_node->list_end),
			type2);
	if (type1 || type2)
		rmlLISTty_node->length = rmlLISTty_node->list_end->depth + 1;
	else
		rmlLISTty_node->length = 0;
	return rmlLISTty_node;
}

/* appends ty in front of the list */
extern rmldb_LISTty_t* rmldb_make_cons_rmldb_LISTty(rmldb_type_t* type, rmldb_LISTty_t* list)
{
	if (type && list)
		rmldb_type_list_push_front(
			&list->list_start,
			&list->list_end,
			type);
	if (list->list_end) 
		list->length = list->list_end->depth + 1;
	else
		list->length = 0;
	return list;
}

/* make a VARty */
extern rmldb_VARty_t* rmldb_make_rmldb_VARty(char* id)
{
	rmldb_VARty_t* rmlVARty_node = (rmldb_VARty_t*)malloc(sizeof(rmldb_VARty_t));
	rmlVARty_node->id = id;
	return rmlVARty_node;
}

/* make a CONSty */
extern rmldb_CONSty_t* rmldb_make_rmldb_CONSty(rmldb_LISTty_t* list, char* id)
{
	rmldb_CONSty_t* rmlCONSty_node = (rmldb_CONSty_t*)malloc(sizeof(rmldb_CONSty_t));
	rmlCONSty_node->list = list;
	rmlCONSty_node->id = id;
	return rmlCONSty_node;
}

/* make a TUPLEty */
extern rmldb_TUPLEty_t* rmldb_make_rmldb_TUPLEty(rmldb_LISTty_t* list)
{
	rmldb_TUPLEty_t* rmlTUPLEty_node = (rmldb_TUPLEty_t*)malloc(sizeof(rmldb_TUPLEty_t));
	rmlTUPLEty_node->list = list;
	return rmlTUPLEty_node;	
}

/* make a RELty */
extern rmldb_RELty_t* rmldb_make_rmldb_RELty(rmldb_LISTty_t* list1, rmldb_LISTty_t* list2)
{
	rmldb_RELty_t* rmlRELty_node = (rmldb_RELty_t*)malloc(sizeof(rmldb_RELty_t));
	rmlRELty_node->list1 = list1;
	rmlRELty_node->list2 = list2;
	return rmlRELty_node;	
}

/* makes a type from a component, and casts it according to kind */
extern rmldb_type_t* rmldb_make_rmldb_type(rmldb_tyKind_t kind, void* component)
{
	rmldb_type_t* rmldb_type_node = (rmldb_type_t*)malloc(sizeof(rmldb_type_t));
	rmldb_type_node->kind = kind;
	switch (kind)
	{
	case RMLDB_eLISTty:  rmldb_type_node->component.l = (rmldb_LISTty_t*)component; break;
	case RMLDB_eVARty:   rmldb_type_node->component.v = (rmldb_VARty_t*)component; break;
	case RMLDB_eCONSty:  rmldb_type_node->component.c = (rmldb_CONSty_t*)component; break;
	case RMLDB_eTUPLEty: rmldb_type_node->component.t = (rmldb_TUPLEty_t*)component; break;
	case RMLDB_eRELty:   rmldb_type_node->component.r = (rmldb_RELty_t*)component; break;
	}
    rmldb_type_node->name = 0;
	rmldb_type_node->depth = 0;
    rmldb_type_node->prev = 0;
    rmldb_type_node->next = 0;
	return rmldb_type_node;
}

extern void rmldb_type_list_push_back(rmldb_type_t **start_node, rmldb_type_t **end_node, rmldb_type_t *node)
{
	rmldb_type_t *last;
	last = node;
	if(*start_node)
		last->depth = ((rmldb_type_t*)*end_node)->depth + 1;
	else { *start_node = last; last->depth = 0; }
	last->next = NULL;	last->prev = *end_node;
	if(*end_node) ((rmldb_type_t*)*end_node)->next = last;
	*end_node = last;
}

extern void rmldb_type_list_push_front(rmldb_type_t **start_node, rmldb_type_t **end_node, rmldb_type_t *node)
{
	rmldb_type_t *first, *tmp;
	first = node;
	first->depth = 0;
	first->next = NULL;
	if(*start_node)
	{
		/* depth++ on all */
		for(tmp = *start_node; tmp; tmp = tmp->next)
		{
			tmp->depth++;
		}
		/* link front */
		first->next = *start_node;
		((rmldb_type_t*)*start_node)->prev = first;
	}
	else
	{
		if (!*end_node) *end_node = first;
	}
	first->prev = NULL;
	*start_node = first;
}


extern rmldb_var_db_t* rmldb_make_rmldb_var_db(
		char* file,
		rmldb_range_db_t* range,
		rmldb_range_db_t* clause_range,
		char* relation,
		char* name,
		rmldb_type_db_t* type_db)
{
	rmldb_var_db_t* rmldb_var_db_node = (rmldb_var_db_t*)malloc(sizeof(rmldb_var_db_t));
	rmldb_var_db_node->file = file;
	rmldb_var_db_node->range = range;
	rmldb_var_db_node->clause_range = clause_range;
	rmldb_var_db_node->relation = relation;
	rmldb_var_db_node->name = name;
	rmldb_var_db_node->type_db = type_db;
	rmldb_var_db_node->depth = 0;
	rmldb_var_db_node->prev = NULL;
	rmldb_var_db_node->next = NULL;
	return rmldb_var_db_node;
}

void rmldb_var_db_add(rmldb_var_db_t **start_node, 
				    rmldb_var_db_t **end_node, 
					rmldb_var_db_t *node)
{
	rmldb_var_db_t *last;
	last = node;
	if(*start_node)
		last->depth = ((rmldb_var_db_t*)*end_node)->depth + 1;
	else { *start_node = last; last->depth = 0; }
	last->next = NULL;	last->prev = *end_node;
	if(*end_node) ((rmldb_var_db_t*)*end_node)->next = last;
	*end_node = last;
}

extern rmldb_con_db_t* rmldb_make_rmldb_con_db(
		char* file,
		rmldb_range_db_t* range,
		char* name,
		rmldb_type_db_t* type_db,
		int constructor)
{
	rmldb_con_db_t* rmldb_con_db_node = (rmldb_con_db_t*)malloc(sizeof(rmldb_con_db_t));
	rmldb_con_db_node->file = file;
	rmldb_con_db_node->range = range;
	rmldb_con_db_node->name = name;
	rmldb_con_db_node->type_db = type_db;
	rmldb_con_db_node->constructor = constructor;
	rmldb_con_db_node->is_transparent = 0;
	rmldb_con_db_node->depth = 0;
	rmldb_con_db_node->prev = NULL;
	rmldb_con_db_node->next = NULL;
	return rmldb_con_db_node;
}

void rmldb_con_db_add(struct rmldb_con_db **start_node, 
				    struct rmldb_con_db **end_node, 
					struct rmldb_con_db *node)
{
	struct rmldb_con_db *last;
	last = node;
	if(*start_node)
		last->depth = ((struct rmldb_con_db*)*end_node)->depth + 1;
	else { *start_node = last; last->depth = 0; }
	last->next = NULL;	last->prev = *end_node;
	if(*end_node) ((struct rmldb_con_db*)*end_node)->next = last;
	*end_node = last;
}

extern rmldb_type_db_t* rmldb_make_rmldb_type_db(
		char* file,
		rmldb_range_db_t* range,
		char* name)
{
	rmldb_type_db_t* rmldb_type_db_node = (rmldb_type_db_t*)malloc(sizeof(rmldb_type_db_t));
	rmldb_type_db_node->file = file;
	rmldb_type_db_node->name = name;
	rmldb_type_db_node->range = range;
	rmldb_type_db_node->type = NULL;
	rmldb_type_db_node->depth = 0;
	rmldb_type_db_node->prev = NULL;
	rmldb_type_db_node->next = NULL;
	return rmldb_type_db_node;
}

extern rmldb_type_db_t* rmldb_make_rmldb_type_db_t(
		rmldb_type_t *type)
{
	rmldb_type_db_t* rmldb_type_db_node = (rmldb_type_db_t*)malloc(sizeof(rmldb_type_db_t));
	rmldb_type_db_node->file = NULL;
	rmldb_type_db_node->range = NULL;
	rmldb_type_db_node->name = NULL;
	rmldb_type_db_node->type = type;
	rmldb_type_db_node->depth = 0;
	rmldb_type_db_node->prev = NULL;
	rmldb_type_db_node->next = NULL;
	return rmldb_type_db_node;
}


void rmldb_type_db_add(struct rmldb_type_db **start_node, 
				    struct rmldb_type_db **end_node, 
					struct rmldb_type_db *node)
{
	struct rmldb_type_db *last;
	last = node;
	if(*start_node)
		last->depth = ((struct rmldb_type_db*)*end_node)->depth + 1;
	else { *start_node = last; last->depth = 0; }
	last->next = NULL;	last->prev = *end_node;
	if(*end_node) ((struct rmldb_type_db*)*end_node)->next = last;
	*end_node = last;
}

extern rmldb_relation_db_t* rmldb_make_rmldb_relation_db(
		char* file,
		rmldb_range_db_t* range,
		char* name,
		rmldb_type_db_t* type_db)
{
	rmldb_relation_db_t* rmldb_relation_db_node = (rmldb_relation_db_t*)malloc(sizeof(rmldb_relation_db_t));
	rmldb_relation_db_node->file = file;
	rmldb_relation_db_node->range = range;
	rmldb_relation_db_node->name = name;
	rmldb_relation_db_node->type_db = type_db;	
	rmldb_relation_db_node->depth = 0;
	rmldb_relation_db_node->prev = NULL;
	rmldb_relation_db_node->next = NULL;
	return rmldb_relation_db_node;
}

void rmldb_relation_db_add(struct rmldb_relation_db **start_node, 
				    struct rmldb_relation_db **end_node, 
					struct rmldb_relation_db *node)
{
	struct rmldb_relation_db *last;
	last = node;
	if(*start_node)
		last->depth = ((struct rmldb_relation_db*)*end_node)->depth + 1;
	else { *start_node = last; last->depth = 0; }
	last->next = NULL;	last->prev = *end_node;
	if(*end_node) ((struct rmldb_relation_db*)*end_node)->next = last;
	*end_node = last;
}


void rmldb_order_rmldb_con_db(void)
{
#ifdef RML_DEBUG
	/*
   char *file; int constructor; rmldb_range_db_t* range; char *name;
   rmldb_type_db_t* type_db; int depth;
	*/
	char *buff;
	int seq;
	rmldb_con_db_t *i,*j;
	void *tmp = NULL;
    int tmpInt = 0;
	for(j = rmldb_con_db_start; j; j = j->next)
	for(i = j; i; i = i->next)
	{
	    if (!strcmp(i->file, j->file))
		if (rmldb_range_cmp(j->range, i->range) == RMLDB_RANGE_GT)
		{
			/* XXXX fixme! move arround ->prev ->next not the contents */
			tmp = (void*)i->range;
			i->range = j->range;
			j->range = (rmldb_range_db_t*)tmp;

			buff = (char*)malloc(strlen(i->name)+1);
			strcpy(buff, i->name);
			free(i->name);
			i->name = (char*)malloc(strlen(j->name)+1);
			strcpy(i->name,j->name);
			free(j->name);
			j->name = buff;

			tmp = (void*)i->type_db;
			i->type_db = j->type_db;
			j->type_db = (rmldb_type_db_t*)tmp;

			tmpInt = i->depth;
			i->depth = j->depth;
			j->depth = tmpInt;
		}
	}
	/* order over datatypes of the same type */
	seq = 0;
	for(i = rmldb_con_db_start; i; i = i->next)
	{
		if (i->next)
		{
			if (!strcmp(rmldb_get_type_id(i), rmldb_get_type_id(i->next)))
			{
				i->constructor = seq; seq++;
				i->next->constructor = seq;
			}
			else
			{
				if (seq == 0) /* means there is ONLY ONE CONSTRUCTOR in this datatype */
				{
					rmldb_LISTty_t* rmlLISTty_node = rmldb_get_type_components(i);
					/* means is not a constant constructor OR a type > 2 */
					/* must be exactly 1 slot and 1 constructor in the datatype
					 * datatype varTys ident = CONSTRUCTOR tys */
					if (rmlLISTty_node != NULL && rmlLISTty_node->length == 1)  
					{
						i->is_transparent = 1;
						/*
						fprintf(stderr,"TRANSPARENT on %s->%s\n", i->name, rmldb_get_type_id(i));
						*/
					}
				}
				seq = 0;
			}
		}
		else
		{
			if (seq == 0) /* means there is ONLY ONE CONSTRUCTOR in this datatype */
			{
				rmldb_LISTty_t* rmlLISTty_node = rmldb_get_type_components(i);
				/* means is not a constant constructor OR a type > 2 */
				/* must be exactly 1 slot and 1 constructor in the datatype
				 * datatype varTys ident = CONSTRUCTOR tys */
				if (rmlLISTty_node != NULL && rmlLISTty_node->length == 1)  
				{
					i->is_transparent = 1;
					/*
					fprintf(stderr,"TRANSPARENT on %s->%s\n", i->name, rmldb_get_type_id(i));
					*/				
				}
			}
		}
	}
	/*
	fprintf(stderr,"\n---- TYPES----\n");
	for(i = rmldb_con_db_start; i; i = i->next)
	{
		fprintf(stderr,"%s:%d.%d.%d.%d | %s->%s[%d][",
			i->file,
			i->range->sl,i->range->sc,i->range->el,i->range->ec,
			rmldb_get_type_id(i), 
			i->name,
			i->constructor);
		rmldb_print_type_list(rmldb_get_type_components(i), 0, ", ", stderr);
		fprintf(stderr,"]\n");
	}
	*/
#endif /* RML_DEBUG */	
}

extern int rmldb_load_db(char** programdb)
{
#ifdef RML_DEBUG
	if (rml_debug_enabled)
	{
		char line[RMLDB_MAX_STRING];  /* declare a char array */
		rmldb_var_db_t *rmldb_var_db_node;
		struct rmldb_type_db *rmldb_type_db_node;
		struct rmldb_con_db *rmldb_con_db_node;
		struct rmldb_relation_db *rmldb_relation_db_node;
		int i=0;
	
		while (programdb[i])
		{
	    	aarmldb_scan_string(programdb[i]);
		  	  if (aarmldbparse())
				fprintf(stderr, "%s parsing of program type database failed!\n", RMLDB_PROMPT); 
	    	i++;
	  	}
	  	rmldb_order_rmldb_con_db();
	}
#endif /* RML_DEBUG */
  	return 0;
}
