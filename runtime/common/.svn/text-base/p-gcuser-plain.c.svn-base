/* p_gcuser.c
 * mutator interface to the collector
 */
#include <stdio.h>
#include "rml.h"
#include "z-ysize.h"
#include "p-gccore.h"

void *rml_prim_gcalloc(rml_uint_t nwords, rml_uint_t nargs) {
  void **p;
  if (rml_flag_gclog)
    rml_gc_start_clock = rml_prim_clock();
  rml_minor_collection(nargs);
  if (nwords > rml_young_size) /* RML_YOUNG_SIZE ) */
  {
    if ( (p = rml_older_alloc(nwords, nargs)) != 0) {
      rml_state_young_next = rml_young_region;
    } else {
      fprintf(stderr, "rml_prim_gcalloc failed to get %lu words\n",
          (unsigned long)nwords);
      rml_exit(1);
    }
  } else {
    p = rml_young_region;
    rml_state_young_next = p + nwords;
  }
  if (rml_flag_gclog) {
    rml_gc_end_clock = rml_prim_clock();
    rml_gc_total_time += (double)(rml_gc_end_clock - rml_gc_start_clock)
        / (double)RML_CLOCKS_PER_SEC;
  }
  return (void*)p;
}

void *rml_prim_alloc(rml_uint_t nwords, rml_uint_t nargs) {
#if	defined(RML_STATE_APTR) || defined(RML_STATE_LPTR)
  struct rml_state *rmlState = &rml_state;
#endif	/*RML_STATE_APTR || RML_STATE_LPTR*/
  void **p= rml_young_next;
  if ( (rml_young_next = p + nwords) >= rml_young_limit)
    p = rml_prim_gcalloc(nwords, nargs);
  return p;
}
