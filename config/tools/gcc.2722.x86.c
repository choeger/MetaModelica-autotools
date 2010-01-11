/* bugs/gcc.2722.x86.c
 * This program triggers a bug in gcc 2.7.2.2 for the Intel x86 architecture.
 * (The bug has been present at least since 2.6.3.)
 * When compiled correctly, the program should produce no output.
 * Any output on the form "X is Y, should be Z" indicates an error.
 * On the Intel x86, the erroneous output is:
 *
 *	SC[3] is 0x0000000B, should be 0x00000061
 *
 * In the `update' function below, gcc schedules the load of A1 at 2a
 * and store at 2b BEFORE the load of SC1 at 1a and store at 1b.
 * The store at 2b CLOBBERS the original value of SC1, causing the
 * load at 1a to pick up the wrong value, and consequently store
 * the wrong value at 1b.
 *
 * I originally reported this to bug-gcc in the fall of 1995.
 * Richard Kenner emailed a fix to me in january 1996, but looking
 * at the 2.7.2.2 sources, I see that the fix hasn't been included yet.
 */
#include <stdio.h>

struct state {
    void **SP;
    void **FC;
    void **SC;
    void *ARGS[10];
    void *STACK[100];
};

void update(struct state *state) 
{
    void **SC = state->SC;
    void *SC1 = SC[1];		/*1a*/
    void *SC2 = SC[2];
    void *SC3 = SC[3];
    void *A0 = state->ARGS[0];
    void *A1 = state->ARGS[1];	/*2a*/
    SC[3] = SC1;		/*1b*/
    SC[2] = SC3;
    SC[1] = A1;			/*2b*/
    SC[0] = (void*)5;
    state->ARGS[1] = A0;
    state->ARGS[0] = SC3;
    state->FC = (void**)SC2;
    state->SP = SC;
}

void check(const char *name, void *now, void *expected)
{
    if( now != expected )
	fprintf(stderr, "%s is 0x%08X, should be 0x%08X\n", name, now, expected);
}

struct state state;

int main(void)
{
    state.STACK[99] = (void*)99;
    state.STACK[98] = (void*)98;
    state.STACK[97] = (void*)97;
    state.STACK[96] = (void*)96;
    state.SC = &state.STACK[96];
    state.ARGS[1] = (void*)11;
    state.ARGS[0] = (void*)10;
    update(&state);
    check("SC", (void*)state.SC, (void*)&state.STACK[96]);
    check("FC", (void*)state.FC, (void*)98);
    check("SP", (void*)state.SP, (void*)&state.STACK[96]);
    check("SC[3]", state.SC[3], (void*)97);
    check("SC[2]", state.SC[2], (void*)99);
    check("SC[1]", state.SC[1], (void*)11);
    check("SC[0]", state.SC[0], (void*)5);
    check("ARGS[1]", state.ARGS[1], (void*)10);
    check("ARGS[0]", state.ARGS[0], (void*)99);
    return 0;
}
