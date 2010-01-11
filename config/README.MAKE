Each target is supposed to have a "variables.mk" file, in
which idiosyncrasies of C compilers are taken care of.

# Name of an ANSI-C compiler, or at least a late-model K&R C
# compiler that understands prototypes. Usually "cc" or "gcc".
CC=???

# Common flags when invoking the C compiler.
CFLAGS=???

# Flags for enabling optimization. Usually "-O".
COFLAGS=???

# Flags for enabling profiling. Usually "-p" or "-pg".
CPFLAGS=???

# Flags for enabling debugging. Usually "-g".
CGFLAGS=???

# Name of a command for making a newly constructed "ar" archive
# acceptable to the linker. Usually "ranlib" or "echo".
RANLIB=???

# Command for invoking a "cpp"-like preprocessor, via "$(CPP) -P file".
# Usually "cpp" or "gcc -x c -E -ansi"
CPP=???

# Command for invoking a non-preprocessing assembler via
# "$(AS) -o f.o f.s". Usually "as" or "as -nocpp".
AS=???

# Name of a linker that can combine two object files into a third one,
# via "$(LD) -r -o f3.o f1.o f2.o". Usually "ld".
LD=???

# The 'diff' code generation scheme needs to construct a vector of
# label objects for each module, where some of those labels also
# have global names. This cannot be done portably and efficiently in C.
# To compile a generated C file f.c to f.o, the following is done:
# 1. The C compiler compiles f.c to a preliminary f.o.
# 2. fgrep is used to select certain macro calls from f.c into a
#    temporary "table file".
# 3. The table file is preprocessed using $CPP and an OS-specific
#    .h-file into an assembly file.
# 4. The assembly file is assembled into an object file, which now
#    contains the desired data structures and names.
# 5. Using $LD -r, the object file is combined with the preliminary f.o;
#    f.o is then replaced with this final f.o.
# Due to limitations in some systems, it may be necessary to apply
# further rewrites to the assembly code produced in step 3 before it
# is assembled. The ASLABTABFIX variable should either be undefined,
# or defined as a pipeline component "| filter". In the latter case,
# the filter is used to "fix" the assembly code before it is assembled.
# Only ports that actually need this fix should define ASLABTABFIX.
ASLABTABFIX=???

# End of "variables.mk" parameters.
