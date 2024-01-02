Compiling and loading the list-C evaluator and parser (file ListC/README)
-------------------------------------------------------------------------


Garbage collection opgaver instrucitoner:
0. fslex --unicode CLex.fsl; if ($?) { fsyacc --module CPar CPar.fsy }
1. Cd ind i assignment9/listVM/listVM
2. gcc -o listmachine listmachine.c
3. ./listmachine ../../ex30.out 1000


Building the list-C command line compiler listcc:

   fslex --unicode CLex.fsl
   fsyacc --module CPar CPar.fsy

   fsc --standalone -r ~/fsharp/FsLexYacc.Runtime.dll Absyn.fs CPar.fs CLex.fs Parse.fs Machine.fs Comp.fs ListCC.fs -o listcc.exe
      or if you have fsharpc as the compiler instead of fsc, e.g., Mono on Mac:
   fsharpc --standalone -r ~/fsharp/FsLexYacc.Runtime.dll Absyn.fs CPar.fs CLex.fs Parse.fs Machine.fs Comp.fs ListCC.fs -o listcc.exe

    Jacob Desktop:
    fslex --unicode CLex.fsl; if ($?) { fsyacc --module CPar CPar.fsy }


Using the list-C command line compiler to compile program ex30.lc
to listmachine code (in file ex30.out):

   ListCC ex30.lc
     or
   mono listcc.exe ex30.lc


On Unix you compile with gcc -o listmachine listmachine.c and use the
executable.

Run examples (assuming listmachine is a symbolic link to ListVM/ListVM/listmachine.exe)

   ./listmachine ex30.out 8

 
