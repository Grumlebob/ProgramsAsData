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
    fsc --standalone -r C:\Users\jgrum\Documents\Programming\FSharp\LexAndYacc\FsLexYacc.Runtime.dll Absyn.fs CPar.fs CLex.fs Parse.fs Machine.fs Comp.fs ListCC.fs -o listcc.exe
    (This compiles our changes/ListCC)
    
Using the list-C command line compiler to compile program ex30.lcto listmachine code (in file ex30.out):
AKA: How to generate .OUT file:
   .\listcc.exe .\2021Opg3.lc


Run examples (assuming listmachine is a symbolic link to ListVM/ListVM/listmachine.exe)
   ./listmachine ex30.out 8

 
