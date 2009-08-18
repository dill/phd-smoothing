#!/bin/sh

# -O2 needed for weird things not to happen in Mac...

# pseudo-Ramsay test
#gcc -lm -g test.c inout.c wood.c

# w-shaped area
#gcc -lm -g -O2 doubleyuh-test.c inout.c wood.c 

# w-shaped area - alter step debugging (facing mostly)
gcc -lm -g -O2 alterdebug.c inout.c wood.c 

# linked list test
#gcc -lm -g ll-test.c inout.c 

# shared object file
gcc -lm -g -std=gnu99 -shared inout.c wood.c -o wood.so -L/usr/lib/R/lib  -fPIC -O2 #-lR
#rm *.o
