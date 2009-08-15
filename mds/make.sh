#!/bin/sh

# pseudo-Ramsay test
#gcc -lm -g test.c inout.c wood.c

# w-shaped area
gcc -lm -g doubleyuh-test.c inout.c wood.c 

# linked list test
#gcc -lm -g ll-test.c inout.c 

# shared object file
#gcc -lm -g -std=gnu99 -shared inout.c wood.c -o wood.so -L/usr/lib/R/lib  -fPIC #-lR
#rm *.o
