#!/bin/sh

# -O2 needed for weird things not to happen in Mac...

# pseudo-Ramsay test
#gcc -lm -g test.c inout.c wood.c

# Ramsay test
#gcc -Wall -pedantic -lm -g -O0 -std=gnu99 inout.c utils.c wood.c ramsay-test.c

# w-shaped area
#gcc -lm -g -O0 doubleyuh-test.c inout.c wood.c 

# w-shaped area - alter step debugging (facing mostly)
#gcc -lm -g alterdebug.c inout.c wood.c 

# linked list test
#gcc -lm -g ll-test.c inout.c 

# shared object file
rm wood.so
#gcc  -Wall -O0 -pedantic -lm -g -std=gnu99 -shared inout.c utils.c wood.c -o wood.so -L/usr/lib/R/lib  -fPIC #-lefence #-lR
gcc  -Wall -O0 -pedantic -lm -g -std=gnu99 -shared inout.c utils.c wood.c -o wood.so  -fPIC

# optimised...
#gcc  -Wall -O2 -pedantic -lm -std=gnu99 -shared inout.c utils.c wood.c -o wood.so  -fPIC

# valgrind to work on snow leopard?
#gcc -m32 -Wall -O0 -pedantic -lm -g -std=gnu99 -shared inout.c utils.c wood.c -o wood.so  -fPIC
