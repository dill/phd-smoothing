// for testing each function in the new C code

#include <stdio.h>

int main(void)
{

   //////////// EVERYTHING
   int nbnd;
   nbnd=11;
   double bnd[nbnd][2];

   bnd[0][0]=0; 
   bnd[1][0]=0; 
   bnd[2][0]=1; 
   bnd[3][0]=1; 
   bnd[4][0]=0.3; 
   bnd[5][0]=0.3; 
   bnd[6][0]=1; 
   bnd[7][0]=1; 
   bnd[8][0]=bnd[0][0]; 

   bnd[0][1]=0; 
   bnd[1][1]=1; 
   bnd[2][1]=1; 
   bnd[3][1]=0.6; 
   bnd[4][1]=0.6; 
   bnd[5][1]=0.3; 
   bnd[6][1]=0.3; 
   bnd[7][1]=0; 
   bnd[8][1]=bnd[0][0]; 

   int i;
   double p1[2], p2[2];

   p1[0]=5.333916; p1[1]=2.946539;
   p2[0]=2.437534; p2[1]=0.686418;

   printf("facing=%d\n",facing(p1,p2,nbnd,bnd));

   return 0;
}
