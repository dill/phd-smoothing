// for testing each function in the new C code

#include <stdio.h>

int main(void)
{

   //////////// EVERYTHING
   int nbnd;
   nbnd=11;
   double bnd[nbnd][2];

   bnd[0][0]=0.5;
   bnd[1][0]=2;
   bnd[2][0]=3;
   bnd[3][0]=4;
   bnd[4][0]=6;
   bnd[5][0]=5.5;
   bnd[6][0]=4;
   bnd[7][0]=3;
   bnd[8][0]=2;
   bnd[9][0]=1;
   bnd[10][0]=bnd[0][0];

   bnd[0][1]=4;
   bnd[1][1]=0;
   bnd[2][1]=1.3;
   bnd[3][1]=0;
   bnd[4][1]=4;
   bnd[5][1]=4;
   bnd[6][1]=1;
   bnd[7][1]=2;
   bnd[8][1]=1;
   bnd[9][1]=4;
   bnd[10][1]=bnd[0][1];

   int i;
   double p1[2], p2[2];
   double ip1[2], ip2[2];

// actual points
//   p1[0]=5.333916; p1[1]=2.946539;
//   p2[0]=2.437534; p2[1]=0.686418;

   // eps
   p1[0]=5.333916; p1[1]=2.946539;
   p2[0]=3; p2[1]=1.300000;

   printf("facing=%d\n",facing(p1,p2,nbnd,bnd));

//   int ret;
//
//   ret=first_ips(p1,p2,nbnd,bnd,ip1,ip2);
//
//   printf("first_ips=%d\n",ret);
//
//   printf("ip1=list(x=%f,y=%f)\nip2=list(x=%f,y=%f)\n",ip1[0],ip1[1],ip2[0],ip2[1]);

//   int retint[nbnd];
//
//   do_intersect(p1,p2,nbnd,bnd,retint);
//
//   // DEBUG
//   printf("ex retint=");
//   int k;
//   for(k=0;k<(nbnd-1);k++){
//      printf(" %d,",retint[k]);
//   }
//   printf("\n");



   return 0;
}
