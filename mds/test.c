// for testing each function in the new C code

#include <stdio.h>
#include "utils.h"

int main(void)
{

   printf("**********************\n");
   printf("utils.c check script\n");
   printf("**********************\n");


   // online
   // tests if a point is on a line
   //
   printf("*** online\n");
   int res;

   // create point and line
   double p1[2]={0,0};

   double thisline[2][2]={1,1,-1,-1};

   // see if the line y=x passes through the origin
   res=online(p1,thisline);
   printf("1=%i\n",res);


   // see if the line y=x passes through (8,3)
   p1[0]=8;
   p1[1]=3;
   res=online(p1,thisline);
   printf("0=%i\n",res);


   // see if the line y=1 passes through (1,1)
   // (horizontal line check 1)
   thisline[0][0]=0;
   thisline[0][1]=1;
   thisline[1][0]=5;
   thisline[1][1]=1;
   p1[0]=1;
   p1[1]=1;
   res=online(p1,thisline);
   printf("1=%i (horiz check 1)\n",res);


   // see if the line y=1 passes through (3,4)
   // (horizontal line check 2)
   p1[0]=3;
   p1[1]=4;
   res=online(p1,thisline);
   printf("0=%i (horiz check 2)\n",res);

   // see if the line x=1 passes through (1,1)
   thisline[0][0]=1;
   thisline[0][1]=0;
   thisline[1][0]=1;
   thisline[1][1]=5;
   p1[0]=1;
   p1[1]=1;
   res=online(p1,thisline);
   printf("1=%i (vert check 1)\n",res);

   // see if the line x=1 passes through (4,6)
   p1[0]=4;
   p1[1]=6;
   res=online(p1,thisline);
   printf("0=%i (vert check 2)\n",res);


   // intpoint
   // find the intersection point
   printf("*** intpoint\n");
   double p2[2]={-1,1};
   p1[0]=1;p1[1]=-1;

   thisline[0][0]=1;
   thisline[0][1]=1;
   thisline[1][0]=-1;
   thisline[1][1]=-1;

   point intpointret;
   intpointret=intpoint(p1,p2,thisline);

   printf("(0,0)=(%f, %f)\n",intpointret.x,intpointret.y);
   
// more tests





   return 0;
}
