// for testing each function in the new C code

#include <stdio.h>

int main(void)
{

//   int i;
//
//   printf("**********************\n");
//   printf("utils.h check script\n");
//   printf("**********************\n");
//
//
//   // online
//   // tests if a point is on a line
//   //
//   printf("*** online\n");
//   int res;
//
//   // create point and line
//   double p1[2]={0,0};
//
//   double thisline[2][2]={1,1,-1,-1};
//
//   // see if the line y=x passes through the origin
//   res=online(p1,thisline);
//   printf("1=%i\n",res);
//
//
//   // see if the line y=x passes through (8,3)
//   p1[0]=8;
//   p1[1]=3;
//   res=online(p1,thisline);
//   printf("0=%i\n",res);
//
//
//   // see if the line y=1 passes through (1,1)
//   // (horizontal line check 1)
//   thisline[0][0]=0;
//   thisline[0][1]=1;
//   thisline[1][0]=5;
//   thisline[1][1]=1;
//   p1[0]=1;
//   p1[1]=1;
//   res=online(p1,thisline);
//   printf("1=%i (horiz check 1)\n",res);
//
//
//   // see if the line y=1 passes through (3,4)
//   // (horizontal line check 2)
//   p1[0]=3;
//   p1[1]=4;
//   res=online(p1,thisline);
//   printf("0=%i (horiz check 2)\n",res);
//
//   // see if the line x=1 passes through (1,1)
//   thisline[0][0]=1;
//   thisline[0][1]=0;
//   thisline[1][0]=1;
//   thisline[1][1]=5;
//   p1[0]=1;
//   p1[1]=1;
//   res=online(p1,thisline);
//   printf("1=%i (vert check 1)\n",res);
//
//   // see if the line x=1 passes through (4,6)
//   p1[0]=4;
//   p1[1]=6;
//   res=online(p1,thisline);
//   printf("0=%i (vert check 2)\n",res);
//
//
//   // intpoint
//   // find the intersection point
//   printf("*** intpoint\n");
//   double p2[2]={-1,1};
//   p1[0]=1;p1[1]=-1;
//
//   thisline[0][0]=1;
//   thisline[0][1]=1;
//   thisline[1][0]=-1;
//   thisline[1][1]=-1;
//
//   double intpointret[2];
//   intpoint(p1,p2,thisline,intpointret);
//
//   printf("(0,0)=(%f, %f)\n",intpointret[0],intpointret[1]);
// 
//   // NB does not detect if you don't intersect  
//   // should fail
//   
//   
//   
//
//   // dointersect 
//   printf("*** dointersect\n");
//   p1[0]=-1;p1[1]=0;
//
////void *dointersect(double p1[2], double p2[2], int nbnd, double bnd[nbnd][2],int bndint[nbnd-1])
//   
//   // create a simple boundary
//   // just the unit square
//   double bnd[5][2];
//   bnd[0][0]=-1;bnd[0][1]=1;
//   bnd[1][0]=1;bnd[1][1]=1;
//   bnd[2][0]=1;bnd[2][1]=-1;
//   bnd[3][0]=-1;bnd[3][1]=-1;
//   bnd[4][0]=bnd[0][0];bnd[4][1]=bnd[0][1];
//   int nbnd =5;
//
//   int bndint[(nbnd-1)];
//
//   // first off, something totally in the box
//   p1[1]=-0.5;p1[0]=-0.5;
//   p2[1]=0.5;p2[0]=0.5;
// 
//   do_intersect(p1,p2,nbnd, bnd, bndint);
//
//   printf("(0,0,0,0)=(");
//   for(i=0;i<(nbnd-1);i++){
//      printf("%d,",bndint[i]);
//   }
//   printf(")\n");
//
//
//   // horizontal 
//   p1[1]=0;p1[0]=-5;
//   p2[1]=0;p2[0]=5;
// 
//   do_intersect(p1,p2,nbnd, bnd, bndint);
//
//   printf("(0,1,0,1)=(");
//   for(i=0;i<(nbnd-1);i++){
//      printf("%d,",bndint[i]);
//   }
//   printf(") (horiz)\n");
//
//
//   // general 
//   p1[0]=-0.5;p1[1]=-2;
//   p2[0]=0.5;p2[1]=2;
// 
//   do_intersect(p1,p2,nbnd, bnd, bndint);
//
//   printf("(1,0,1,0)=(");
//   for(i=0;i<(nbnd-1);i++){
//      printf("%d,",bndint[i]);
//   }
//   printf(") (general)\n");
//
//   // vertical 
//   p1[1]=-5;p1[0]=0;
//   p2[1]=5;p2[0]=0;
// 
//   do_intersect(p1,p2,nbnd, bnd, bndint);
//
//   printf("(1,0,1,0)=(");
//   for(i=0;i<(nbnd-1);i++){
//      printf("%d,",bndint[i]);
//   }
//   printf(") (vert)\n");
//
//   // hull_length
////   printf("*** hull_length\n");
////   printf("8=%f\n",hull_length(5,bnd));
//
//
//
//   // facing
//   printf("*** facing\n");
//
   //////////// EVERYTHING
   int nbnd;
   nbnd=9;
   double xbnd[nbnd];
   double ybnd[nbnd];

   xbnd[0]=0; 
   xbnd[1]=0; 
   xbnd[2]=1; 
   xbnd[3]=1; 
   xbnd[4]=0.3; 
   xbnd[5]=0.3; 
   xbnd[6]=1; 
   xbnd[7]=1; 
   xbnd[8]=xbnd[0]; 

   ybnd[0]=0; 
   ybnd[1]=1; 
   ybnd[2]=1; 
   ybnd[3]=0.6; 
   ybnd[4]=0.6; 
   ybnd[5]=0.3; 
   ybnd[6]=0.3; 
   ybnd[7]=0; 
   ybnd[8]=ybnd[0]; 

   int i;

	printf("bnd<-list(x=c(),y=c())\n");
	for(i=0;i<9;i++){
		printf("bnd$x<-c(bnd$x,%f)\n",xbnd[i]);
	}
	for(i=0;i<9;i++){
		printf("bnd$y<-c(bnd$y,%f)\n",ybnd[i]);
	}

   double p1[2], p2[2];
   p2[0]=0.5;
   p2[1]=0.7;
   p1[0]=0.4;
   p1[1]=0.2;

   double ret=0;
   wood_path(&p1,&p2,&nbnd,&xbnd,&ybnd,&ret);

   printf("path length is %f\n",ret);

   return 0;
}
