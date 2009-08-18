// for testing each function in the new C code

#include <stdio.h>

int main(void)
{

   //////////// EVERYTHING
   int nbnd;
   nbnd=11;
   double xbnd[nbnd];
   double ybnd[nbnd];

   xbnd[0]=0.5;
   xbnd[1]=2;
   xbnd[2]=3;
   xbnd[3]=4;
   xbnd[4]=6;
   xbnd[5]=5.5;
   xbnd[6]=4;
   xbnd[7]=3;
   xbnd[8]=2;
   xbnd[9]=1;
   xbnd[10]=xbnd[0];
   
   ybnd[0]=4;
   ybnd[1]=0;
   ybnd[2]=1.3;
   ybnd[3]=0;
   ybnd[4]=4;
   ybnd[5]=4;
   ybnd[6]=1;
   ybnd[7]=2;
   ybnd[8]=1;
   ybnd[9]=4;
   ybnd[10]=ybnd[0];


   int i;

	printf("bnd<-list(x=c(),y=c())\n");
	for(i=0;i<nbnd;i++){
		printf("bnd$x<-c(bnd$x,%f)\n",xbnd[i]);
	}
	for(i=0;i<nbnd;i++){
		printf("bnd$y<-c(bnd$y,%f)\n",ybnd[i]);
	}


//5.3339164,2.9417190,3.9715439,3.2099026,2.1371683,1.4398911,0.8069778,2.4375339,3.9929986,5.5913726

//2.9465388,1.6504505,0.6649949,1.0934539,0.7399752,1.9396603,3.8355911,0.6864179,0.1722671,3.8570141


   double p1[2], p2[2];
//   p1[0]=5.3339164; p2[0]=1.4398911;
//   p1[1]=2.9465388; p2[1]=1.9396603;

   p1[0]=5.3339164; p2[0]=2.4375339;
   p1[1]=2.9465388; p2[1]=0.6864179;

   double ret=0;
   wood_path(&p1,&p2,&nbnd,&xbnd,&ybnd,&ret);

   printf("path length is %f\n",ret);

   return 0;
}
