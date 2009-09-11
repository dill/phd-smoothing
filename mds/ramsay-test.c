// for testing each function in the new C code

#include <stdio.h>

int main(void)
{

   //////////// EVERYTHING
   int nbnd, len, i;
   double *xbnd, *ybnd, **bnd, *x, *y, *ret;

   len=4;

   nbnd=21;
   bnd=(double**)malloc(sizeof(double*)*(nbnd));
   bnd[0]=(double*)malloc(sizeof(double)*(nbnd*2));

   for(i=0; i<nbnd; i++){
      bnd[i]=bnd[0]+i*2;
   }

   xbnd=(double*)malloc(sizeof(double)*nbnd);
   ybnd=(double*)malloc(sizeof(double)*nbnd);
   x=(double*)malloc(sizeof(double)*nbnd);
   y=(double*)malloc(sizeof(double)*nbnd);

   for(i=0; i<nbnd; i++){
      xbnd[i]=xbnd[0]+i;
      ybnd[i]=ybnd[0]+i;
   }

   ret=(double*)malloc(sizeof(double)*(len*len-len)/2);

   for(i=0; i<(len*len-len)/2; i++){
      ret[i]=ret[0]+i;
   }

   bnd[0][0]=-0.9;
   bnd[1][0]=-0.7102265;
   bnd[2][0]=-0.2209369;
   bnd[3][0]=3.126667;
   bnd[4][0]=3.329194;
   bnd[5][0]=3.399676;
   bnd[6][0]=3.309842;
   bnd[7][0]=3.095726;
   bnd[8][0]=-0.03246995;
   bnd[9][0]=-0.08371665;
   bnd[10][0]=-0.1;
   bnd[11][0]=-0.07891405;
   bnd[12][0]=-0.02454855;
   bnd[13][0]=3.126667;
   bnd[14][0]=3.329194;
   bnd[15][0]=3.399676;
   bnd[16][0]=3.309842;
   bnd[17][0]=3.095726;
   bnd[18][0]=-0.2922295;
   bnd[19][0]=-0.7534498;
   bnd[20][0]=-0.9;
   
   bnd[0][1]=1.102146e-16;
   bnd[1][1]=0.5527914;
   bnd[2][1]=0.8724602;
   bnd[3][1]=0.8794146;
   bnd[4][1]=0.7272259;
   bnd[5][1]=0.4838936;
   bnd[6][1]=0.2470218;
   bnd[7][1]=0.1116233;
   bnd[8][1]=0.09458172;
   bnd[9][1]=0.05469482;
   bnd[10][1]=-1.224606e-17;
   bnd[11][1]=-0.06142127;
   bnd[12][1]=-0.09694003;
   bnd[13][1]=-0.1205854;
   bnd[14][1]=-0.2727741;
   bnd[15][1]=-0.5161064;
   bnd[16][1]=-0.7529782;
   bnd[17][1]=-0.8883767;
   bnd[18][1]=-0.8512355;
   bnd[19][1]=-0.4922533;
   bnd[20][1]=1.102146e-16;



   for(i=0;i<nbnd;i++){
      xbnd[i]=bnd[i][0];
   }
   xbnd[20]=bnd[0][0];

   for(i=0;i<nbnd;i++){
      ybnd[i]=bnd[i][1];
   }
   ybnd[20]=bnd[0][1];


//   printf("bnd<-list(x=c(),y=c())\n");
//   for(i=0;i<nbnd;i++){
//   	printf("bnd$x<-c(bnd$x,%f)\n",xbnd[i]);
//   }
//   for(i=0;i<nbnd;i++){
//   	printf("bnd$y<-c(bnd$y,%f)\n",ybnd[i]);
//   }

//   p2[0]=0.5;
//   p2[1]=0.7;
//   p1[0]=0.4;
//   p1[1]=0.2;

   x[0]=-0.386364;
   y[0]=0.333333;
   x[1]=0.329545;
   y[1]=-0.5;

//   x[2]=-0.284091;
//   y[2]=0.500000;
//   x[3]=0.022727;
//   y[3]=-0.333333;

   x[2]=0.329545;
   y[2]=0.750000;
   x[3]=-0.386364;
   y[3]=-0.500000;


   wood_path(&len,x,y,&nbnd,xbnd,ybnd,ret);

   printf("path length is %f\n",ret[0]);


   free(bnd[0]);
   free(bnd);
   free(xbnd);
   free(ybnd);
   free(x);
   free(y);
   free(ret);

   return 0;
}
