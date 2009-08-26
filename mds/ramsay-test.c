// for testing each function in the new C code

#include <stdio.h>

int main(void)
{

   //////////// EVERYTHING
   int nbnd;
   nbnd=21;
   double bnd[nbnd][2];

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


   double xbnd[nbnd];
   double ybnd[nbnd];

   xbnd[0]=bnd[0][0];
   xbnd[1]=bnd[1][0];
   xbnd[2]=bnd[2][0];
   xbnd[3]=bnd[3][0];
   xbnd[4]=bnd[4][0];
   xbnd[5]=bnd[5][0];
   xbnd[6]=bnd[6][0];
   xbnd[7]=bnd[7][0];
   xbnd[8]=bnd[8][0];
   xbnd[9]=bnd[9][0];
   xbnd[10]=bnd[10][0];
   xbnd[11]=bnd[11][0];
   xbnd[12]=bnd[12][0];
   xbnd[13]=bnd[13][0];
   xbnd[14]=bnd[14][0];
   xbnd[15]=bnd[15][0];
   xbnd[16]=bnd[16][0];
   xbnd[17]=bnd[17][0];
   xbnd[18]=bnd[18][0];
   xbnd[19]=bnd[19][0];
   xbnd[20]=bnd[20][0];

   ybnd[0]=bnd[0][1];
   ybnd[1]=bnd[1][1];
   ybnd[2]=bnd[2][1];
   ybnd[3]=bnd[3][1];
   ybnd[4]=bnd[4][1];
   ybnd[5]=bnd[5][1];
   ybnd[6]=bnd[6][1];
   ybnd[7]=bnd[7][1];
   ybnd[8]=bnd[8][1];
   ybnd[9]=bnd[9][1];
   ybnd[10]=bnd[10][1];
   ybnd[11]=bnd[11][1];
   ybnd[12]=bnd[12][1];
   ybnd[13]=bnd[13][1];
   ybnd[14]=bnd[14][1];
   ybnd[15]=bnd[15][1];
   ybnd[16]=bnd[16][1];
   ybnd[17]=bnd[17][1];
   ybnd[18]=bnd[18][1];
   ybnd[19]=bnd[19][1];
   ybnd[20]=bnd[20][1];

   int i;

	printf("bnd<-list(x=c(),y=c())\n");
	for(i=0;i<nbnd;i++){
		printf("bnd$x<-c(bnd$x,%f)\n",xbnd[i]);
	}
	for(i=0;i<nbnd;i++){
		printf("bnd$y<-c(bnd$y,%f)\n",ybnd[i]);
	}

   double p1[2], p2[2];
//   p2[0]=0.5;
//   p2[1]=0.7;
//   p1[0]=0.4;
//   p1[1]=0.2;

   p1[0]=-0.386364;
   p1[1]=0.333333;
   p2[0]=0.329545;
   p2[1]=-0.5;

//   p1[0]=-0.284091;
//   p1[1]=0.500000;
//   p2[0]=0.022727;
//   p2[1]=-0.333333;


//   int bndint[nbnd-1];
//
//   do_intersect(p1,p2,nbnd,bnd,bndint);
//
//   printf("bndint= ");
//
//   for(i=0;i<(nbnd-1);i++){
//      printf("%d",bndint[i]);
//   }
//   printf("\n");

   int err;
   int intind[2];
   double ip1[2], ip2[2];

   err=first_ips(p1, p2, nbnd, bnd, ip1, ip2, intind);

   printf("err=%d\n",err);
   printf("ip1=list(x=%f,y=%f)\n",ip1[0],ip1[1]);
   printf("ip2=list(x=%f,y=%f)\n",ip2[0],ip2[1]);
   printf("intind= %d %d\n",intind[0], intind[1]);


   double ret=0;
   wood_path(&p1,&p2,&nbnd,&xbnd,&ybnd,&ret);

   printf("path length is %f\n",ret);

   return 0;
}
