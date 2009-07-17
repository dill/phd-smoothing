/* First hash at re-writing the utils.R script in C */

#include <math.h>


double *twosort(double[2]);
int online(double[2],double[2][2]);
//int facing(double[2], double[2] , int, double[][2])





/* determine whether the line between two points is facing inside or outside */
//int *facing(double p1[2], double p2[2] , int nbnd, double bnd[nbnd][2]){
//   /*
//   Args:
//      p1, p2      the points
//      nbnd        length of the boundary
//      bnd         the boundary
//      Return:
//            (ref to) logical 2-vector, TRUE= facing inside
//                                  FALSE= facing outside
//   */
//
//   int ret[2], bbindex;
//
//
//   /*
//   there might be no intersections, so if there aren't
//   then just return FALSE and let the delete step take 
//   care of it 
//   */
//   ret[2]={0,0}; 
//
///// DO SOMETHING WITH THIS
//   doint=dointersect(p1,p2,bnd);
//
//
//
///// check that this makes sense
//   if(sum(doint)>1){
//
//      // find intersections & sort by distance
//      bbindex=c(1:(length(bnd$x)-1))[doint]
//      // hold distances and intersection points temporarily
//      dists<-c(); ips<-list(x=c(),y=c())
//
//      for(j in bbindex){
//         # calculate and save the intersection
//         ip<-intersection_point(p1,p2,pe(bnd,c(j,j+1)))
//         ips$x<-c(ips$x,ip$x)
//         ips$y<-c(ips$y,ip$y)
//
//         # find the distance and save
//         dists<-c(dists,sqrt((p1$x-ip$x)^2+(p1$y-ip$y)^2))
//      }
//
//      # remove duplicates (ie when dist is zero)
//      if(length(ips$x)>3){
//         p1.ind<-which((ips$x==p1$x)&(ips$y==p1$y))
//         p2.ind<-which((ips$x==p2$x)&(ips$y==p2$y))
//         nonzero<-c(p1.ind,p2.ind)
//         if(length(nonzero)!=0 & length(nonzero)!=length(dists)){
//            dists<-dists[-nonzero]
//            bbindex<-bbindex[-nonzero]
//            ips$x<-ips$x[-nonzero];ips$y<-ips$y[-nonzero]
//         }
//      }
//
//
//      # find first intersection between p1 and bnd
//      p1.int<-pe(ips,order(dists)[1])
//      # find first intersection between p2 and bnd
//      p2.int<-pe(ips,order(dists,decreasing=TRUE)[1])
//
//      # midpoint between p1 and first intersection 
//      p1.mp<-list(x=(p1.int$x+p1$x)/2,y=(p1.int$y+p1$y)/2)
//      # midpoint between p2 and first intersection 
//      p2.mp<-list(x=(p2.int$x+p2$x)/2,y=(p2.int$y+p2$y)/2)
//
//      # are the midpoints inside?
//      ret<-inSide(bnd,c(p1.mp$x,p2.mp$x),c(p1.mp$y,p2.mp$y))
//   }
//   return(ret)
//}

typedef struct
{
   double x,y;
} point;


point intpoint(double[2], double[2],double[2][2]);
// find the intersection point between two points and a line
point intpoint(double p1[2], double p2[2],double edge[2][2])
{
   /*args:
      p1, p2      the two points making up the end points of the line
      line         boundary of the shape to test intersection with
     return:
        intersection point
   */

   double eps,a1,b1,c1,a2,b2,c2;
   point ret;

   eps=1.0e-16;

   // calculation of intersection is straight from 
   // Handbook of Mathematics Bronstein et al. pp. 195,196

   /// calculate intersection point
   //   first calculate the coefficients for the lines
   //   the line between the points
   a1=-1/(p2[0]-p1[0]);
   b1=1/(p2[1]-p1[1]);
   c1=p1[0]/(p2[0]-p1[0])-p1[1]/(p2[1]-p1[1]);

   // the edge
   a2=-1/(edge[1][0]-edge[0][0]);
   b2=1/(edge[1][1]-edge[0][1]);
   c2= edge[0][0]/(edge[1][0]-edge[0][0])-edge[0][1]/(edge[1][1]-edge[0][1]);

   // handle the horizontal/vertical line case...

   // point line horizontal
   if(fabs((p2[1]-p1[1])/(p2[0]-p1[0]))<eps){
      a1=0;
      b1=1;
      c1=-p1[1];
   // point line vertical
   }else if(fabs((p2[0]-p1[0])/(p2[1]-p1[1]))<eps){
      a1=1;
      b1=0;
      c1=-p1[0];
   // edge horizontal
   }else if(fabs((edge[1][1]-edge[0][1])/(edge[1][0]-edge[0][0]))<eps){
      a2=0;
      b2=1;
      c2=-edge[0][1];
   // edge vertical
   }else if(fabs((edge[1][0]-edge[0][0])/(edge[1][1]-edge[0][1]))<eps){
      a2=1;
      b2=0;
      c2=-edge[0][0];
   }

   /// do something to check for infinities...
//   if(all(c(a1,a2,b1,b2,c1,c2)!=Inf) & all(c(a1,a2,b1,b2,c1,c2)!=-Inf)){
      // calculate the intersection


// want to calculate...
//      intx<-det(matrix(c(b1,b2,c1,c2),2,2))/det(matrix(c(a1,a2,b1,b2),2,2))
//      inty<-det(matrix(c(c1,c2,a1,a2),2,2))/det(matrix(c(a1,a2,b1,b2),2,2))

      ret.x=(b1*c2-b2*c1)/(a1*b2-a2*b1);
      ret.y=(c1*a2-a1*c2)/(a1*b2-b1*a2);

//   }else{
//      ret<-list(x=Inf,y=Inf)
//   }

   return ret;

}














/* find if a point is on a line */
int online(double p1[],double thisline[][2]){
   // returns 1 if the point is on the line, 0 otherwise


   /* So here we just use an n by 2 matrix to represent
      the points, first col is x, second y, obv.*/
   double eps, *xsort, *ysort, leftside, rightside,xarr[2],yarr[2];
   
   /* Take this global at some point*/
   eps=1.0e-10;



   /* left hand side of equation */
   /* difference between y values */
   if(fabs(thisline[1][1]-thisline[0][1])<eps){
      /* first handle if it's a horizontal line */

      xarr[0]=thisline[0][0];
      xarr[1]=thisline[1][0];
      xsort=twosort(xarr);
      // need to make sure this kind of thing makes sense
       

      if((fabs(thisline[1][1]-p1[1])<eps) &&
         ((p1[0]<xsort[1])&&(p1[0]>xsort[0]))){
         return 1;
      }else{ 
         return 0;
      }

   }else{
      leftside=(p1[1]-thisline[0][1])/(thisline[1][1]-thisline[0][1]);
   }

   /* right hand side of equation */
   if(fabs(thisline[1][0]-thisline[0][0])<eps){
      /* first handle if it's a vertical line */


      yarr[0]=thisline[0][1];
      yarr[1]=thisline[1][1];
      ysort=twosort(yarr);

      if((fabs(thisline[1][0]-p1[0])<eps) &&
         ((p1[1]<ysort[1])&&(p1[1]>ysort[0]))){
         return 1;
      }else{
         return 0;
      }

   }else{
      rightside=(p1[0]-thisline[0][0])/(thisline[1][0]-thisline[0][0]);
   }

   /* If nothing went wrong then do the comparison*/
   if(fabs(leftside-rightside)<eps){
      return(1);
   }else{
      return(0);
   }

}





double *twosort(double twovec[2])
{
   // see if two vectors are small->large
 
   double tmp;  

   if(twovec[1]<twovec[0])
   {
      tmp=twovec[0];
      twovec[0]=twovec[1];
      twovec[1]=tmp;
   }

   return(twovec);
}













