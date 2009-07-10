/* First hash at re-writing the utils.R script in C */

#include <math.h>








/* determine whether the line between two points is facing inside or outside */
int facing(double p1[2], double p2[2] , int nbnd bnd[nbnd][2]){
   /*
   Args:
      p1, p2      the points
      bnd         the boundary
      Return:
                  logical 2-vector, TRUE= facing inside
                                  FALSE= facing outside
   */

   int ret[2], bbindex;


   /*
   there might be no intersections, so if there aren't
   then just return FALSE and let the delete step take 
   care of it 
   */
   ret[2]={0,0}; 

/// DO SOMETHING WITH THIS
   doint=do_intersect(p1,p2,bnd);



/// check that this makes sense
   if(sum(doint)>1){

      // find intersections & sort by distance
      bbindex=c(1:(length(bnd$x)-1))[doint]
      // hold distances and intersection points temporarily
      dists<-c(); ips<-list(x=c(),y=c())

      for(j in bbindex){
         # calculate and save the intersection
         ip<-intersection_point(p1,p2,pe(bnd,c(j,j+1)))
         ips$x<-c(ips$x,ip$x)
         ips$y<-c(ips$y,ip$y)

         # find the distance and save
         dists<-c(dists,sqrt((p1$x-ip$x)^2+(p1$y-ip$y)^2))
      }

      # remove duplicates (ie when dist is zero)
      if(length(ips$x)>3){
         p1.ind<-which((ips$x==p1$x)&(ips$y==p1$y))
         p2.ind<-which((ips$x==p2$x)&(ips$y==p2$y))
         nonzero<-c(p1.ind,p2.ind)
         if(length(nonzero)!=0 & length(nonzero)!=length(dists)){
            dists<-dists[-nonzero]
            bbindex<-bbindex[-nonzero]
            ips$x<-ips$x[-nonzero];ips$y<-ips$y[-nonzero]
         }
      }


      # find first intersection between p1 and bnd
      p1.int<-pe(ips,order(dists)[1])
      # find first intersection between p2 and bnd
      p2.int<-pe(ips,order(dists,decreasing=TRUE)[1])

      # midpoint between p1 and first intersection 
      p1.mp<-list(x=(p1.int$x+p1$x)/2,y=(p1.int$y+p1$y)/2)
      # midpoint between p2 and first intersection 
      p2.mp<-list(x=(p2.int$x+p2$x)/2,y=(p2.int$y+p2$y)/2)

      # are the midpoints inside?
      ret<-inSide(bnd,c(p1.mp$x,p2.mp$x),c(p1.mp$y,p2.mp$y))
   }
   return(ret)
}

















/* find if a point is on a line */
int online(double p1[1][2],double thisline[2][2]){

   /* So here we just use an n by 2 matrix to represent
      the points, first col is x, second y, obv.*/
   double eps, xsort[2], leftside, rightside;
   
   /* Take this global at some point*/
   eps=1e-10;



   /* left hand side of equation */
   /* differnece between y values */
   if(fabs(thisline[2][2]-thisline[1][2])<eps){
      /* first handle if it's a horizontal line */

/* check sort function*/

      xsort<-sort(this.line$x) /// need to make sure this kind of thing makes sense
      if((fabs(thisline[2][2]-p1[2])<eps) &
         ((p1[1]<xsort[2])&(p1[1]>xsort[1]))){
         return(1);
      }else{ 
         return(0);
      }

   }else{
      leftside=(p1[2]-thisline[1][2])/(thisline[2][2]-thisline[1][2]);
   }

   /* right hand side of equation */
   if(fabs(thisline[2][1]-thisline[1][1])<eps){
      /* first handle if it's a vertical line */


      ysort<-sort(this.line$y); /// Same here!


      if((fabs(thisline[2][1]-p1[1])<eps) &
         ((p1$y<ysort[2])&(p1$y>ysort[1]))){
         return(1);
      }else{
         return(0);
      }

   }else{
      rightside=(p1[1]-thisline[1][1])/(thisline[2][1]-thisline[1][1]);
   }

   /* If nothing went wrong then do the comparison*/
   if(fabs(leftside-rightside)<eps){
      return(1);
   }else{
      return(0);
   }

}



