/* First hash at re-writing the utils.R script in C */

#include <math.h>
#include <stdlib.h>
#include <stdio.h>

// typedefs
typedef struct
{
   double x,y;
} point;

// some prototypes
double *twosort(double[2]);
int online(double[2],double[2][2]);
int facing(double p1[2], double p2[2] , int nbnd, double bnd[nbnd][2]);
point intpoint(double[2], double[2],double[2][2]);
void do_intersect(double[2], double[2], int nbnd, double[nbnd][2],int bndint[nbnd-1]);
double minarr(int narr, double arr[narr]);
double maxarr(int narr, double arr[narr]);
int compare_doubles (const void *a, const void *b);
int crapfind(int narr, double[narr], double);
int iarrsum(int narr, int arr[narr]);
double hull_length(int nhull, double hull[nhull][2]);
void sp_do_intersect(double[2], double[2], int nbnd, double[nbnd][2],int[nbnd-1]);










// do two points and the boundary intersect?
void do_intersect(double p1[2], double p2[2], int nbnd, double bnd[nbnd][2],int bndint[nbnd-1])
{
   /*
    * p1,p2    points we wish to test
    * nbnd     length of boundary
    * bnd      boundary
    * bndint   boundary intersections
    */

   // uses: maxarr, minarr, intpoint

   // we do this by seeing if the bounding boxes intersect
   // from Mastering Algorithms with Perl, p 451

   double eps;
   int i;
   double pbbox[2][2], ebbox[2][2], thisedge[2][2];
   point ip;

   eps=1e-10;

   // bounding box around the points p1,p2
   //p.bbox<-list(x=c(max(p1$x,p2$x),min(p1$x,p2$x)),
   //             y=c(max(p1$y,p2$y),min(p1$y,p2$y)))
   double xarr[2] = {p1[0],p2[0]};
   double yarr[2] = {p1[1],p2[1]};
   pbbox[0][0]=maxarr(2,xarr);
   pbbox[0][1]=maxarr(2,yarr);
   pbbox[1][0]=minarr(2,xarr);
   pbbox[1][1]=minarr(2,yarr);

   // iterate over sides (ie vertex pairs)
   // NB the last vertex should be the first
   for (i=0;i<(nbnd-1);i++){

      // set true to begin with
      bndint[i]=1;

      // create the edge
      thisedge[0][0]=bnd[i][0];
      thisedge[1][0]=bnd[i+1][0];
      thisedge[0][1]=bnd[i][1];
      thisedge[1][1]=bnd[i+1][1];

      // bounding box for the edge
      //e.bbox<-list(x=c(max(bnd$x[c(i,i+1)]),min(bnd$x[c(i,i+1)])),
      //             y=c(max(bnd$y[c(i,i+1)]),min(bnd$y[c(i,i+1)])))
      xarr[0] = thisedge[0][0];
      xarr[1] = thisedge[1][0];
      yarr[0] = thisedge[0][1];
      yarr[1] = thisedge[1][1];
      ebbox[0][0]=maxarr(2,xarr);
      ebbox[0][1]=maxarr(2,yarr);
      ebbox[1][0]=minarr(2,xarr);
      ebbox[1][1]=minarr(2,yarr);

      // establish whether the bounding boxes intersect
      if(ebbox[0][0]+eps < pbbox[1][0]) bndint[i]=0;
      if(pbbox[0][0]+eps < ebbox[1][0]) bndint[i]=0;
      if(ebbox[0][1]+eps < pbbox[1][1]) bndint[i]=0;
      if(pbbox[0][1]+eps < ebbox[1][1]) bndint[i]=0;

      // if the bounding boxes do intersect, check that the
      // intersection of the two lines lies within the bounding
      // boxes.
      if(bndint[i]){
         // first find the intersection point
         ip=intpoint(p1,p2,thisedge);

// NEED TO DO SOME ERROR CHECKING HERE!
//         if(is.na(ip$x)|is.na(ip$y)){
//            bndint[i]<-FALSE
//         }else{

            // first need to handle the horizontal and vertical line cases
            // then handle whether the intersection point lies within the
            // the bounding box
            if(fabs(ebbox[0][0]-ebbox[1][0])>=eps){
               if(ip.x>=ebbox[0][0] | ip.x<=ebbox[1][0]) bndint[i]=0;
            }
            if(fabs(pbbox[0][0]-pbbox[1][0])>=eps){
               if(ip.x>=pbbox[0][0] | ip.x<=pbbox[1][0]) bndint[i]=0;
            }
            if(fabs(ebbox[0][1]-ebbox[1][1])>=eps){
               if(ip.y>=ebbox[0][1] | ip.y<=ebbox[1][1]) bndint[i]=0;
            }
            if(fabs(pbbox[0][1]-pbbox[1][1])>=eps){
               if(ip.y>=pbbox[0][1] | ip.y<=pbbox[1][1]) bndint[i]=0;
            }

      if(bndint[i]){
               if(fabs(ebbox[0][0]-ebbox[1][0])<=eps){
                  if(ip.y>=ebbox[0][1] | ip.y<=ebbox[1][1]) bndint[i]=0;
               }
               if(fabs(pbbox[0][0]-pbbox[1][0])<=eps){
                  if(ip.y>=pbbox[0][1] | ip.y<=pbbox[1][1]) bndint[i]=0;
               }
               if(fabs(ebbox[0][1]-ebbox[1][1])<=eps){
                  if(ip.x>=ebbox[0][0] | ip.x<=ebbox[1][0]) bndint[i]=0;
               }
               if(fabs(pbbox[0][1]-pbbox[1][1])<=eps){
                  if(ip.x>=pbbox[0][0] | ip.x<=pbbox[1][0]) bndint[i]=0;
               }
             }
         }
// error checking end brace!
//      }
   }
}


// special do_intersect, thinks that points that start/end at the same
// place don't intersect. Neither do exactly overlapping lines.
void sp_do_intersect(double p1[2], double p2[2], int nbnd, double bnd[nbnd][2],int bndint[nbnd-1])
{

   int i, tmpnbnd, tmpbndint[1];
   double tmpbnd[2][2];

   // iterate over sides (ie vertex pairs)
   // NB the last vertex should be the first
   for(i=0;i<(nbnd-1);i++){
      // set true to begin with
      bndint[i]=1;

      // case where the lines are exactly overlapping
      if((p1[0]==bnd[i][0] & p2[0]==bnd[i+1][0] &
          p1[1]==bnd[i][1] & p2[1]==bnd[i+1][1])|
         (p2[0]==bnd[i][0] & p1[0]==bnd[i+1][0] &
          p2[1]==bnd[i][1] & p1[1]==bnd[i+1][1])) bndint[i]=0;

      // start/end points the same
      if((p1[0]==bnd[i][0]   & p1[1]==bnd[i][1])|
         (p2[0]==bnd[i][0]   & p2[1]==bnd[i][1])|
         (p1[0]==bnd[i+1][0] & p1[1]==bnd[i+1][1]) |
         (p2[0]==bnd[i+1][0] & p2[1]==bnd[i+1][1])) bndint[i]=0;

      // call original routine if this doesn't work
      if(bndint[i]){
         tmpbnd[0][0]=bnd[i][0]; tmpbnd[0][1]=bnd[i][1];
         tmpbnd[1][0]=bnd[i+1][0]; tmpbnd[1][1]=bnd[i+1][1];
         tmpnbnd=2;

         tmpbndint[0]=bndint[i];

         do_intersect(p1, p2, tmpnbnd, tmpbnd, tmpbndint);
         bndint[i]=tmpbndint[0];
      }
   }
}





/* determine whether the line between two points is facing inside or outside */
int facing(double p1[2], double p2[2] , int nbnd, double bnd[nbnd][2])
{
   /*
   Args:
      p1, p2      the points
      nbnd        length of the boundary
      bnd         the boundary
      Return:
            logical, TRUE= facing inside
                                  FALSE= facing outside
   */

   int ret=0;
   int i,lbbindex;
   double thisedge[2][2];


   // do_intersect returns a string of T/F values
   // default set to true
   int *retint[nbnd-1];
   for(i=0;i<(nbnd-1);i++){
      *retint[i]=1;
   }

   do_intersect(p1,p2,nbnd,bnd,*retint);

   // length of the bounding box index
   lbbindex=iarrsum((nbnd-1),*retint);

// DEBUG
   printf("lbbindex=%d\n",lbbindex);


   double ip1[2],ip2[2];

   first_last_ips(nbnd, bnd, ip1,ip2,);


   // if lbbindex == 0 then skip to the return, see bottom
   if(lbbindex>1){
      // find intersections & sort by distance
      // bbindex=c(1:(length(bnd$x)-1))[doint]
      int bbindex[lbbindex];
      int j=0;

      for(i=0;i<(nbnd-1);i++){
         if(retint[i]){
            bbindex[j]=i;
            j++;
         }
      }

      // hold distances and intersection points temporarily
      double dists[lbbindex];
      double sortdists[lbbindex];
      double ips[lbbindex][2];
      point ip;

      for(i=0;i<lbbindex;i++){

         thisedge[0][0]=bnd[i][0];
         thisedge[1][0]=bnd[i+1][0];
         thisedge[0][1]=bnd[i][1];
         thisedge[1][1]=bnd[i+1][1];

         // calculate and save the intersection
         ip=intpoint(p1,p2,thisedge);
         ips[i][0]=ip.x;
         ips[i][1]=ip.y;

         // find the distance and save
         dists[i]=hypot((p1[0]-ip.x),(p1[1]-ip.y));
         // also copy for sorting
         sortdists[i]=dists[i];
      }

// SORT THIS OUT!!!!
      // remove duplicates (ie when dist is zero)
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
/////////////////////////////////////////////

      // prototype from stdlib.h
      // void qsort (void *array, size_t count, size_t size, comparison_fn_t compare)
      // The qsort function sorts the array array. 
      // The array contains count elements, each of which is of size size.
      // The compare function is used to perform the comparison on the array elements. 
      qsort(sortdists,lbbindex,sizeof(double),compare_doubles);

      // find first intersection between p1 and bnd
      // p1.int<-pe(ips,order(dists)[1])
      int firstel = crapfind(lbbindex,dists,sortdists[0]);
      double p1int[2]={ips[firstel][0],ips[firstel][1]};
      
      // find first intersection between p2 and bnd
      // p2.int<-pe(ips,order(dists,decreasing=TRUE)[1])
      int lastel = crapfind(lbbindex,dists,sortdists[(lbbindex-1)]);
      double p2int[2]={ips[lastel][0],ips[lastel][1]};

      // midpoint between p1 and first intersection 
      double p1mp[2]={(p1int[0]+p1[0])/2,(p1int[1]+p1[1])/2};
      // midpoint between p2 and first intersection 
      double p2mp[2]={(p2int[0]+p2[0])/2,(p2int[1]+p2[1])/2};

      // are the midpoints inside?
      // ret<-inSide(bnd,c(p1.mp$x,p2.mp$x),c(p1.mp$y,p2.mp$y))
      // call the in_out routine from soap. Need to make sure that things are
      // in the right format
      //void in_out(double *bx, double *by, double *break_code, double *x,double *y,int *in, int *nb, int *n)

      double bx[nbnd];
      double by[nbnd];
      for(i=0;i<(nbnd-1);i++){
         bx[i]=bnd[i][0]; 
         by[i]=bnd[i][1];
      }

      // find the midpoints between p1, p2 their first intersections
      // store in x and y blocks
      double xmp[2]={(p1int[0]+p1[0])/2,(p2int[0]+p2[0])/2};
      double ymp[2]={(p1int[1]+p2[1])/2,(p2int[1]+p2[1])/2};

      // to handle holes, multiple boundaries
      // don't handle this at the moment
      double break_code=1.0e6;

 // DEBUG
 printf("xmp=(%f,%f)\n",xmp[0],xmp[1]);
 printf("ymp=(%f,%f)\n",ymp[0],ymp[1]);

      int in[2]={0,0};
      in_out(bx, by, break_code, xmp, ymp, in, nbnd, 2);

      // if they are both inside, return true
      if(in[0] && in[1]) ret=1;

   } // end of lbbindex if()

   return(ret);

}



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
int online(double p1[],double thisline[][2])
{
   // uses: twosort
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


// calculate the length of the hull
double hull_length(int nhull, double hull[nhull][2])
{
   // args:
   //  nhull      # elements in hull
   //  hull       hull (list(x=,y=))
   // return      its length

   int i;
   double hullen=0;

   for (i=0; i<(nhull-1);i++){
      hullen=hullen+hypot((hull[i+1][0]-hull[i][0]),(hull[i+1][1]-hull[i][1]));
   }
   return(hullen);
}





/*
 * Real utility stuff below here!
 *
 *
 */

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


// find the maximum in an array
double maxarr(int narr, double arr[])
{
   int i;
   double maxval=arr[0];

   for(i=1;i<narr;i++){
      if(arr[i]>maxval){
         maxval=arr[i];
      }
   }
   return maxval;
}

// find the minimum in an array
double minarr(int narr, double arr[])
{
   int i;
   double minval=arr[0];

   for(i=1;i<narr;i++){
      if(arr[i]<minval){
         minval=arr[i];
      }
   }
   return minval;
}


// sum an array of integers 
int iarrsum(int narr, int arr[])
{
   int i;
   int val=0;

   for(i=0;i<narr;i++){
      val=val+arr[i];
   }
   return val;
}


/* for use with qsort from stdlib.h
 * see:
 * http://www.gnu.org/software/libc/manual/html_mono/libc.html#Search_002fSort-Example
 */
int compare_doubles (const void *a, const void *b)
{
   const double *da = (const double *) a;
   const double *db = (const double *) b;
   
   return (*da > *db) - (*da < *db);
}

// my very own, very poor find
// returns the first element of the array to match the value
int crapfind(int narr, double arr[narr], double val){
   int i;
   int index;

   for(i=0;i<narr;i++){
      if(arr[i]==val){
         index=i;
         break;
      }
   }
   return index;
}





