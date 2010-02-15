/* First hash at re-writing the utils.R script in C */
// Copyright 2009 David Lawrence Miller

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include "utils.h"

// do two points and the boundary intersect?
void do_intersect(double p1[2], double p2[2], int nbnd, double **bnd,int *bndint)
{
   /*
    * p1,p2    points we wish to test
    * nbnd     length of boundary
    * bnd      boundary
    * bndint   boundary intersections (length nbnd-1)
    */

   // uses: maxarr, minarr, intpoint

   // we do this by seeing if the bounding boxes intersect
   // from Mastering Algorithms with Perl, p 451

   double eps=1e-16;
   int i;
   double pbbox[2][2], ebbox[2][2], thisedge[2][2], ip[2], xarr[2], yarr[2];

   // bounding box around the points p1,p2
   //p.bbox<-list(x=c(max(p1$x,p2$x),min(p1$x,p2$x)),
   //             y=c(max(p1$y,p2$y),min(p1$y,p2$y)))
   xarr[0] = p1[0]; xarr[1] = p2[0];
   yarr[0] = p1[1]; yarr[1] = p2[1];
   pbbox[0][0]=maxarr(2,xarr);
   pbbox[0][1]=maxarr(2,yarr);
   pbbox[1][0]=minarr(2,xarr);
   pbbox[1][1]=minarr(2,yarr);

   // iterate over sides (ie vertex pairs)
   // NB the last vertex should be the first
   for(i=0;i<(nbnd-1);i++){
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
      // if max edge x less than min point x
      if(ebbox[0][0]+eps <= pbbox[1][0]) bndint[i]=0;
      // if max point x less than min edge x
      if(pbbox[0][0]+eps <= ebbox[1][0]) bndint[i]=0;
      // if max edge y less than min point y
      if(ebbox[0][1]+eps <= pbbox[1][1]) bndint[i]=0;
      // if max point y less than min edge y
      if(pbbox[0][1]+eps <= ebbox[1][1]) bndint[i]=0;

      // if the bounding boxes do intersect, check that the
      // intersection of the two lines lies within the bounding
      // boxes.
      if(bndint[i]){

         // first find the intersection point
         intpoint(p1,p2,thisedge,ip);

         // check the intersection point is not just one of p1 or p2
         if(( (fabs(ip[0]-p1[0]) <=eps) && (fabs(ip[1]-p1[1]) <=eps) ) |
            ( (fabs(ip[0]-p2[0]) <=eps) && (fabs(ip[1]-p2[1]) <=eps) )){
            bndint[i]=0;
         }

         // or that it's one of the edge end points
         if(( (fabs(ip[0]-thisedge[0][0]) <=eps) && (fabs(ip[1]-thisedge[0][1]) <=eps)) |
            ((fabs(ip[0]-thisedge[1][0]) <=eps) && (fabs(ip[1]-thisedge[1][1]) <=eps))){
            bndint[i]=0;
         }

         // first need to handle the horizontal and vertical line cases
         if(fabs(ebbox[0][0]-ebbox[1][0])>=eps){
            if((ip[0]>=ebbox[0][0]) | (ip[0]<=ebbox[1][0])) bndint[i]=0;
         }
         if(fabs(pbbox[0][0]-pbbox[1][0])>=eps){
            if((ip[0]>=pbbox[0][0]) | (ip[0]<=pbbox[1][0])) bndint[i]=0;
         }
         if(fabs(ebbox[0][1]-ebbox[1][1])>=eps){
            if((ip[1]>=ebbox[0][1]) | (ip[1]<=ebbox[1][1])) bndint[i]=0;
         }
         if(fabs(pbbox[0][1]-pbbox[1][1])>=eps){
            if((ip[1]>=pbbox[0][1]) | (ip[1]<=pbbox[1][1])) bndint[i]=0;
         } // end of horiz/vert check
   
         // then handle whether the intersection point lies within the
         // the bounding box
         if(bndint[i]){
            if(fabs(ebbox[0][0]-ebbox[1][0])<=eps){
               if((ip[1]>=ebbox[0][1]) | (ip[1]<=ebbox[1][1])) bndint[i]=0;
            }
            if(fabs(pbbox[0][0]-pbbox[1][0])<=eps){
               if((ip[1]>=pbbox[0][1]) | (ip[1]<=pbbox[1][1])) bndint[i]=0;
            }
            if(fabs(ebbox[0][1]-ebbox[1][1])<=eps){
               if((ip[0]>=ebbox[0][0]) | (ip[0]<=ebbox[1][0])) bndint[i]=0;
            }
            if(fabs(pbbox[0][1]-pbbox[1][1])<=eps){
               if((ip[0]>=pbbox[0][0]) | (ip[0]<=pbbox[1][0])) bndint[i]=0;
            }
         } // end of bounding box ip check
      }
   }// end iterate over boundary
}


// special do_intersect, thinks that points that start/end at the same
// place don't intersect. Neither do exactly overlapping lines.
void sp_do_intersect(double p1[2], double p2[2], int nbnd, double **bnd,int *bndint)
{
   int i,j, tmpnbnd, tmpbndint[1];
   double **tmpbnd;
   double eps=1e-16;

   tmpnbnd=2;

   tmpbnd=(double**)malloc(sizeof(double*)*tmpnbnd);
   tmpbnd[0]=(double*)malloc(sizeof(double)*tmpnbnd*2);

   for(i=0; i<tmpnbnd; i++){
      tmpbnd[i]=tmpbnd[0]+i*2;
   }

   // iterate over sides (ie vertex pairs)
   // NB the last vertex should be the first
   for(j=0;j<(nbnd-1);j++){
      // set true to begin with
      bndint[j]=1;

      // case where the lines are exactly overlapping
      if(( (fabs(p1[0]-bnd[j][0])   <= eps) & 
           (fabs(p2[0]-bnd[j+1][0]) <= eps) &
           (fabs(p1[1]-bnd[j][1])   <= eps) & 
           (fabs(p2[1]-bnd[j+1][1]) <= eps) ) |
         ( (fabs(p2[0]-bnd[j][0])   <= eps) & 
           (fabs(p1[0]-bnd[j+1][0]) <= eps) &
           (fabs(p2[1]-bnd[j][1])   <= eps) &
           (fabs(p1[1]-bnd[j+1][1]) <= eps) )) bndint[j]=0;
     
      // start/end points the same
      if(( (fabs(p1[0]-bnd[j][0])  <= eps) && (fabs(p1[1]-bnd[j][1])  <= eps) )|
         ( (fabs(p2[0]-bnd[j][0])  <= eps) && (fabs(p2[1]-bnd[j][1])  <= eps) )|
         ( (fabs(p1[0]-bnd[j+1][0])<= eps) && (fabs(p1[1]-bnd[j+1][1])<= eps) )|
         ( (fabs(p2[0]-bnd[j+1][0])<= eps) && (fabs(p2[1]-bnd[j+1][1])<= eps) ) )
            bndint[j]=0;

      // call original routine if this doesn't work
      if(bndint[j]){
         tmpbnd[0][0]=bnd[j][0]; tmpbnd[0][1]=bnd[j][1];
         tmpbnd[1][0]=bnd[j+1][0]; tmpbnd[1][1]=bnd[j+1][1];

         tmpbndint[0]=bndint[j];

         do_intersect(p1, p2, tmpnbnd, tmpbnd, tmpbndint);

         bndint[j]=tmpbndint[0];
      }
   } // end for loop
   free(tmpbnd[0]);
   free(tmpbnd);
}


/* determine whether the line between two points is facing inside or outside */
int facing(double p1[2], double p2[2] , int nbnd, double **bnd){
   /*
   Args:
      p1, p2      the points
      nbnd        length of the boundary
      bnd         the boundary
      Return:
            1 if facing inside, 0 otherwise
   */
   int ret=0;
   int in[2]={0,0};
   int i, err, intind[2], tmpinout;
   double ip1[2],ip2[2], xmp[2], ymp[2];
   double *bx, *by, xmin, ymin, mina[2], break_code;

   err=first_ips(p1, p2, nbnd, bnd, ip1, ip2, intind);

   // if there are no errors, go ahead
   if(err==0){
      // are the midpoints inside?
      // ret<-inSide(bnd,c(p1.mp$x,p2.mp$x),c(p1.mp$y,p2.mp$y))
      // call the in_out routine from soap. Need to make sure that things are
      // in the right format
      //void in_out(double *bx, double *by, double *break_code, double *x,double *y,int *in, int *nb, int *n)

      bx=(double*)malloc(sizeof(double)*nbnd);
      by=(double*)malloc(sizeof(double)*nbnd);

      for(i=0; i<nbnd; i++){
         bx[i]=bx[0]+i;
         by[i]=by[0]+i;

         bx[i]=bnd[i][0]; 
         by[i]=bnd[i][1];
      }

      // DEBUG
//      printf("#----------\n");
//      printf("ip1<-list(x=%f,y=%f)\n",ip1[0],ip1[1]);
//      printf("ip2<-list(x=%f,y=%f)\n",ip2[0],ip2[1]);
//      printf("p1<-list(x=%f,y=%f)\n",p1[0],p1[1]);
//      printf("p2<-list(x=%f,y=%f)\n",p2[0],p2[1]);
//      printf("plot(bnd,type=\"l\")\n");
//      printf("points(p1,col=\"red\",pch=19)\n");
//      printf("points(p2,col=\"red\",pch=19)\n");
//      printf("points(ip1,col=\"blue\",pch=19)\n");
//      printf("points(ip2,col=\"blue\",pch=19)\n");
//      printf("scan();\n");

//      if(onbnd(p1,nbnd,bnd)>0){
//         ip1[0]=p1[0];
//         ip1[1]=p1[1];
//      }
//
//      if(onbnd(p2,nbnd,bnd)>0){
//         ip2[0]=p2[0];
//         ip2[1]=p2[1];
//      }


      // find the midpoints between p1, p2 their first intersections
      // store in x and y blocks
      xmp[0]=(ip1[0]+p1[0])/2;
      xmp[1]=(ip2[0]+p2[0])/2;
      ymp[0]=(ip1[1]+p1[1])/2;
      ymp[1]=(ip2[1]+p2[1])/2;

      // to handle holes, multiple boundaries
      // ignore this at the moment
      xmin=minarr(nbnd,bx);
      ymin=minarr(nbnd,by);
      mina[0] = xmin; mina[1]=ymin;
      break_code=minarr(2,mina)-1;

      tmpinout=2;

      in_out(bx, by, &break_code, xmp, ymp, in, &nbnd, &tmpinout);

      // if they are both inside, return true (ie they face inside)
      if(in[0] && in[1]){
         ret=1;
      }

      // free some memory
      free(bx);free(by);
   }

   // if err returned >0 then return 0
   return(ret);
}

// find the intersection point between two points and a line
void intpoint(double p1[2], double p2[2],double edge[2][2], double ip[2])
{
   /*args:
      p1, p2      the two points making up the end points of the line
      line         boundary of the shape to test intersection with
     return:
        intersection point
   */

   double eps,a1,b1,c1,a2,b2,c2;

   eps=1.0e-16;

   // calculation of intersection is straight from 
   // Handbook of Mathematics Bronstein et al. pp. 195,196

   /// calculate intersection point
   //   first calculate the coefficients for the lines
   //   the line between the points
   a1=-1/(p2[0]-p1[0]);
   b1=1/(p2[1]-p1[1]);
   c1=p1[0]/(p2[0]-p1[0]) - p1[1]/(p2[1]-p1[1]);

   // the edge
   a2=-1/(edge[1][0]-edge[0][0]);
   b2=1/(edge[1][1]-edge[0][1]);
   c2= edge[0][0]/(edge[1][0]-edge[0][0]) - edge[0][1]/(edge[1][1]-edge[0][1]);

   // handle the horizontal/vertical line case...

   // point line horizontal
   if( (fabs((p2[1]-p1[1])/(p2[0]-p1[0]))<eps) | isnan((p2[1]-p1[1])/(p2[0]-p1[0]))){
      a1=0;
      b1=1;
      c1=-p1[1];
   // point line vertical
   }

   if( (fabs((p2[0]-p1[0])/(p2[1]-p1[1]))<eps) | isnan((p2[0]-p1[0])/(p2[1]-p1[1]))){
      a1=1;
      b1=0;
      c1=-p1[0];
   }

   // edge horizontal
   if( (fabs((edge[1][1]-edge[0][1])/(edge[1][0]-edge[0][0]))<eps) | 
            isnan((edge[1][1]-edge[0][1])/(edge[1][0]-edge[0][0]))){
      a2=0;
      b2=1;
      c2=-edge[0][1];
   // edge vertical
   }

   if( (fabs((edge[1][0]-edge[0][0])/(edge[1][1]-edge[0][1]))<eps) | 
            isnan((edge[1][0]-edge[0][0])/(edge[1][1]-edge[0][1]))){
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
      ip[0]=(b1*c2-(b2*c1))/(a1*b2-(a2*b1));
      ip[1]=(c1*a2-(a1*c2))/(a1*b2-(b1*a2));

//   }else{
//      ret<-list(x=Inf,y=Inf)
//   }
}


 
/* find if a point is on a line */
int online(double p1[],double thisline[][2]){
   // uses: twosort
   // returns 1 if the point is on the line, 0 otherwise
 
   /* So here we just use an n by 2 matrix to represent
the points, first col is x, second y, obv.*/
   double eps, leftside, rightside,xarr[2],yarr[2];
   
   /* Take this global at some point*/
   eps=1.0e-16;
 
   /* left hand side of equation */
   /* difference between y values */
   if(fabs(thisline[1][1]-thisline[0][1])<eps){
      /* first handle if it's a horizontal line */
 
      xarr[0]=thisline[0][0];
      xarr[1]=thisline[1][0];
      twosort(xarr);
      // need to make sure this kind of thing makes sense
 
      if((fabs(thisline[1][1]-p1[1])<eps) &&
         ((p1[0]<xarr[1])&&(p1[0]>xarr[0]))){
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
      twosort(yarr);
 
      if((fabs(thisline[1][0]-p1[0])<eps) &&
         ((p1[1]<yarr[1])&&(p1[1]>yarr[0]))){
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


//int online(double p1[],double thisline[][2]){
// 
//   double m,c, eps=1e-16;
//   double xarr[2], yarr[2];
// 
//   xarr[0]=thisline[0][0];
//   xarr[1]=thisline[1][0];
//   yarr[0]=thisline[0][1];
//   yarr[1]=thisline[1][1];
// 
//   twosort(xarr);
//   twosort(yarr); // make xarr, yarr small->large
// 
//   // check p1 is inside the bounding box
//   if((p1[0]>=xarr[1]) && (p1[0]<=xarr[0]) &&
//      (p1[1]>=yarr[1]) && (p1[1]<=yarr[0])){
//      return 0;
//   }
// 
//   // calculate gradient of the line
//   m = (thisline[1][1]-thisline[0][1])/(thisline[1][0]-thisline[0][0]);
// 
//   // calculate intercept
//   c = thisline[1][1]-m*thisline[1][0];
// 
//   // does is p1 a solution?
//   if(fabs(p1[1]-(m*p1[0]+c))<=eps){
//      return 1;
//   }else{
//      return 0;
//   }
//}



// is a point on the boundary? Just calls online repeatedly...
int onbnd(double point[], int nbnd, double** bnd){
//int online(double p1[],double thisline[][2])

   int i,ind=0;
   double thisline[2][2];


   // iterate over the whole boundary
   for(i=0;i<(nbnd-1);i++){
      thisline[0][0]=bnd[i][0];
      thisline[0][1]=bnd[i][1];
      thisline[1][0]=bnd[i+1][0];
      thisline[1][1]=bnd[i+1][1];

      ind=ind+online(point,thisline);

      // DEBUG
//      if(online(point,thisline)!=0){
//         printf("on[%d]=%d\n",(i-1),online(point,thisline));
//
//         printf("plot(bnd,type=\"l\",asp=1)\n");
//         printf("lines(pe(bnd,c(%d,%d)),lwd=5,col=\"red\")\n",i,i+1);
//         printf("p1<-list(x=%f,y=%f)\n",point[0],point[1]);
//         printf("points(p1,col=\"red\",pch=19)\n");
//      }
   }
   return(ind);
}

// calculate the length of the hull by iterating over
// the list object
double hull_length(node** hull) {

   double hullen=0;
   node* current = *hull;

   if(current ==NULL){
      return(1e16);
   }

   while (current->next != NULL) {
      hullen=hullen+hypot((current->next->data[0] - current->data[0]),
                          (current->next->data[1] - current->data[1]));
      current = current->next;
   }
   return(hullen);
}

///////////////////////////////////
// find the first intersection points of p1 and p2
// with bnd
int first_ips(double p1[2], double p2[2], int nbnd, double **bnd, 
               double ip1[2], double ip2[2],int intind[2])
{   
   /* Args:
   *   p1, p2        the points
   *   nbnd          length of boundary
   *   bnd           the boundary
   *   ip1, ip2      intersection points (returned)
   *   intbnd        boundary intersection indices (index of bnd where
   *                 the intersections occur.)
   *
   * Return:
   *                 error code, 0=okay
   */

   int i, lbbindex, firstel, lastel, *retint, *bbindex;
   double thisedge[2][2], **ips, *dists, *sortdists;
   double ip[2];
   int j=0;

   // error code 0= okay
   int err=0;

   // setup retint
   retint=(int*)malloc(sizeof(int)*(nbnd-1));
   for(i=0; i<(nbnd-1); i++){
      retint[i]=retint[0]+i;
   }

   // do the bounding box check first, for speed
   // do_intersect returns a string of T/F values
  
   // find intersections 
   // this is what is used in the R code.
   do_intersect(p1,p2,nbnd,bnd,retint);
   
   // length of the bounding box index
   lbbindex=iarrsum((nbnd-1),retint);

   // setup bbindex, dists, sortdists
   bbindex=(int*)malloc(sizeof(int)*lbbindex);
   dists=(double*)malloc(sizeof(double)*lbbindex);
   sortdists=(double*)malloc(sizeof(double)*lbbindex);
   for(i=0; i<lbbindex; i++){
      bbindex[i]=bbindex[0]+i;
      dists[i]=dists[0]+i;
      sortdists[i]=sortdists[0]+i;
   }

   // if lbbindex < 1 increment err
   if(lbbindex>1){
      // find intersections & sort by distance
      // bbindex=c(1:(length(bnd$x)-1))[doint]

      // populate bbindex   
      for(i=0;i<(nbnd-1);i++){
         if(retint[i]){
            bbindex[j]=i;
            j++;
         }
      }

      // hold distances and intersection points temporarily
      ips=(double**)malloc(sizeof(double*)*lbbindex);
      ips[0]=(double*)malloc(sizeof(double)*lbbindex*2);
      for(i=0; i<lbbindex; i++){
         ips[i]=ips[0]+i*2;
      }

      for(i=0;i<lbbindex;i++){
         // get current index
         j=bbindex[i];

         thisedge[0][0]=bnd[j][0];
         thisedge[1][0]=bnd[j+1][0];
         thisedge[0][1]=bnd[j][1];
         thisedge[1][1]=bnd[j+1][1];

         // calculate and save the intersection
         intpoint(p1,p2,thisedge,ip);
         ips[i][0]=ip[0];
         ips[i][1]=ip[1];

         // find the distance and save
         dists[i]=hypot(p1[0]-ip[0],p1[1]-ip[1]);


//printf("p1[0]=%f p1[1]=%f ip[0]=%f ip[1]=%f\n",p1[0], p1[1],ip[0],ip[1]);
//printf("p2[0]=%f p2[1]=%f\n",p2[0], p2[1]);
//printf("hyp=%f\n",hypot(p1[0]-ip[0],p1[1]-ip[1]));

         // also copy for sorting
         sortdists[i]=dists[i];
      }

      // prototype from stdlib.h
      // void qsort (void *array, size_t count, size_t size, comparison_fn_t compare)
      // The qsort function sorts the array array. 
      // The array contains count elements, each of which is of size size.
      // The compare function is used to perform the comparison on the array elements. 
      qsort(sortdists,lbbindex,sizeof(double),compare_doubles);

      // find first intersection between p1 and bnd
      // p1.int<-pe(ips,order(dists)[1])
      firstel = crapfind(lbbindex,dists,sortdists[0]);

      ip1[0]=ips[firstel][0];
      ip1[1]=ips[firstel][1];
      
      // find first intersection between p2 and bnd
      // p2.int<-pe(ips,order(dists,decreasing=TRUE)[1])
      lastel = crapfind(lbbindex,dists,sortdists[(lbbindex-1)]);
      ip2[0]=ips[lastel][0];
      ip2[1]=ips[lastel][1];

      // calculate intind
      intind[0]=bbindex[firstel];
      intind[1]=bbindex[lastel];

      free(ips[0]);
      free(ips);

   }else{
      // let the Robot warn us...
      //printf("DANGER, WILL ROBINSON! lbbindex=%d (< 2)\n",lbbindex);
      err++;
   } // end of lbbindex<2 check

   free(bbindex);
   free(dists);
   free(sortdists);
   free(retint);

   return(err);
}


/*
 * Linked list code here
 * mostly modified from http://cslibrary.stanford.edu/
 */

/*
 *   Takes a list and a data value.
 *   Creates a new link with the given data and pushes
 *   it onto the front of the list.
 *   The list is not passed in by its head pointer.
 *   Instead the list is passed in as a "reference" pointer
 *   to the head pointer -- this allows us
 *   to modify the caller's memory.
 */

// free a linked list's memory
void FreeList(node** headRef) { 
   node* current = *headRef;// deref headRef to get the real head 
   node* next=NULL; 

   while (current != NULL) { 
      next = current->next; // note the next pointer 
      free(current); // delete the node 
      current = next; // advance to the next node 
   } 

   *headRef = NULL; // Again, deref headRef to affect the real head back 
                    // in the caller. 
} 

void Push(node** headRef, double data[2]) {
   node* newNode = malloc(sizeof(node));
   newNode->data[0] = data[0];
   newNode->data[1] = data[1];

   // next element of the new node will be the head
   newNode->next = *headRef;  // The '*' to dereferences back 
								 		// to the real head

	if(newNode->next !=NULL){
	   newNode->next->prev = newNode;
	}
  
   // previous element of new node will be NULL
   newNode->prev = NULL;
   *headRef = newNode;
}

// Normal AppendNode 
//void AppendNode(node** headRef, double data[2]) {
//   node* current = *headRef;
//   node* newNode = malloc(sizeof(node));
//
//   newNode->data[0] = data[0];
//   newNode->data[1] = data[1];
//   newNode->next = NULL;
//
//   // special case for length 0
//   if (current == NULL) {
//      newNode->prev = NULL;
//      *headRef = newNode;
//   }else{
//      // Locate the last node
//      while (current->next != NULL) {
//         current = current->next;
//      }
//      // insert the new node
//      newNode->prev = current;
//      current->next = newNode;
//   }
//}

// AppendNode with Push()
void AppendNode(node** headRef, double data[2]) { 
   node* current = *headRef; 
   // special case for the empty list 
   if (current == NULL) { 
      Push(headRef, data); 
   }else{ 
      // Locate the last node 
      while (current->next != NULL) { 
         current = current->next; 
      } 
      // Build the node after the last node 
      Push(&(current->next), data); 
      current->next->prev=current;
   } 
}

// Variant of CopyList() that uses Push()
void CopyList(node* head, node** newList)
{
   node* current = head;      // used to iterate over the original list

   // Re-write of this code with AppendNode.
   while (current != NULL) {
      AppendNode(newList, current->data);
      current = current->next;
   }
}

/*
 *   Given a linked list head pointer, compute
 *     and return the number of nodes in the list.
 */
int Length(node* head) {
    node* current = head;
    int count = 0;
    while (current != NULL) {
       count++;
       current = current->next;
    }
    return count;
}

/*
*  Debug printer for paths
*/
void PrintPath(node* mypath) {
   node* current=mypath;

   printf("plot(bnd,type=\"l\",asp=1)\n");
   printf("path<-list(x=c(),y=c())\n");
   
   while(current!=NULL){
      printf("path$x<-c(path$x,%f)\n",current->data[0]);
      printf("path$y<-c(path$y,%f)\n",current->data[1]);
      current=current->next;
   }
   printf("lines(path,lwd=2,col=\"red\")\n");
   printf("scan()\n");
   printf("#******* END  ********** \n");
   
}

// Delete the first and last entries of a list
void DelTopBot(node* head)
{
   node* current = NULL;      // used to iterate over the original list

   // miss out the first node
   *head=*(head->next);
   current=head;
   current->prev=NULL;
   
   current=head;

   // skip to the end 
   while (current->next->next != NULL) {
      current = current->next;
   }
   
//   free(current->next);
   current->next=NULL;

}

// reverse the elements of a linked list
//void ReverseList(node** head){
//   node* tmp=NULL;   
//   node* current=*head;
//
//   while(current!=NULL){
//      tmp=current->next;
//      current->next=current->prev;
//      current->prev=tmp;
//      if(current->prev == NULL){
//         head=&current;
//      }
//      current=current->prev;
//   }
//}

void ReverseList(node** head){
   node* current=*head;

   node* revlist = NULL;

   while(current!=NULL){
      Push(&revlist,current->data);
      current=current->next;
   }

   FreeList(head);

   *head=revlist;

}

////////////////////////////

/*
 * Real utility stuff below here!
 */

void twosort(double *twovec)
{
   // see if two vectors are small->large
   double tmp;  

   if(twovec[1]<twovec[0])
   {
      tmp=twovec[0];
      twovec[0]=twovec[1];
      twovec[1]=tmp;
   }
}

// integer version of the above
void itwosort(int *twovec)
{
   // see if two vectors are small->large
   int tmp;  

   if(twovec[1]<twovec[0])
   {
      tmp=twovec[0];
      twovec[0]=twovec[1];
      twovec[1]=tmp;
   }
}

// find the maximum in an array
double maxarr(int narr, double *arr)
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
double minarr(int narr, double *arr)
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
int iarrsum(int narr, int *arr)
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
// returns -1 if not found
int crapfind(int narr, double *arr, double val){
   int i, index=-1;

   for(i=0;i<narr;i++){
      if(arr[i]==val){
         index=i;
         break;
      }
   }
   return index;
}

