// Various utility functions for finding the shortest path. 
// Copyright 2009-2010 David Lawrence Miller
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include "utils.h"
#include "wood.h"

extern double eps;

int do_int(double p1[2], double p2[2], double p3[2], double p4[2], double ip[2]){
   double s,t, s1_x, s1_y, s2_x, s2_y, denom;

   s1_x = p2[0] - p1[0];
   s1_y = p2[1] - p1[1];
   s2_x = p4[0] - p3[0];
   s2_y = p4[1] - p3[1];

   denom= (-s2_x*s1_y + s1_x*s2_y);

   if(fabs(denom)<eps){
      return 0;
   }

   s = (-s1_y * (p1[0] - p3[0]) + s1_x * (p1[1] - p3[1])) /denom;
   t = ( s2_x * (p1[1] - p3[1]) - s2_y * (p1[0] - p3[0])) /denom;

   if (s >= 0 && s <= 1 && t >= 0 && t <= 1){
      // Collision detected
      ip[0] = p1[0] + (t * s1_x);
      ip[1] = p1[1] + (t * s1_y);
      return 1;
   }
   return 0; // No collision
}

void do_intersect(double p1[], double p2[], int nbnd, double **bnd,int *bndint){
   int i;
   double p3[2],p4[2],ip[2];

   // iterate over sides (ie vertex pairs)
   // NB the last vertex should be the first
   for(i=0;i<(nbnd-1);i++){
      bndint[i]=0;

      p3[0]=bnd[i][0];
      p4[0]=bnd[i+1][0];
      p3[1]=bnd[i][1];
      p4[1]=bnd[i+1][1];

      // are the lines identical?
      if(( (fabs(p1[0]-p3[0]) < eps) & 
           (fabs(p2[0]-p4[0]) < eps) &
           (fabs(p1[1]-p3[1]) < eps) & 
           (fabs(p2[1]-p4[1]) < eps) ) |
         ( (fabs(p2[0]-p3[0]) < eps) & 
           (fabs(p1[0]-p4[0]) < eps) &
           (fabs(p2[1]-p3[1]) < eps) &
           (fabs(p1[1]-p4[1]) < eps) )) bndint[i]=1;

      if(bndint[i]==0){
         bndint[i]=do_int(p1,p2,p3,p4,ip);
      }
   } // end bnd loop
}

void sp_do_intersect(double p1[], double p2[], int nbnd, double **bnd,int *bndint)
{
   /*
    * p1,p2    points we wish to test
    * nbnd     length of boundary
    * bnd      boundary
    * bndint   boundary intersections (length nbnd-1)
    */
   int i;
   double p3[2], p4[2];
   double ip[2];

   // iterate over sides (ie vertex pairs)
   // NB the last vertex should be the first
   for(i=0;i<(nbnd-1);i++){
      // set true to begin with
      bndint[i]=1;

      p3[0]=bnd[i][0];
      p4[0]=bnd[i+1][0];
      p3[1]=bnd[i][1];
      p4[1]=bnd[i+1][1];

      // are they just the same line?
      if(( (fabs(p1[0]-p3[0]) < eps) & 
           (fabs(p2[0]-p4[0]) < eps) &
           (fabs(p1[1]-p3[1]) < eps) & 
           (fabs(p2[1]-p4[1]) < eps) ) |
         ( (fabs(p2[0]-p3[0]) < eps) & 
           (fabs(p1[0]-p4[0]) < eps) &
           (fabs(p2[1]-p3[1]) < eps) &
           (fabs(p1[1]-p4[1]) < eps) )) bndint[i]=0;
     
      bndint[i]=do_int(p1,p2,p3,p4,ip);

      if(bndint[i]){
         // is the intersection point just one of the ends?
         if(( (fabs(ip[0]-p1[0]) <eps) & (fabs(ip[1]-p1[1]) <eps) ) |
            ( (fabs(ip[0]-p2[0]) <eps) & (fabs(ip[1]-p2[1]) <eps) )){
            bndint[i]=0;
         }
         if(( (fabs(ip[0]-p3[0]) <eps) & (fabs(ip[1]-p3[1]) <eps) ) |
            ( (fabs(ip[0]-p4[0]) <eps) & (fabs(ip[1]-p4[1]) <eps) )){
            bndint[i]=0;
         }
      }
   }
}

/* determine whether the line between two points is facing inside or outside */
int facing(double p1[], double p2[] , int nbnd, double **bnd){
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
      // call the in_out routine from soap.
      //void in_out(double *bx, double *by, double *break_code, double *x,double *y,int *in, int *nb, int *n)

      bx=(double*)malloc(sizeof(double)*nbnd);
      by=(double*)malloc(sizeof(double)*nbnd);

      for(i=0; i<nbnd; i++){
         bx[i]=bx[0]+i;
         by[i]=by[0]+i;
         bx[i]=bnd[i][0]; 
         by[i]=bnd[i][1];
      }

      // find the midpoints between p1, p2 their first intersections
      // store in x and y blocks
//      xmp[0]=(ip1[0]+p1[0])/2;
//      xmp[1]=(ip2[0]+p2[0])/2;
//      ymp[0]=(ip1[1]+p1[1])/2;
//      ymp[1]=(ip2[1]+p2[1])/2;

      xmp[0]=(ip1[0]+eps*p1[0])/(1+eps);
      xmp[1]=(ip2[0]+eps*p2[0])/(1+eps);
      ymp[0]=(ip1[1]+eps*p1[1])/(1+eps);
      ymp[1]=(ip2[1]+eps*p2[1])/(1+eps);

      // to handle holes, multiple boundaries
      // ignore this at the moment
      xmin=minarr(nbnd,bx);
      ymin=minarr(nbnd,by);
      mina[0] = xmin; mina[1]=ymin;
      break_code=minarr(2,mina)-1;

      tmpinout=2;

      in_out(bx, by, &break_code, xmp, ymp, in, &nbnd, &tmpinout);

      // if they are both inside, return true (ie they face inside)
      // or if one is on boundary and the other is inside...
      if((in[0] && in[1])){
         ret=1;
      }

      // free some memory
      free(bx);free(by);
   }
   return ret;
}

int intpoint(double p1[], double p2[],double edge[][2], double ip[]){
   double p3[2],p4[2];
   int ret;
   double arr[2];

   p3[0]=edge[0][0];
   p3[1]=edge[0][1];
   p4[0]=edge[1][0];
   p4[1]=edge[1][1];

   // if they overlap
   if(( (fabs(p1[0]-p3[0]) < eps) & 
        (fabs(p2[0]-p4[0]) < eps) &
        (fabs(p1[1]-p3[1]) < eps) & 
        (fabs(p2[1]-p4[1]) < eps) ) |
      ( (fabs(p2[0]-p3[0]) < eps) & 
        (fabs(p1[0]-p4[0]) < eps) &
        (fabs(p2[1]-p3[1]) < eps) &
        (fabs(p1[1]-p4[1]) < eps) )){
      arr[0]=p1[0];
      arr[1]=p2[0];
      ip[0]=minarr(2,arr);
      arr[0]=p1[1];
      arr[1]=p2[1];
      ip[1]=minarr(2,arr);
      return 0;
   }

   // ends
   if(online(p1,edge)){
      ip[0]=p1[0];
      ip[1]=p1[1];
      return 0;
   }
   if(online(p2,edge)){
      ip[0]=p2[0];
      ip[1]=p2[1];
      return 0;
   }

   ret = do_int(p1,p2,p3,p4,ip);

   return !ret;
}

int online(double p1[],double thisline[][2]){
 
   double m,c;
   double xarr[2], yarr[2];

   // check this first before messing around with arrays 
   // is p1 an endpoint of thisline?
   if( ((fabs(p1[0]-thisline[0][0])<eps) & (fabs(p1[1]-thisline[0][1])<eps)) |
       ((fabs(p1[0]-thisline[1][0])<eps) & (fabs(p1[1]-thisline[1][1])<eps))){
      return 1;
   }

   // now create the bounding box
   xarr[0]=thisline[0][0];
   xarr[1]=thisline[1][0];
   yarr[0]=thisline[0][1];
   yarr[1]=thisline[1][1];
   twosort(xarr);
   twosort(yarr); // make xarr, yarr small->large
      
   // check p1 is inside the bounding box
   if( ((p1[0]>xarr[1]) | (p1[0]<xarr[0])) |
       ((p1[1]>yarr[1]) | (p1[1]<yarr[0]))   ){
      return 0;
   }
 
   // calculate gradient of the line
   /* first handle if it's a vertical/horizontal line */
   if(fabs(thisline[1][0]-thisline[0][0])<eps){
      /* vertical line */
 
      if((fabs(thisline[1][0]-p1[0])<eps) &&
         ((p1[1]<yarr[1])&&(p1[1]>yarr[0]))){
         return 1;
      }else{
         return 0;
      }
 
   }else if(fabs(thisline[1][1]-thisline[0][1])<eps){
      /* horizontal line */
 
      if((fabs(thisline[1][1]-p1[1])<eps) &&
         ((p1[0]<xarr[1])&&(p1[0]>xarr[0]))){
         return 1;
      }else{
         return 0;
      }
   }else{
      m = (thisline[1][1]-thisline[0][1])/(thisline[1][0]-thisline[0][0]);
   }
 
   // calculate intercept
   c = thisline[1][1]-m*thisline[1][0];
 
   // is p1 a solution?
   if(fabs(p1[1]-(m*p1[0]+c))<eps){
      return 1;
   }else{
      return 0;
   }
}

// calculate the length of the hull by iterating over
// the list object
double hull_length(node** hull) {

   double hullen=0;
   node* current = *hull;

   if(current ==NULL){
      return(1e10);
   }

   while (current->next != NULL) {
      hullen=hullen+hypot((current->next->data[0] - current->data[0]),
                          (current->next->data[1] - current->data[1]));
      current = current->next;
   }
   return(hullen);
}

// find the first intersection points of p1 and p2
// with bnd
int first_ips(double p1[], double p2[], int nbnd, double **bnd, 
               double ip1[], double ip2[],int intind[]){   
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
   * Uses:
   *                 crapfind, qsort
   */

   int i, firstel, lastel, *retint,*retint2, *bbindex;
   int lbbindex=0;
   double thisedge[2][2], **ips, *dists, *sortdists;
   double ip[2];
   int j=0;

   // error code 0= okay
   int err=0;

   // setup retint
   retint=(int*)malloc(sizeof(int)*(nbnd-1));
   retint2=(int*)malloc(sizeof(int)*(nbnd-1));
   for(i=0; i<(nbnd-1); i++){
      retint[i]=retint[0]+i;
      retint2[i]=retint2[0]+i;
   }

   // do_intersect returns a string of T/F values
  
   // find intersections 
   // this is what is used in the R code.
   do_intersect(p1,p2,nbnd,bnd,retint);
   
   for(i=0;i<(nbnd-1);i++){
      if(retint[i]){
         if( (fabs(p1[0]-bnd[i][0])<eps) && (fabs(p1[1]-bnd[i][1])<eps)){
            ip[0]=(p1[0]+eps*p2[0])/(1+eps);
            ip[1]=(p1[1]+eps*p2[1])/(1+eps);
            thisedge[0][0]=bnd[i][0];
            thisedge[0][1]=bnd[i][1];
            thisedge[1][0]=bnd[i+1][0];
            thisedge[1][1]=bnd[i+1][1];
            do_intersect(ip,p2,nbnd,bnd,retint2);
            retint[i]=retint2[i];
            retint[i-1]=retint2[i-1];
         }else if( (fabs(p2[0]-bnd[i][0])<eps) && (fabs(p2[1]-bnd[i][1])<eps)){
            ip[0]=(p1[0]+(eps)*p2[0])/(1+eps);
            ip[1]=(p1[1]+(eps)*p2[1])/(1+eps);
            thisedge[0][0]=bnd[i][0];
            thisedge[0][1]=bnd[i][1];
            thisedge[1][0]=bnd[i+1][0];
            thisedge[1][1]=bnd[i+1][1];
            do_intersect(ip,p1,nbnd,bnd,retint2);
            retint[i]=retint2[i];
            retint[i-1]=retint2[i-1];
         }
      }
      if(!retint[i]){
         if( ( (fabs(p1[0]-bnd[i][0])<eps) && (fabs(p1[1]-bnd[i][1])<eps) )| 
             ( (fabs(p2[0]-bnd[i][0])<eps) && (fabs(p2[1]-bnd[i][1])<eps)) ){
            retint[i]=1;
         }
      }
   }

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

   // if lbbindex < 2 increment err
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
         err=intpoint(p1,p2,thisedge,ip);
         ips[i][0]=ip[0];
         ips[i][1]=ip[1];

         // find the distance and save
         dists[i]=hypot(p1[0]-ip[0],p1[1]-ip[1]);

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
      //printf("### DANGER, WILL ROBINSON! lbbindex=%d (< 2)\n",lbbindex);
      err++;
   } // end of lbbindex<2 check

   free(bbindex);
   free(dists);
   free(sortdists);
   free(retint);
   free(retint2);

   return(err);
}

// match the end of a linked list with a point
int match_ends(double *point, node** head, double **bnd, int nbnd, double *dist){
   /*
    * Args:
    *    point    the point to investigate
    *    head     the path to look at
    *
    * Return:
    *    integer 1= match top
    *            2= match bottom
    *            0= no match
   */
   node* current=NULL;
   current=*head;
   double ep[2];
   int *retint, i;

   retint=(int*)malloc(sizeof(int)*(nbnd-1));
   for(i=0; i<(nbnd-1); i++){
      retint[i]=retint[0]+i;
   }

   // check the start
   ep[0]=current->data[0];
   ep[1]=current->data[1];

   if( (fabs(ep[0]-point[0])<eps) & (fabs(ep[1]-point[1])<eps)){
//      *dist=0;
      free(retint);
      return 1;
   }
   
   do_intersect(point,ep,nbnd,bnd,retint);
   if(iarrsum(nbnd-1,retint)==0){
//      *dist=hypot(ep[0]-point[0],ep[1]-point[1]);
      free(retint);
      return 1;
   }

   // fast-forward to the end
   while (current->next != NULL){
      current=current->next;
   }

   // check the end
   ep[0]=current->data[0];
   ep[1]=current->data[1];

   if( (fabs(ep[0]-point[0])<eps) & (fabs(ep[1]-point[1])<eps)){
//      *dist=0;
      free(retint);
      return 2;
   }

   do_intersect(point,ep,nbnd,bnd,retint);
   if(iarrsum(nbnd-1,retint)==0){
//      *dist=hypot(ep[0]-point[0],ep[1]-point[1]);
      free(retint);
      return 2;
   }

   free(retint);
   // if nothing happened
   return 0;
}

// check to see if any of the ends can be used as a start path
void append_check(node** paths, int npaths, double p1[2], double p2[2], int app[2], int nbnd, double **bnd){
   /*
    * Args:
    *    paths    array of paths
    *    npaths   length of paths
    *    point    point to investigate
    *    app[2]   entry 0: match_ends output
    *             entry 1: path number
   */
   int i,me;
   double ep[2],dist1,dist2,olddist=1e12;   
   node* current=NULL;
   int* retint;

   retint=(int*)malloc(sizeof(int)*(nbnd-1));
   for(i=0; i<(nbnd-1); i++){
      retint[i]=retint[0]+i;
   }

   app[0]=0;

   // loop over paths
   for(i=0; i<npaths; i++){

      // call match_ends --  is the path between p1 and an end of i
      //                     paths[i] Euclidean in domain?
      me=match_ends(p1,&paths[i],bnd, nbnd,&dist1);

      // do any of the selected paths have a Euclidean path between
      // their other end and p2?   

      if(me!=0){

         // if p1 matched an end, then check that p2 matches the other end
         current=paths[i];

         // get the right end of paths[i]
         if(me==1){
            while (current->next != NULL){
               current=current->next;
            }
         }

         ep[0]=current->data[0];
         ep[1]=current->data[1];

         // check to see that p2->other end is Euclidean in domain
         do_intersect(p2,ep,nbnd,bnd,retint);
         if(iarrsum(nbnd-1,retint)==0){
//            dist2=hypot(ep[0]-p2[0],ep[1]-p2[1]);
            
//            if((dist1+dist2)<olddist){
               // return the orientation and path number
               app[0]=me;
               app[1]=i;
//               olddist=dist1+dist2;
//            }
               break;
         }
      }
   }
   free(retint);
}

// create a reference grid
void create_refpaths(double *xref, double *yref, int nref, int nbnd,node*** savedpaths, int *npaths, double **bnd){

   double p1[2],p2[2],*pl; 
   int i,j,k,m,err,npl;
   *npaths=0;
 
   // we know what's inside, find only those paths that are
   // non-Euclidean within the domain, check that first...
   npl=(nref*(nref-1))/2;
   pl=(double*)malloc(sizeof(double)*(npl));
   for(i=0; i<npl; i++){
      pl[i]=pl[0]+i;
   }

   get_euc_path(xref,yref,nbnd,bnd,nref,pl,0);

   // how many paths shall we calculate?   
   for(i=0;i<npl;i++){
      if(pl[i]==-1){
         (*npaths)++;
      }
   }

   // malloc the memory for the saved paths
   *savedpaths=(node**)malloc(sizeof(node*)*(*npaths));
   for(i=0; i<(*npaths); i++){
      (*savedpaths)[i]=(*savedpaths[0])+i*sizeof(node*);
      (*savedpaths)[i]=NULL;
   }

   k=0;m=0;
   // calculate some paths
   for(i=0;i<nref;i++){
      for(j=(i+1);j<nref;j++){
         if(pl[k]==-1){
            p1[0]=xref[i]; p1[1]=yref[i]; // set p1
            p2[0]=xref[j]; p2[1]=yref[j]; // set p2
            err=make_bnd_path(p1,p2,nbnd,bnd,&((*savedpaths)[m]),0);
            err=iter_path(&((*savedpaths)[m]),nbnd,bnd);
            m++;
         }
         k++;
      }
   }
   free(pl);
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

// Push something to the start of a list
void Push(node** headRef, double data[]) {
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

// Append something to the end of a list
// AppendNode with Push()
void AppendNode(node** headRef, double data[]) { 
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
// copy from head to newList
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
int Length(node** head) {
    node* current = *head;
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
void PrintPath(node** mypath) {
   node* current=*mypath;

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
void DelTopBot(node** head){
   node* current = *head;      // used to iterate over the original list
   node* top = NULL;

   // miss out the first node
   current=current->next;
   free(current->prev);
   current->prev=NULL;
   top=current; 

   // skip to the end 
   while (current->next->next != NULL) {
      current = current->next;
   }
   
   free(current->next);
   current->next=NULL;

   *head=top;
}

// Remove the first element of a list
void RMTop(node** head){
   node* current = *head;

   // miss out the first node
   current=current->next;
   free(current->prev);
   current->prev=NULL;

   *head=current;
}

// Delete the last entry of a list
void RMBot(node** head){
   node* current = *head;

   // skip to the end 
   while (current->next->next != NULL) {
      current = current->next;
   }
   
   free(current->next);
   current->next=NULL;
}


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

void twosort(double *twovec){
   // sort two vectors small->large
   double tmp;  

   if(twovec[1]<twovec[0]){
      tmp=twovec[0];
      twovec[0]=twovec[1];
      twovec[1]=tmp;
   }
}

// integer version of the above
void itwosort(int *twovec){
   // see if two vectors are small->large
   int tmp;  

   if(twovec[1]<twovec[0]){
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
int iarrsum(int narr, int *arr){
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
int compare_doubles (const void *a, const void *b){
   const double *da = (const double *) a;
   const double *db = (const double *) b;
   return (*da > *db) - (*da < *db);
}


// my very own, very poor find
// returns the first element of the array to match the value
int crapfind(int narr, double *arr, double val){
   int i, index;

   for(i=0;i<narr;i++){
      if(arr[i]==val){
         index=i;
         break;
      }
   }
   return index;
}

// routine to set the global eps value
void set_epsilon(int n, double *x, double *y){
   // x and y should be the boundary points
   // since all points are inside there...

   double machEps = 1e-10;
   double maxx,minx,maxy,miny, dxy[2];

   // first find the max and min of x and y
   maxx=maxarr(n,x);
   maxy=maxarr(n,y);
   minx=minarr(n,x);
   miny=minarr(n,y);

   // determine the machine epsilon 
   // from http://en.wikipedia.org/wiki/Machine_epsilon#Approximation_using_C
   do{
      machEps /= 2.0;
      // If next epsilon yields 1, then break, because current
      // epsilon is the machine epsilon.
   }while ((double)(1.0 + (machEps/2.0)) != 1.0);


   dxy[0]=(maxx-minx)*machEps;
   dxy[1]=(maxy-miny)*machEps;

   // now calculate our epsilon
   eps=maxarr(2,dxy);

   printf("eps=%.30f\n",eps);

}



