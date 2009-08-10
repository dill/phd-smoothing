/* First hash at re-writing the utils.R script in C */

#include <math.h>
#include <stdlib.h>
#include <stdio.h>


// definition of linked list
typedef struct node
{
   double data[2];  // space for x and y
   struct node* next; // pointer to next item
   struct node* prev; // pointer to previous item
} node;

// some prototypes
void twosort(double *);
int online(double[2],double[2][2]);
int facing(double p1[2], double p2[2] , int nbnd, double bnd[nbnd][2]);
void intpoint(double[2], double[2],double[2][2],double[2]);
void do_intersect(double[2], double[2], int nbnd, double[nbnd][2],int bndint[nbnd]);
double minarr(int narr, double arr[narr]);
double maxarr(int narr, double arr[narr]);
int compare_doubles (const void *a, const void *b);
int crapfind(int narr, double[narr], double);
int iarrsum(int narr, int arr[narr]);
double hull_length(node**);
void sp_do_intersect(double[2], double[2], int nbnd, double[nbnd][2],int[nbnd]);
int first_ips(double[2], double[2], int nbnd, double bnd[nbnd][2], 
               double[2], double[2], int[2]);
void Push(node**, double[2]);
void AppendNode(node**, double[2]);
node* CopyList(node*);
int Length(node*);

// for in_out in separate file
void in_out(double *, double *, double *,double *,double *,int *, int *, int * );


// do two points and the boundary intersect?
void do_intersect(double p1[2], double p2[2], int nbnd, double bnd[nbnd][2],int bndint[nbnd])
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
   double pbbox[2][2], ebbox[2][2], thisedge[2][2], ip[2];

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
   for (i=0;i<nbnd;i++){

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
         intpoint(p1,p2,thisedge,ip);

// NEED TO DO SOME ERROR CHECKING HERE!
//         if(is.na(ip$x)|is.na(ip$y)){
//            bndint[i]<-FALSE
//         }else{

            // first need to handle the horizontal and vertical line cases
            // then handle whether the intersection point lies within the
            // the bounding box
            if(fabs(ebbox[0][0]-ebbox[1][0])>=eps){
               if(ip[0]>=ebbox[0][0] | ip[0]<=ebbox[1][0]) bndint[i]=0;
            }
            if(fabs(pbbox[0][0]-pbbox[1][0])>=eps){
               if(ip[0]>=pbbox[0][0] | ip[0]<=pbbox[1][0]) bndint[i]=0;
            }
            if(fabs(ebbox[0][1]-ebbox[1][1])>=eps){
               if(ip[1]>=ebbox[0][1] | ip[1]<=ebbox[1][1]) bndint[i]=0;
            }
            if(fabs(pbbox[0][1]-pbbox[1][1])>=eps){
               if(ip[1]>=pbbox[0][1] | ip[1]<=pbbox[1][1]) bndint[i]=0;
            }

      if(bndint[i]){
               if(fabs(ebbox[0][0]-ebbox[1][0])<=eps){
                  if(ip[1]>=ebbox[0][1] | ip[1]<=ebbox[1][1]) bndint[i]=0;
               }
               if(fabs(pbbox[0][0]-pbbox[1][0])<=eps){
                  if(ip[1]>=pbbox[0][1] | ip[1]<=pbbox[1][1]) bndint[i]=0;
               }
               if(fabs(ebbox[0][1]-ebbox[1][1])<=eps){
                  if(ip[0]>=ebbox[0][0] | ip[0]<=ebbox[1][0]) bndint[i]=0;
               }
               if(fabs(pbbox[0][1]-pbbox[1][1])<=eps){
                  if(ip[0]>=pbbox[0][0] | ip[0]<=pbbox[1][0]) bndint[i]=0;
               }
             }
         }
// error checking end brace!
//      }
   }
}


// special do_intersect, thinks that points that start/end at the same
// place don't intersect. Neither do exactly overlapping lines.
void sp_do_intersect(double p1[2], double p2[2], int nbnd, double bnd[nbnd][2],int bndint[nbnd])
{

   int i, tmpnbnd, tmpbndint[1];
   double tmpbnd[2][2];

   // iterate over sides (ie vertex pairs)
   // NB the last vertex should be the first
   for(i=0;i<nbnd;i++){
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
            1 if facing inside, 0 otherwise
   */

   int ret=0;
   int i, err, intind[2];
   double ip1[2],ip2[2];

   err=first_ips(p1, p2, nbnd, bnd, ip1, ip2, intind);
//DEBUG
//printf("first_ips error=%d\n",err);
//printf("ip1=list(x=%f,y=%f)\n",ip1[0],ip1[1]);
//printf("ip2=(x=%f,y=%f)\n",ip2[0],ip2[1]);


   // if there are no errors, go ahead
   if(err==0){
//DEBUG
//printf("first_ips error=%d\n",err);
      // midpoint between p1 and first intersection 
      double p1mp[2]={(ip1[0]+p1[0])/2,(ip1[1]+p1[1])/2};
      // midpoint between p2 and first intersection 
      double p2mp[2]={(ip2[0]+p2[0])/2,(ip2[1]+p2[1])/2};

      // are the midpoints inside?
      // ret<-inSide(bnd,c(p1.mp$x,p2.mp$x),c(p1.mp$y,p2.mp$y))
      // call the in_out routine from soap. Need to make sure that things are
      // in the right format
      //void in_out(double *bx, double *by, double *break_code, double *x,double *y,int *in, int *nb, int *n)

      double bx[nbnd];
      double by[nbnd];
      for(i=0;i<nbnd;i++){
         bx[i]=bnd[i][0]; 
         by[i]=bnd[i][1];
      }

      // find the midpoints between p1, p2 their first intersections
      // store in x and y blocks
      double xmp[2]={(ip1[0]+p1[0])/2,(ip2[0]+p2[0])/2};
      double ymp[2]={(ip1[1]+p2[1])/2,(ip2[1]+p2[1])/2};

      // to handle holes, multiple boundaries
      // don't handle this at the moment
      double break_code=1.0e6;

 // DEBUG
//printf("xmp=(%f,%f)\n",xmp[0],xmp[1]);
//printf("ymp=(%f,%f)\n",ymp[0],ymp[1]);

      int in[2]={0,0};
      int tmpinout=2;

      in_out(&bx, &by, &break_code, &xmp, &ymp, &in, &nbnd, &tmpinout);

      // if they are both inside, return true
      if(in[0] && in[1]) ret=1;

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

      ip[0]=(b1*c2-b2*c1)/(a1*b2-a2*b1);
      ip[1]=(c1*a2-a1*c2)/(a1*b2-b1*a2);

//   }else{
//      ret<-list(x=Inf,y=Inf)
//   }

// do something with return!
//   return ret;

}



/* find if a point is on a line */
int online(double p1[],double thisline[][2])
{
   // uses: twosort
   // returns 1 if the point is on the line, 0 otherwise


   /* So here we just use an n by 2 matrix to represent
      the points, first col is x, second y, obv.*/
   double eps, leftside, rightside,xarr[2],yarr[2];
   
   /* Take this global at some point*/
   eps=1.0e-10;



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


// calculate the length of the hull by iterating over
// the list object
double hull_length(node** hull) {

   double hullen=0;
   node* current = *hull;

   // this is a bit ugly
   while (current->next != NULL) {
      hullen=hullen+hypot((current->next->data[0]-current->data[0]),
                          (current->next->data[1]-current->data[1]));
      current = current->next;
   }
   return(hullen);
}

///////////////////////////////////
// find the first intersection points of p1 and p2
// with bnd
int first_ips(double p1[2], double p2[2], int nbnd, double bnd[nbnd][2], 
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

   int i, lbbindex, firstel, lastel, retint[nbnd];
   double thisedge[2][2];
   double ip[2];

   // error code 0= okay
   int err=0;

   // do the bounding box check first, for speed
   // do_intersect returns a string of T/F values
   // default set to true
//   int retint[nbnd];
//   for(i=0;i<nbnd;i++){
//      retint[i]=1;
//   }
  
   // find intersections 
   do_intersect(p1,p2,nbnd,bnd,retint);
   
// DEBUG
//printf("retint=");
//int k;
//for(k=0;k<nbnd;k++){
//	printf("%d ",retint[k]);
//}
//printf("\n");


   // length of the bounding box index
   lbbindex=iarrsum(nbnd,retint);

// DEBUG
//printf("retint=");
//int k;
//for(k=0;k<(nbnd-1);k++){
//   printf(" %d,",retint[k]);
//}
//printf("\n");

// DEBUG
//printf("lbbindex=%d\n",lbbindex);
   
   // if lbbindex < 1 increment err
   if(lbbindex>1){
      // find intersections & sort by distance
      // bbindex=c(1:(length(bnd$x)-1))[doint]
      int bbindex[lbbindex];
      int j=0;

      // populate bbindex   
      for(i=0;i<nbnd;i++){
         if(retint[i]){
            bbindex[j]=i;
            j++;
         }
      }

      // hold distances and intersection points temporarily
      double dists[lbbindex];
      double sortdists[lbbindex];
      double ips[lbbindex][2];

      for(i=0;i<lbbindex;i++){

         // get current index
         j=bbindex[i];
//printf("first_ips: bbindex[%d]=%d\n",i,bbindex[i]);

         thisedge[0][0]=bnd[j][0];
         thisedge[1][0]=bnd[j+1][0];
         thisedge[0][1]=bnd[j][1];
         thisedge[1][1]=bnd[j+1][1];

         // calculate and save the intersection
         intpoint(p1,p2,thisedge,ip);
// DEBUG
//printf("****************\n");
//printf("plot(bnd,type=\"l\")\n");
//printf("ip=list(x=%f,y=%f)\n",ip[0],ip[1]);
//printf("p1=list(x=%f,y=%f)\n",p1[0],p1[1]);
//printf("p2=list(x=%f,y=%f)\n",p2[0],p2[1]);
//printf("edge=list(x=c(%f,%f),y=c(%f,%f))\n",thisedge[0][0],thisedge[1][0],
//                                            thisedge[0][1],thisedge[1][1]);
//printf("lines(edge,col=\"green\",lwd=2);\n");
//printf("points(p1); points(p2);\n"); 
//printf("points(ip,col=\"red\",pch=24)\n"); 
//printf("****************\n");

         ips[i][0]=ip[0];
         ips[i][1]=ip[1];

         // find the distance and save
         dists[i]=hypot(p1[0]-ip[0],p1[1]-ip[1]);
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
      qsort(&sortdists,lbbindex,sizeof(double),compare_doubles);

// DEBUG
//printf("**************qsort debug ************\n");
//int k;
//for(k=0;k<lbbindex;k++){
//   printf("%f,",dists[k]);
//}
//printf("\n");
//for(k=0;k<lbbindex;k++){
//   printf("%f,",sortdists[k]);
//}
//printf("\n");
//printf("**************END qsort debug ************\n");
///////////////////////////////////



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

   }else{
      // let the Robot warn us...
//      printf("DANGER, WILL ROBINSON! lbbindex<1\n");
      err++;
   }

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
void Push(node** headRef, double data[2]) {
   node* newNode = malloc(sizeof(node));
   //node* head = *headRef; 
   newNode->data[0] = data[0];
   newNode->data[1] = data[1];

   // make prev of head the new node
   //*headRef->prev = newNode; 

   // next element of the new node will be the head
   newNode->next = *headRef;  // The '*' to dereferences back 
								 		// to the real head


	if(newNode->next !=NULL){
	   newNode->next->prev = newNode;
   	//*headRef->prev = newNode;
	}


  
   // previous element of new node will be NULL
   newNode->prev = NULL;  // The '*' to dereferences back to 
								  // the real head
   *headRef = newNode;        // ditto

}

//struct node* AppendNode(struct node** headRef, double data[2]) {
// think this will work
void AppendNode(node** headRef, double data[2]) {
   node* current = *headRef;
   node* newNode;
   newNode = malloc(sizeof(node));
   newNode->data[0] = data[0];
   newNode->data[1] = data[1];
   newNode->next = NULL;
   // special case for length 0
   if (current == NULL) {
      newNode->prev = NULL;
      *headRef = newNode;
   }else{
      // Locate the last node
      while (current->next != NULL) {
         current = current->next;
      }
      newNode->prev = current;
      current->next = newNode;

   }
}


// Variant of CopyList() that uses Push()
node* CopyList(node* head)
{
   node* current = head;      // used to iterate over the original list
   node* newList = NULL;      // head of the new list
   node* tail = NULL; // kept pointing to the last node in the new list
   while (current != NULL) {
      if (newList == NULL) { // special case for the first new node
         Push(&newList, current->data);
         tail = newList;
      }else{
         Push(&(tail->next), current->data);     // add each node at the tail
         tail = tail->next;    // advance the tail to the new last node
      }
      current = current->next;
   }
   return(newList);
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





