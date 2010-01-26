// Simon's algorithm for finding the path
// Copyright 2009 David Lawrence Miller

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "utils.h"
#include "wood.h"

void wood_path(int *len, int *start, double *x, double *y, int *nbnd, double *xbnd, double *ybnd,double *pathlen)
{
   // args:
   //   len         the length of x and y *
   //   start       point to start at *
   //   x,y         lists of x and y points
   //   nbnd        length of bnd
   //   bnd         the boundary that got in the way
   //  return:
   //   length of the path

   // * these can be manipulated to control which we evaluate
   //  eg. in the insertion case

   double **bnd, p1[2], p2[2];
   int i,j,k, *retint;

   bnd=(double**)malloc(sizeof(double*)*(*nbnd));
   bnd[0]=(double*)malloc(sizeof(double)*(*nbnd)*2);

   // put bnd in the right format and allocate memory
   for(i=0; i<*nbnd; i++){
      bnd[i]=bnd[0]+i*2;

      bnd[i][0]=xbnd[i];
      bnd[i][1]=ybnd[i];
   }

   // allocate memory for retint
   retint=(int*)malloc(sizeof(int)*(*nbnd-1));
   for(i=0; i<(*nbnd-1); i++){
      retint[i]=retint[0]+i;
   }

   // insertion counter
   k=0;

   // switch between insertion and full MDS 
   // insertion format is c(old,new)
   //  * so *start gives the index of the limit of the old points for insertion
   //    if we're not doing insertion then this is just the length of the 
   //    point vector
   if(*start != 0){
      // #### Main loop for INSERTION
      //for(i=0; i<(*len); i++){
      for(i=0; i<(*start); i++){
         // set p1
         p1[0]=x[i]; p1[1]=y[i];

         for(j=(*start); j<(*len); j++){
            // set p2
            p2[0]=x[j]; p2[1]=y[j];
  
            // check to see if we have to do the path finding or
            // just the hypotenuse 
            do_intersect(p1, p2, *nbnd, bnd, retint);
            //                           vvv just hypot when we touch only 1 vertex 
            if(iarrsum((*nbnd-1), retint)>1){
               pathlen[k]=make_path(p1,p2,*nbnd,bnd);
            }else{
               pathlen[k]=hypot(p2[0]-p1[0],p2[1]-p1[1]);
            }
            // increment pathlen counter
            k++;
         }    
      }
   }else{
      // #### Main for loop for FULL MDS
      for(i=0; i<(*len); i++){
         // set p1
         p1[0]=x[i]; p1[1]=y[i];

         for(j=(i+1); j<(*len); j++){
            // set p2
            p2[0]=x[j]; p2[1]=y[j];
 
            // DEBUG
            printf("(i,j)=(%d,%d)\n",i,j);
            printf("p1=(%f,%f)\n",p1[0],p1[1]);
            printf("p2=(%f,%f)\n",p2[0],p2[1]);
 
            // check to see if we have to do the path finding or
            // just the hypotenuse 
            do_intersect(p1, p2, *nbnd, bnd, retint);
   
            // DEBUG
            //printf("ints=%d\n",iarrsum((*nbnd-1),retint));
   
            //                           vvv just hypot when we touch only 1 vertex 
            if(iarrsum((*nbnd-1), retint)>1){
               pathlen[k]=make_path(p1,p2,*nbnd,bnd);
            }else{
               pathlen[k]=hypot(p2[0]-p1[0],p2[1]-p1[1]);
            }
            // increment pathlen counter
            k++;
         }    
      }
   }

   // loop over all point pairs
   // insertion case: go from old points to new points

   free(bnd[0]);
   free(bnd);
   free(retint);
}


double make_path(double p1[2], double p2[2], int nbnd, double **bnd)
{
   int conv, conv_stop;
   double hulllen;
   node* prevpath=NULL;
   node* mypath=NULL;
   
   // used for debugging, below...
   //node* current=NULL;

   // create the initial path:
   // p1, p1 1st intersection, some of bnd, p2 1st intersection, p2
   make_bnd_path(p1,p2,nbnd,bnd,&mypath);

   // convergence stop
   conv=0;
   conv_stop=10;

   // keep going until we don't remove any more points.
   do{

      // free up prevpath before we copy onto it
      if(conv>0){
         FreeList(&prevpath);
      }

      // save previous path
      //prev.path<-my.path
      CopyList(mypath,&prevpath);

      // delete step, remove anything that doesn't need to be there
      delete_step(&mypath,nbnd,bnd);

      // add new vertices
      alter_step(&mypath,nbnd,bnd);

      // increment convergence stopper 
      conv++;

   } while( !(has_converged(prevpath,mypath)) & (conv<conv_stop) );

   // DEBUG
//   printf("#******* END  ********** \n");
//   printf("plot(bnd,type=\"l\")\n");
//   printf("path<-list(x=c(),y=c())\n");
//   
//   current=mypath;
//   while(current!=NULL){
//      printf("path$x<-c(path$x,%f)\n",current->data[0]);
//      printf("path$y<-c(path$y,%f)\n",current->data[1]);
//      current=current->next;
//   }
//   printf("lines(path,lwd=2,col=\"red\")\n");
//   printf("scan()\n");

//   if(conv==conv_stop){
//      printf("WARNING: path find finished without convergence!\n");
//      printf("conv = %d\n",conv);
//      printf("convergence = %d\n",has_converged(prevpath,mypath) );
//   }

   // return the length of the path
   hulllen=hull_length(&mypath);

   FreeList(&mypath);
   FreeList(&prevpath);

   // return the length of the path
   return(hulllen);
}


// create a path between p1 and p2 using the boundary
void make_bnd_path(double p1[2], double p2[2], int nbnd, double **bnd, node** path)
{
   /* Args:
    *  p1, p2           points
    *  nbnd             length of boundary
    *  bnd              boundary
    * Return:
    *          head node of the linked list
    */

   // find the first intersection between p1, p2 and the boundary side that 
   // each point intersects
   double ip1[2],ip2[2], curr_insert[2];
   int intind[2],i,start;
   int err=0;
   double line1[2][2], line2[2][2];
   node* bnd1 = NULL;
   node* bnd2 = NULL;

   err=first_ips(p1, p2, nbnd, bnd, ip1, ip2, intind);

	// if there are no errors
   if(err==0){

      // first sort the intersection indices
      itwosort(intind);

      // want elements intind[0]-1 to intind[1 ](inclusive)
      //picker<-sort(c(ip1.index[1],(ip1.index[length(ip1.index)]+1)))
      //picker<-c(picker[1]:picker[2])
      //bnd.1.sort<-pe(bnd,picker)

      // since we ordered intind first, we don't need to worry too much

      // push everything in
      //                     vvvvvvvvvv <- since we want it to be inclusive
      for(i=(intind[0]+1);i<(intind[1]+1);i++){
         curr_insert[0]=bnd[i][0];
         curr_insert[1]=bnd[i][1];
         Push(&bnd1,curr_insert);
      }

      // create the second boundary segment
      // want intind[1]+1:intind[0]
      // bnd.2.sort<-pe(pe(bnd,c(1:(length(bnd$x)-1))),
      //       setdiff(c(1:(length(bnd$x)-1)),picker))
      // bnd.2.sort<-pe(bnd.2.sort,c(rev(1:(picker[1]-1)),
      //       length(bnd.2.sort$x):picker[1]))

      // handle the case where start is actually the end
      if(intind[1]!=nbnd){
         start=intind[1];
      }else{
         start=1;
      }

      // insert until we hit the end 
      for(i=start;i<nbnd;i++){
         curr_insert[0]=bnd[i][0];
         curr_insert[1]=bnd[i][1];
         Push(&bnd2,curr_insert);
      }

      // insert from the end back to intend[0] 
      //             vvvvvvv don't include intind[0] twice
      for(i=(nbnd-1);i>=intind[0];i--){
         curr_insert[0]=bnd[i][0];
         curr_insert[1]=bnd[i][1];
         AppendNode(&bnd2,curr_insert);
      }

      line1[0][0]=bnd[intind[0]][0];
      line1[0][1]=bnd[intind[0]][1];
      line1[1][0]=bnd[intind[0]+1][0];
      line1[1][1]=bnd[intind[0]+1][1];

      line2[0][0]=bnd[intind[1]][0];
      line2[0][1]=bnd[intind[1]][1];
      line2[1][0]=bnd[intind[1]+1][0];
      line2[1][1]=bnd[intind[1]+1][1];

      if( online(ip1,line1) & online(ip2,line2)){
         curr_insert[0]=ip1[0]; curr_insert[1]=ip1[1];
         AppendNode(&bnd1,curr_insert);
         Push(&bnd2,curr_insert); 
   
         curr_insert[0]=p1[0]; curr_insert[1]=p1[1];
         AppendNode(&bnd1,curr_insert);
         Push(&bnd2,curr_insert); 
   
         curr_insert[0]=ip2[0]; curr_insert[1]=ip2[1];
         Push(&bnd1,curr_insert);
         AppendNode(&bnd2,curr_insert); 
   
         curr_insert[0]=p2[0]; curr_insert[1]=p2[1];
         Push(&bnd1,curr_insert);
         AppendNode(&bnd2,curr_insert); 
      }else{
         curr_insert[0]=ip1[0]; curr_insert[1]=ip1[1];
         Push(&bnd1,curr_insert);
         AppendNode(&bnd2,curr_insert);
   
         curr_insert[0]=p1[0]; curr_insert[1]=p1[1];
         Push(&bnd1,curr_insert);
         AppendNode(&bnd2,curr_insert);
   
         curr_insert[0]=ip2[0]; curr_insert[1]=ip2[1];
         AppendNode(&bnd1,curr_insert);
         Push(&bnd2,curr_insert);
   
         curr_insert[0]=p2[0]; curr_insert[1]=p2[1];
         AppendNode(&bnd1,curr_insert);
         Push(&bnd2,curr_insert);
      }

      // pick the shorter path to return
      if(hull_length(&bnd1)<hull_length(&bnd2)){
//         CopyList(bnd1,path);
         *path=bnd1;
         FreeList(&bnd2);
      }else{
//         CopyList(bnd2,path);
         *path=bnd2;
         FreeList(&bnd1);
      }


   }else{ // end of error if()
      printf("ERROR: make_bnd_path FAILED. Error returned from first_ips\n");
      printf("DEBUG: p1=list(x=%f,y=%f); p2=list(x=%f,y=%f);\n",p1[0],p1[1],p2[0],p2[1]);
   }

}


// iterate over the points in the path:
// delete as many points as possible, making sure that the
// path is still outside
void delete_step(node** path, int nbnd, double **bnd)
{
   // Args:
   //  path     the current path
   //  nbnd     # elements of boundary
   //  bnd      the boundary
   // Return:
   //           revised path with dropped vertices
   
   int i, *intbnd, in[1];
   double mytrip[3][2], p1[2], p2[2], xmp[1], ymp[1], *bx, *by, xmin, ymin, mina[2], break_code;
	int inout_n=1;
   // convergence stop
   int conv=0;
   int conv_stop=10;
   
   // some linked lists
   node* current=NULL;   // iterator
   node* prevpath=NULL;
   // needed for deletion
   node* start_ptr=NULL;
   node* end_ptr=NULL;

   // setup intbnd
   intbnd=(int*)malloc(sizeof(int)*(nbnd-1));
   for(i=0; i<(nbnd-1); i++){
      intbnd[i]=intbnd[0]+i;
   }

   ////// needed later on for the in_out() call
   bx=(double*)malloc(sizeof(double)*nbnd);
   by=(double*)malloc(sizeof(double)*nbnd);
   for(i=0; i<nbnd; i++){
      bx[i]=bx[0]+i;
      by[i]=by[0]+i;
      bx[i]=bnd[i][0];
      by[i]=bnd[i][1];
   }
   
   // to handle holes, multiple boundaries
   // ignore this at the moment
   xmin=minarr(nbnd,bx);
   ymin=minarr(nbnd,by);
   mina[0] = xmin; mina[1] = ymin;
   break_code=minarr(2,mina)-1;

   // keep going until we don't remove any more points.
   do{
      if(conv>0){
         FreeList(&prevpath);
      }
      // use prevpath to keep a copy of the previous path for comparison
      //prev.path<-path
      CopyList(*path,&prevpath);

      // start point for triplet selection
      current = *path;   // iterator

      //while((i+1)<=length(path$x)){
      while(current!=NULL){

         // equivalent of some ANDs in the above, but doesn't cause a memory
         // problem since the while() evaluates all of the conditions.
         if(current->next==NULL){
            break;
         }else if(current->next->next==NULL){
            break;
         }

         // create the current triplet to inspect
         //my.trip<-pe(path,c(i-1,i,i+1))
         for(i=0;i<3;i++){ 
            mytrip[i][0]=current->data[0]; mytrip[i][1]=current->data[1];
				// don't go to far...
				if(i!=2){
	            current=current->next; 
				}
         }

         // pointer is now at i+1

         // if we are going forward and back again, just remove the point
         // in this case we remove the ith and (i+1)th entries, point (i-1)th
         // next to the (i+2)th.
         // path<-pe(path,-c(i,i+1))
         if((mytrip[0][0]==mytrip[2][0]) & (mytrip[0][1]==mytrip[2][1])){
            // current is sitting at the 3rd entry
            // create a pointer to that
            end_ptr=current->next; // pointer to i+2

            // go back twice
            current=current->prev; // pointer now at i
            current=current->prev; // pointer at i-1

            // change where next points to
            current->next=end_ptr; // point i-1 next to i+2

            start_ptr=current; // pointer to i-1

            // go forward again (remember next has changed)
            current=current->next; // back to i+2
            
            // free memory of i and i+1
            free(current->prev->prev);
            free(current->prev);

            // change previous
            current->prev=start_ptr; //set i+2 prev to i-1
         }else{
            //// This is all setup for the next if() vvvvv

            // start and end points of the trip in p1 and p2
            p1[0]=mytrip[0][0];  p1[1]=mytrip[0][1];
            p2[0]=mytrip[2][0];  p2[1]=mytrip[2][1];

            // see if the line between p1 and p2 intersects the boundary
            sp_do_intersect(p1,p2, nbnd,bnd,intbnd);

            // midpoints
            xmp[0]=(mytrip[2][0]+mytrip[0][0])/2;
            ymp[0]=(mytrip[2][1]+mytrip[0][1])/2;

            //if(all(!sp_do_intersect(pe(my.trip,1),pe(my.trip,3),bnd))&
            //  inSide(bnd,(pe(my.trip,3)$x+pe(my.trip,1)$x)/2,
            //             (pe(my.trip,3)$y+pe(my.trip,1)$y)/2)){
            in[0]=1;
            in_out(bx,by,&break_code,xmp,ymp,in, &nbnd,&inout_n);

            // if deleting point i makes the resulting line cross the
            // the boundary then keep it in the path, else get rid of it 
            //path<-pe(path,-i)
            
            // first part asks if there are any intersections, if there are
            // none then that's okay. Second part asks if midpoints are inside.
            if((iarrsum((nbnd-1),intbnd)==0) & in[0]){
               // remove point i by setting next pointer from i-1 to 
               // i+1, and prev from i+1 to i-1

               // current is sitting at the 3rd entry
               // create a pointer to that
               end_ptr=current; // pointer to i+1

               // go back twice
               current=current->prev; // pointer now at i
               current=current->prev; // pointer at i-1

               // change where next points to
               current->next=end_ptr; // point i-1 next to i+1
               start_ptr=current; // pointer to i-1

               // go forward again (remember next has changed)
               current=current->next; // back to i+1
               
               // free i's memory
               free(current->prev);

               // change previous
               current->prev=start_ptr; //set i+1 prev to i-1

               //current=current->next;
               // now current is sitting at i+2
            } // end if on del middle
         } // end of if del back-and-forth

        	current=current->prev; // go back to i, need this to catch all triplets

      } // end iteration over path
      conv++; // increment run counter

   } while( !(has_converged(prevpath,*path)) & (conv<conv_stop) );
   // end of do loop

   // DEBUG
   //printf("converge: %d, conv=%d, conv_stop=%d\n",has_converged(prevpath,*path),conv,conv_stop);

   // free some memory
   free(bx); free(by);
   free(intbnd);
   FreeList(&prevpath);

}           

// alter the path
void alter_step(node** path, int nbnd, double **bnd)
{
   // Args:
   //  path     the current path
   //  nbnd     # elements of boundary
   //  bnd      the boundary
   // Return:
   //           revised path with added/ammended vertices

   double ep1[2], ep2[2], mid[2], triplen;

   node* prevpath=NULL;
   node* newpath=NULL;
   node* end_ptr=NULL;
   node* current=NULL;

   // convergence stop
   int conv=0;
   int conv_stop=10;

   // iterate over the points in the path:
   // alter the path, until on two(?) consecutive runs there are
   // no changes to the path
   do{

      // iterator
      current = *path;

      if(conv>0){
         FreeList(&prevpath);
      }
      // use prevpath to keep a copy of the previous path for comparison            
      //prev.path<-path
      CopyList(*path,&prevpath);

      // start point for triplet selection
      //i<-2
      //while((i+1)<=length(path$x)){
      while(current!=NULL){
         // equivalent of some ANDs in the above, but doesn't cause a memory
         // problem when the while() evaluates all of the conditions.
         if(current->next==NULL){
            break;
         }else if(current->next->next==NULL){
            break;
         }
         // for each point i, look at the line i-1 to i+1

         // create the current triplet to inspect
         // make a copy of it and work out its length
         //my.trip<-pe(path,c(i-1,i,i+1))
         //ep1<-pe(my.trip,1)
         //ep2<-pe(my.trip,3)
   
         ep1[0]=current->data[0];
         ep1[1]=current->data[1];
        	current=current->next;
         mid[0]=current->data[0];
         mid[1]=current->data[1];
        	current=current->next;
         ep2[0]=current->data[0];
         ep2[1]=current->data[1];

         // current at i+1
         
         // calculate old trip length...
         triplen=hypot(mid[0]-ep1[0],mid[1]-ep1[1]);
         triplen=triplen+hypot(ep2[0]-mid[0],ep2[1]-mid[1]);

         // does it go inside-outside-inside?
         if(facing(ep1, ep2, nbnd, bnd)){

            // create a new path
            make_bnd_path(ep1,ep2,nbnd,bnd,&newpath);

            // only insert the path if it's better!
            if(hull_length(&newpath)<=triplen){

               // create new path, compare complete new path with old one, if the
               // new one is better then keep it.
               //new.path<-delete_step(list(
               //   x=c(path$x[1:(i-1)],new.path$x,path$x[(i+1):length(path$x)]),
               //   y=c(path$y[1:(i-1)],new.path$y,path$y[(i+1):length(path$y)])),bnd)
               
               // modify path
               // from i-1, stitch in newpath up to i+1
               // current is at i+1 now
               
               // create a pointer to that
               end_ptr=current; // pointer to i+1
               
               // go back twice
               current=current->prev; // pointer now at i
               current=current->prev; // pointer at i-1
               
               // change where next points to
               newpath->prev=current; // point head of newpath back 

               free(current->next); // free the memory at i

               current->next=newpath; // point i-1 next to the head of newpath
      
               // fast forward to the end of newpath
               while(current->next!=NULL){
                  current=current->next;
               }
   
               // point the end of newpath to i+1
               end_ptr->prev=current; // set previous of i+1 to be the end of newpath
               current->next=end_ptr;
   
               current=current->next; // current now at i+1
            }else{
               
               FreeList(&newpath);
            }// end insert if 
         } // end facing
         
        	current=current->prev; // go back to i, need this to catch all triplets
      } // end of iteration over the path
      conv++;
   } while( !(has_converged(prevpath,*path)) & (conv<conv_stop) );
   //end of main do

   FreeList(&prevpath);

}

// check convergence
int has_converged(node* path1, node* path2)
{
   // args
   //    path1, path2      the two paths to compare
   // return
   //    1 if the paths are the same, 0 otherwise
   node* current1 = path1;
   node* current2 = path2;

   // first check length then their contents
   if(Length(path1)==Length(path2)){
      // check if the new and old paths are the same
      while(current1!=NULL){
         if(( (current1->data[0]) != (current2->data[0]) ) & 
            ( (current1->data[1]) != (current2->data[1]) )) return 0;
         current1=current1->next;
         current2=current2->next;
      }
   }else return 0;

   return 1;
}


