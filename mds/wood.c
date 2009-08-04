// Simon's algorithm for finding the path

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "utils.h"

double wood_path(double[2], double[2], int nbnd, double[nbnd][2]);
node* make_bnd_path(double[2], double[2], int nbnd, double[nbnd][2]);
void delete_step(node**, int nbnd, double[nbnd][2]);
void alter_step(node**, int nbnd, double[nbnd][2]);
int has_converged(node*, node*);

double wood_path(double p1[2], double p2[2], int nbnd, double bnd[nbnd][2])
{
   // args:
   //   p1, p2      the two points to find the path between
   //   nbnd        length of bnd
   //   bnd         the boundary that got in the way
   //  return:
   //   length of the path

   double arr[2], tmp;

   int conv, conv_stop;

   node* prevpath=NULL;
   node* mypath=NULL;

   // HACK:make sure that the points are defined from the left,
   // this may or may not be a fix to the paths joining the wrong points
   arr[0]=p1[0]; arr[1]=p1[1];
   
   twosort(arr);
   if(arr[0]!=p1[0]){ 
      p1[0]=p2[0];
      p2[0]=arr[1];
      tmp=p1[1];
      p1[1]=p2[1];
      p2[1]=tmp;
   }

   // create the initial path:
   // p1, p1 1st intersection, some of bnd, p2 1st intersection, p2
   mypath=make_bnd_path(p1,p2,nbnd,bnd);

// DEBUG
printf("--------------------------\n");
printf("plot(bnd,type=\"l\")\n");
printf("path<-list(x=c(),y=c())\n");

   node* current=mypath;
   while(current!=NULL){
      printf("path$x<-c(path$x,%f)\n",current->data[0]);
      printf("path$y<-c(path$y,%f)\n",current->data[1]);
      current=current->next;
   }

printf("lines(path,lwd=2,col=\"red\")\n");
printf("--------------------------\n");

   // convergence stop
   conv=0;
   conv_stop=10;


// DEBUG
int i=0;

   // keep going until we don't remove any more points.
   do{

// DEBUG
printf("run %d\n",i);
i++;

      // save previous path
      //prev.path<-my.path
      prevpath=CopyList(mypath);

      // delete step, remove anything that doesn't need to be there
      delete_step(&mypath,nbnd,bnd);
// DEBUG
printf("***got past delete_step \n");


// DEBUG
if(!has_converged(prevpath,mypath)){
   printf("*************************\ndelete change!\n");
   current=mypath;
   while(current!=NULL){
      printf("path$x<-c(path$x,%f)\n",current->data[0]);
      printf("path$y<-c(path$y,%f)\n",current->data[1]);
      current=current->next;
   }
}


      // add new vertices
      alter_step(&mypath,nbnd,bnd);
// DEBUG
printf("***got past alter_step \n");
printf("alter change? %d\n",has_converged(prevpath,mypath));

      // increment convergence stopper 
      conv++;

// DEBUG
current=mypath;
while(current!=NULL){
   printf("path$x<-c(path$x,%f)\n",current->data[0]);
   printf("path$y<-c(path$y,%f)\n",current->data[1]);
   current=current->next;
}

   } while(!has_converged(prevpath,mypath) & (conv<conv_stop));



   // return the length of the path
   return(hull_length(mypath));
}




// create a path between p1 and p2 using the boundary
node* make_bnd_path(double p1[2], double p2[2], int nbnd, double bnd[nbnd][2])
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
   int intind[2],i,j, nbnd1, nbnd2, start, err;

   err=first_ips(p1, p2, nbnd, bnd, ip1, ip2, intind);

	// if there are no errors
   if(err==0){

      // This is quite horrible code.
      // What we do is: take the ordering that makes sense first
      // eg 1:5 not 5:1, make that set of edges, then take the difference
      // between that set and the complete set of vertices.

      // first sort the intersection indices
      //
      //  IGNORE AT THE MOMENT
      //twosort(intind);

      // want elements intind[0]-1 to intind[1 ](inclusive)
      //picker<-sort(c(ip1.index[1],(ip1.index[length(ip1.index)]+1)))
      //picker<-c(picker[1]:picker[2])
      //bnd.1.sort<-pe(bnd,picker)

      // initialize a linked list, first the head
      node* bnd1 = NULL;
      //bnd1 = malloc(sizeof(struct node*));
      // ^^^^^^^^^^^^ needed?

      nbnd1=intind[1]-intind[0]-1-1; // number of elements
      // remember bnd is of size nbnd, but first and last elements
      // are the same.

      // since we ordered intind first, we don't need to worry too
      // much about 

      // push everything in
      //                     vvvvvvvvvv <- since we want it to be inclusive
      for(i=(intind[0]+1);i<(intind[1]+1);i++){
         curr_insert[0]=bnd[i][0];
         curr_insert[1]=bnd[i][1];
         Push(&bnd1,curr_insert);
      }



// Hope this doesn't happen at the moment   
//      // make sure we aren't adding a superfluous vertex (not both end of a side
//      // that we don't need)
//      // ie. ip1, vertex, vertex... rather than vertex,ip1,vertex
//   
//   // increased to 2, maybe this fixes things...
//      if(length(picker)>2){
//   
//         if(on_line(ip1,pe(bnd.1.sort,1:2))){
//            bnd.1.sort<-pe(bnd.1.sort,-1)
//            picker<-picker[-1]
//         }
//         ep.1<-length(bnd.1.sort$x)
//   
//         if(on_line(ip2,pe(bnd.1.sort,(ep.1-1):ep.1))){
//            bnd.1.sort<-pe(bnd.1.sort,-ep.1)
//            picker<-picker[-ep.1]
//         }
//      }
   

      // create the second boundary segment
      // want intind[1]+1:intind[0]
      // bnd.2.sort<-pe(pe(bnd,c(1:(length(bnd$x)-1))),
      //       setdiff(c(1:(length(bnd$x)-1)),picker))
      // bnd.2.sort<-pe(bnd.2.sort,c(rev(1:(picker[1]-1)),
      //       length(bnd.2.sort$x):picker[1]))

      // initialize a linked list, first the head
      node* bnd2 = NULL;
      //bnd2 = malloc(sizeof(struct node*));
      // ^^^ need this?

      nbnd2=nbnd-nbnd1; // number of elements

      // handle the case where start is actually the end
      if(intind[1]!=(nbnd-1)){
         start=intind[1];
      }else{
         start=1;
      }

      // insert until we hit the end 
      for(i=start;i<(nbnd-1);i++){
         curr_insert[0]=bnd[i][0];
         curr_insert[1]=bnd[i][1];
         Push(&bnd2,curr_insert);
      }

      // insert from the end back to intend[0] 
      //             vvvvvvv don't include intend[0] twice
      for(i=(nbnd-1);i>=intind[0];i--){
         curr_insert[0]=bnd[i][0];
         curr_insert[1]=bnd[i][1];
         Push(&bnd2,curr_insert);
      }



// AGAIN hope this doesn't happen at the moment   
//      // make sure we aren't adding a superfluous vertex (not both end of a side
//      // that we don't need)
//      if(length(picker)>1){
//         if(on_line(ip1,pe(bnd.2.sort,1:2))){
//            bnd.2.sort<-pe(bnd.2.sort,-1)
//         }
//         ep.1<-length(bnd.2.sort$x)
//         if(on_line(ip2,pe(bnd.2.sort,(ep.1-1):ep.1))){
//            bnd.2.sort<-pe(bnd.2.sort,-ep.1)
//         }
//      }


//////////////// This should be handled above   
//      // tackle the index going out of range, ugly but necessary without 
//      // a stack structure
//      pickend<-picker[length(picker)]
//      if(pickend==(length(bnd$x))) pickend<-1
//      pickendp<-pickend+1
//   
//      pickstart<-picker[1]
//      pickstartm<-pickstart-1
//      if(pickstart==1) pickstartm<-length(bnd$x)-1
         
//      if(on_line(ip2,pe(bnd,pickend:pickendp))&
//         on_line(ip1,pe(bnd,pickstartm:pickstart))){
//         bnd.1.sort<-list(x=rev(bnd.1.sort$x),
//                          y=rev(bnd.1.sort$y))
//         bnd.2.sort<-list(x=rev(bnd.2.sort$x),
//                          y=rev(bnd.2.sort$y))
//      }
////////////////////////////////////   

      // just going by the order in the R code here, don't need to 
      // reverse, since we used push for the insertion
      

      // p1, p1 1st intersection, some of bnd, p2 1st intersection, p2
      curr_insert[0]=ip1[0]; curr_insert[1]=ip1[1];
      AppendNode(&bnd1,curr_insert);
      AppendNode(&bnd2,curr_insert); // pushed ip1 into both

      curr_insert[0]=p1[0]; curr_insert[1]=p1[1];
      AppendNode(&bnd1,curr_insert);
      AppendNode(&bnd2,curr_insert); // pushed p1 into both

      curr_insert[0]=ip2[0]; curr_insert[1]=ip2[1];
      Push(&bnd1,curr_insert);
      Push(&bnd2,curr_insert); // append ip1 for both

      curr_insert[0]=p2[0]; curr_insert[1]=p2[1];
      Push(&bnd1,curr_insert);
      Push(&bnd2,curr_insert); // append p1 for both



// DEBUG
//printf("-----------bnd1 and bnd2-----------\n");
//printf("plot(bnd,type=\"l\")\n");
//
//printf("bnd1<-list(x=c(),y=c())\n");   
//   node* currrrent=bnd1;
//   while(currrrent!=NULL){
//      printf("bnd1$x<-c(bnd1$x,%f)\n",currrrent->data[0]);
//      printf("bnd1$y<-c(bnd1$y,%f)\n",currrrent->data[1]);
//      currrrent=currrrent->next;
//   }
//printf("lines(bnd1,lwd=2,col=\"green\")\n");
//
//printf("bnd2<-list(x=c(),y=c())\n");   
//   currrrent=bnd2;
//   while(currrrent!=NULL){
//      printf("bnd2$x<-c(bnd2$x,%f)\n",currrrent->data[0]);
//      printf("bnd2$y<-c(bnd2$y,%f)\n",currrrent->data[1]);
//      currrrent=currrrent->next;
//   }
//printf("lines(bnd2,lwd=2,col=\"red\")\n");
//printf("END-----------bnd1 and bnd2-----------END\n");
//
//printf("ip1=list(x=%f,y=%f)\n",ip1[0],ip1[1]);
//printf("ip2=list(x=%f,y=%f)\n",ip2[0],ip2[1]);
///////////////////////////////////////////////////////////




//printf("length bnd1=%f\n",hull_length(bnd1));
//printf("length bnd2=%f\n",hull_length(bnd2));


      // pick the shorter path return path
      if(hull_length(bnd1)<hull_length(bnd2)){
//printf("bnd1\n");
         return bnd1;
      }else{
//printf("bnd2\n");
         return bnd2;
      }

   } // end of error if()

}


// iterate over the points in the path:
// delete as many points as possible, making sure that the
// path is still outside
void delete_step(node** path, int nbnd, double bnd[nbnd][2])
{
   // Args:
   //  path     the current path
   //  nbnd     # elements of boundary
   //  bnd      the boundary
   // Return:
   //           revised path with dropped vertices
   
   int i,j, intbnd[nbnd-1];
   double mytrip[3][2], p1[2], p2[2], xmp[1], ymp[1];

   node* current=NULL;   // iterator
   node* prevpath=NULL;

   // needed for deletion
   node* start_ptr=NULL;
   node* end_ptr=NULL;

   ////// needed later on for the in_out() call
   double bx[nbnd];
   double by[nbnd];
   for(i=0;i<nbnd;i++){
      bx[i]=bnd[i][0];
      by[i]=bnd[i][1];
   }
   
   int in[1];

   // to handle holes, multiple boundaries
   // ignore this at the moment
   double xmin=minarr(nbnd,bx);
   double ymin=minarr(nbnd,by);
   double mina[2] = {xmin, ymin};
   double break_code=minarr(2,mina)-1;

   // convergence stop
   int conv=0;
   int conv_stop=10;

   // keep going until we don't remove any more points.
   do{

      // use prevpath to keep a copy of the previous path for comparison
      //prev.path<-path
      prevpath=CopyList(*path);

      // start point for triplet selection
      current = *path;   // iterator

      //i<-2
      //while((i+1)<=length(path$x)){
      while(current!=NULL){

         // equivalent of some ANDs in the above, but doesn't cause a memory
         // problem since the while() evaluates all of the conditions.
         if(current->next==NULL){
            break;
         }else if(current->next->next==NULL){
            break;
         }

      //for(i=2; i<(npath-1); i++){

         // create the current triplet to inspect
         //my.trip<-pe(path,c(i-1,i,i+1))
         for(i=0;i<3;i++){ 
            mytrip[i][0]=current->data[0]; mytrip[i][1]=current->data[1];
            current=current->next; 
         }



         // if we are going forward and back again, just
         // remove the point
         // in this case we remove the ith and (i+1)th entries, point (i-1)th
         // next to the (i+2)th.
         // path<-pe(path,-c(i,i+1))
         if(mytrip[0][0]==mytrip[2][0] & 
            mytrip[0][1]==mytrip[2][1]){

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
            
            // change previous
            current->prev=start_ptr; //set i+2 prev to i-1
            
         }else{

            //// This is all setup for the next if() vvvvv

            // start and end points of the trip in p1 and p2
            p1[0]=mytrip[0][0];  p1[1]=mytrip[0][1];
            p2[0]=mytrip[2][0];  p2[1]=mytrip[2][1];

            // see if the line between p1 and p2 intersects the boundary
            sp_do_intersect(p1,p2, nbnd, bnd,intbnd);

            // midpoints
            xmp[0]=(mytrip[2][0]+mytrip[0][0])/2;
            ymp[0]=(mytrip[2][1]+mytrip[0][1])/2;

printf("mp<-list(x=c(%f),y=c(%f))\n",xmp[0],ymp[0]);
printf("points(mp,pch=19)\n");


            //if(all(!sp_do_intersect(pe(my.trip,1),pe(my.trip,3),bnd))&
            //  inSide(bnd,(pe(my.trip,3)$x+pe(my.trip,1)$x)/2,
            //             (pe(my.trip,3)$y+pe(my.trip,1)$y)/2)){
				int inout_n=1;
            in[0]=1;
            in_out(&bx,&by,&break_code,&xmp,&ymp,&in, &nbnd,&inout_n);

// DEBUG
//printf("break_code: %f\n",break_code);
//printf("inout: %d\n",in[0]);
//printf("iarrsum: %d == 0 \n",iarrsum(nbnd-1,intbnd));

            // if deleting point i makes the resulting line cross the
            // the boundary then keep it in the path, else get rid of it 
            //path<-pe(path,-i)
            
            // first part asks if there are any intersections, if there are
            // none then that's okay. Second part asks if midpoints are inside.
            if( (iarrsum(nbnd-1,intbnd)==0) & in[0]){

printf("in!\n");
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
               
               // change previous
               current->prev=start_ptr; //set i+1 prev to i-1

               current=current->next; // back to i+2

            } // end if on del middle
         } // end of if del back-and-forth
      } // end iteration over path
      conv++; // increment run counter

   }while(!has_converged(prevpath,*path) &
         (conv<conv_stop) ); // end of do loop
  
   // free some memory? 
   free(current); 
   free(prevpath);
   free(start_ptr);
   free(end_ptr);
}           

// alter the path
void alter_step(node** path, int nbnd, double bnd[nbnd][2])
{
   // Args:
   //  path     the current path
   //  nbnd     # elements of boundary
   //  bnd      the boundary
   // Return:
   //           revised path with added/ammended vertices

   int i;
   double ep1[2], ep2[2], mytrip[3][2];

   node* prevpath=NULL;
   node* newpath=NULL;
   node* pathcopy=NULL;
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

      // use prevpath to keep a copy of the previous path for comparison            
      //prev.path<-path
      prevpath=CopyList(*path);

      // start point for triplet selection
      //i<-2
      //while((i+1)<=length(path$x)){
      while(current!=NULL){
         // equivalent of some ANDs in the above, but doesn't cause a memory
         // problem since the while() evaluates all of the conditions.
         if(current->next==NULL){
            break;
         }else if(current->next->next==NULL){
            break;
         }
         // for each point i, look at the line i-1 to i+1

         // copy the path for comparison
         pathcopy=CopyList(*path);

         // create the current triplet to inspect
         //my.trip<-pe(path,c(i-1,i,i+1))
         for(i=0;i<3;i++){
            mytrip[i][0]=current->data[0]; mytrip[i][1]=current->data[1];
            current=current->next;
         }
                                             
         //ep1<-pe(my.trip,1)
         //ep2<-pe(my.trip,3)
         // correct format for facing() call
         ep1[0]=mytrip[0][0]; ep1[1]=mytrip[0][1];
         ep2[0]=mytrip[2][0]; ep2[1]=mytrip[2][1];

// DEBUG
//printf("***********************debug: pre facing\n");
//printf("ep1=list(x=%f,y=%f)\n",ep1[0],ep1[1]);
//printf("ep2=list(x=%f,y=%f)\n",ep2[0],ep2[1]);
//
//printf("facing: %d\n",facing(ep1, ep2, nbnd, bnd));

         // does it go inside-outside-inside?
         if(facing(ep1, ep2, nbnd, bnd)){
// DEBUG
//printf("***********************debug: after facing\n");

            // create a new path
            newpath=make_bnd_path(ep1,ep2,nbnd,bnd);

// DEBUG
//printf("***********************debug: after make_bnd_path\n");

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
            current->next=newpath; // point i-1 next to the head of newpath
   
            // set current back to i+1
            // fast forward to the end of newpath
            while(current->next!=NULL){
               current=current->next;
            }

            // point the end of newpath to i+1
            end_ptr->prev=current; // set previous of i+1 to be the end of newpath
            current->next=end_ptr;
            
            // cut out anything silly
            delete_step(path, nbnd, bnd);

            // if there was no improvement, then ignore what we did.
            if(hull_length(pathcopy)<hull_length(*path)){
               free(path);
               node* path=pathcopy;
            }
         }
         i<-i+1;
      }
      conv++;

   } while(!has_converged(prevpath,*path)&
         (conv<conv_stop)); //end of main do
   
   // free some memory
   //free(pathcopy);
   //free(prevpath);
   //free(newpath);
   //free(end_ptr);
   //free(current);
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


