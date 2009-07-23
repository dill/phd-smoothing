// Simon's algorithm for finding the path

#include <stdio.h>
#include <stdlib.h>
#include <math.h>



//wood_path<-function(p1,p2,bnd){
//   # args:
//   #  p1, p2      the two points to find the path between
//   #  bnd         the boundary that got in the way
//   # return:
//   #  path        set of points on the path
//
//   # HACK:make sure that the points are defined from the left,
//   # this may or may not be a fix to the paths joining the wrong points
//   if(p2$x<p1$x){
//      tmp<-p1
//      p1<-p2
//      p2<-tmp
//   }
//
//### DEBUG
//#plot(bnd,type="l",asp=1)
//#points(p1)
//#points(p2)
//#cat("new point:\n")
//#cat("x=c(",p1$x,",",p2$x,")\n")
//#cat("y=c(",p1$y,",",p2$y,")\n")
//#cat("vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n")
//
//   # create the initial path:
//   # p1, p1 1st intersection, some of bnd, p2 1st intersection, p2
//   my.path<-make_bnd_path(p1,p2,bnd)
//
//   prev.path<-list(x=c(Inf),y=c(Inf))
//
//   # convergence stop
//   conv<-0
//   conv_stop<-10
//
//   # keep going until we don't remove any more points.
//   while(!has_converged(prev.path,my.path) & (conv<conv_stop)){
//      # save previous path
//      prev.path<-my.path
//
//### DEBUG
//#lines(my.path,lwd=2,col="orange")
//#text(my.path,labels=1:length(my.path$x))
//#cat("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n")
//#a<-scan()
//#cat("1\n")
//
//      # delete step, remove anything that doesn't need to be there
//      my.path<-delete_step(my.path,bnd)
//
//### DEBUG
//#lines(my.path,lwd=2,col="red")
//#a<-scan()
//#cat("2\n")
//      # add new vertices
//      my.path<-alter_step(my.path,bnd)
//
//
//      # increment convergence stopper 
//      conv<-conv+1
//
//### DEBUG
//#lines(my.path,lwd=2,col="blue")
//#a<-scan()
//   }
//
//## need this?
//#   if(conv==conv_stop) my.path<-NA
//
//### DEBUG
//#lines(my.path,lwd=2,col="green")
//#cat("########## END ############\n")
//#a<-scan()
//
//   return(my.path)
//}




// create a path between p1 and p2 using the boundary
struct node* make_bnd_path(double p1, double p2, int nbnd, double bnd)
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
   int indind[2],i,j nbnd1, nbnd2, start;

   err=first_ips(p1, p2, nbnd, bnd, ip1, ip2, intind);

   if(err>0){

      // This is quite horrible code.
      // What we do is: take the ordering that makes sense first
      // eg 1:5 not 5:1, make that set of edges, then take the difference
      // between that set and the complete set of vertices.

      // first sort the intersection indices
      twosort(intind);

      // want elements intind[0]-1 to intind[1 ](inclusive)
      //picker<-sort(c(ip1.index[1],(ip1.index[length(ip1.index)]+1)))
      //picker<-c(picker[1]:picker[2])
      //bnd.1.sort<-pe(bnd,picker)

      // initialize a linked list, first the head
      struct node* bnd1 = NULL;
      bnd1 = malloc(sizeof(struct node));

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
         Push(bnd1**,curr_insert);
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
      struct node* bnd2 = NULL;
      bnd2 = malloc(sizeof(struct node));

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
         Push(bnd2**,curr_insert);
      }

      // insert from the end back to intend[0] 
      //             vvvvvvv don't include intend[0] twice
      for(i=(nbnd-1);i>=intend[0];i--){
         curr_insert[0]=bnd[i][0];
         curr_insert[1]=bnd[i][1];
         Push(bnd2**,curr_insert);
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
      Push(**bnd1,curr_insert);
      Push(**bnd2,curr_insert); // pushed ip1 into both

      curr_insert[0]=p1[0]; curr_insert[1]=p1[1];
      Push(**bnd1,curr_insert);
      Push(**bnd2,curr_insert); // pushed p1 into both

      curr_insert[0]=ip1[0]; curr_insert[1]=ip1[1];
      AppendNode(**bnd1,curr_insert);
      AppendNode(**bnd2,curr_insert); // append ip1 for both

      curr_insert[0]=p1[0]; curr_insert[1]=p1[1];
      AppendNode(**bnd1,curr_insert);
      AppendNode(**bnd2,curr_insert); // append p1 for both


      // pick the shorter path return path
      if(hull_length(bnd1)<hull_length(bnd2)){
         return bnd1;
      }else{
         return bnd2;
      }

   } // end of error if()

}


// iterate over the points in the path:
// delete as many points as possible, making sure that the
// path is still outside
void delete_step(struct node* path, int nbnd, double bnd[nbnd][2])
{
   // Args:
   //  path     the current path
   //  nbnd     # elements of boundary
   //  bnd      the boundary
   // Return:
   //           revised path with dropped vertices
   
   int i,j, intbnd[nbnd-1];
   double mytrip[3][2], p1[2], p2[2], xmp, ymp;

   struct node* first, third;

   ////// needed later on for the in_out() call
   double bx[nbnd];
   double by[nbnd];
   for(i=0;i<(nbnd-1);i++){
      bx[i]=bnd[i][0];
      by[i]=bnd[i][1];
   }
   
   int in[1];

   // to handle holes, multiple boundaries
   // ignore this at the moment
   double break_code=1.0e6;

   // convergence stop
   int conv=0;
   int conv_stop=10;

   // keep going until we don't remove any more points.
   do{

      // use prevpath to keep a copy of the previous path for comparison            
      //prev.path<-path
      prevpath=CopyList(path);

      // start point for triplet selection
      struct node* current = path;   // iterator

      //i<-2
      //while((i+1)<=length(path$x)){
      while(current!=NULL){
      //for(i=2; i<(npath-1); i++){

         // create the current triplet to inspect
         //my.trip<-pe(path,c(i-1,i,i+1))
         for(i=0;i<3;i++){ 
            mytrip[i][0]=current->data[0]; mytrip[i][1]=current->data[1];
            current=current->next; 
         }



         // if we are going forward and back again, just
         // remove the point
         // path<-pe(path,-c(i,i+1))
         if(mytrip[0][0]==mytrip[2][0] & 
            mytrip[0][1]==mytrip[2][1]){

            // current is sitting at the 3rd entry
            // create a pointer to that
            third=current;

            // go back twice
            current=current->prev;
            current=current->prev;

            // change where next points to
            current->next=third;
            first=current;

            // go forward again (remember next has changed
            current=current->next;
            
            // change previous
            current->prev=first;
            
         // if deleting the middle point makes the resulting line cross the
         // the boundary then keep it, else get rid of it 
         }else{

            // start and end points of the trip in p1 and p2
            p1[0]=mytrip[0][0];  p1[1]=mytrip[0][1];
            p2[0]=mytrip[2][0];  p2[1]=mytrip[2][1];

            // see if the line between p1 and p2 intersects the boundary
            sp_do_intersect(p1,p2, nbnd, bnd,intbnd);

            // midpoints
            xmp=(mytrip[2][0]+mytrip[0][0])/2;
            ymp=(mytrip[2][1]+mytrip[0][1])/2;

      
            //if(all(!sp_do_intersect(pe(my.trip,1),pe(my.trip,3),bnd))&
            //  inSide(bnd,(pe(my.trip,3)$x+pe(my.trip,1)$x)/2,
            //             (pe(my.trip,3)$y+pe(my.trip,1)$y)/2)){
            in_out(bx,by,break_code,xmp,ymp,in, nbnd,1);
            tmpinout=in[0];
            if( (iarrsum(nbnd-1,intbnd)<(nbnd-1)) & tmpinout){

               //path<-pe(path,-i)
               // move everything down
               for(j=i;j<(npath-1);j++){
                  path[j][0]=path[j+1][0];
                  path[j][1]=path[j+1][1];
               }

            } // end if on del middle, 
         }
      } // end iteration over path
      conv++; // increment run counter

   }while(!has_converged(nprevpath,prevpath,npath,path)&
         (conv<conv_stop));
   // end of do loop

   free(prevpath);
   prevpath = NULL;
}           

// alter the path
//void alter_step(int npath, double *path[npath][2], int nbnd, double bnd[nbnd][2])
//{
//   // Args:
//   //  npath    # elements of current path
//   //  path     the current path
//   //  nbnd     # elements of boundary
//   //  bnd      the boundary
//   // Return:
//   //           revised path with added/ammended vertices
//
//   // use prevpath to keep a copy of the previous path for comparison            
//   //prev.path<-list(x=c(Inf),y=c(Inf))
//   double **prevpath = malloc(npath * sizeof(double));
//
//   int nprevpath=npath;
//   // move everything down
//   for(i=0;i<npath;i++){
//      prevpath[i] = malloc(2 * sizeof(double));
//      prevpath[i][0]=*path[i][0];
//      prevpath[i][1]=*path[i][1];
//   }
//
//   
//   // convergence stop
//   int conv=0;
//   int conv_stop=10;
//
//   // iterate over the points in the path:
//   // alter the path, until on two(?) consecutive runs there are
//   // no changes to the path
//   while(!has_converged(nprevpath,prevpath,npath,path)&
//         (conv<conv_stop)){
//
//      // save the previous path to compare, above
//      //prev.path<-path
//      nprevpath=npath;
//      // move everything down
//      for(i=0;i<npath;i++){
//         prevpath[i][0]=*path[i][0];
//         prevpath[i][1]=*path[i][1];
//      }
//      // maybe want some error handling here?
//      prevpath=realloc(prevpath,nprevpath);
//
//
//      // start point for triplet selection
//      //i<-2
//      //while((i+1)<=length(path$x)){
//      for(i=2; i<(npath-1); i++){
//         // for each point i, look at the line i-1 to i+1
//
//         //my.trip<-pe(path,c(i-1,i,i+1))
//         mytrip[0][0]=*path[i-1][0]; mytrip[0][1]=*path[i-1][1];
//         mytrip[1][0]=*path[i][0];   mytrip[1][1]=*path[i][1];
//         mytrip[2][0]=*path[i+1][0]; mytrip[2][1]=*path[i+1][1];
//
//         //ep1<-pe(my.trip,1)
//         //ep2<-pe(my.trip,3)
//         ep1[0]=*path[i-1][0]; ep1[1]=*path[i-1][1];
//         ep2[0]=*path[i+1][0]; ep2[1]=*path[i+1][1];
//
//
//         // does it go inside-outside-inside?
//         if(facing(ep1, ep2, nbnd, bnd)){
//
//
//
////////////////// MAKE THIS HAPPEN!
//            // create a new path
//            new.path<-make_bnd_path(ep1,ep2,bnd)
///////////////////////
//
//
//            // create new path, compare complete new path with old one, if the
//            // new one is better then keep it.
//            new.path<-delete_step(list(
//                    x=c(path$x[1:(i-1)],new.path$x,path$x[(i+1):length(path$x)]),
//                    y=c(path$y[1:(i-1)],new.path$y,path$y[(i+1):length(path$y)])),bnd)
//            my.trip<-delete_step(list(
//                    x=c(path$x[1:(i-1)],my.trip$x,path$x[(i+1):length(path$x)]),
//                    y=c(path$y[1:(i-1)],my.trip$y,path$y[(i+1):length(path$y)])),bnd)
//
//            if(hull_length(new.path)<hull_length(my.trip)){
//               path<-new.path
//            }else{
//               path<-my.trip
//            }
//         }
//         i<-i+1
//      }
//      conv++;
//   }
//   // return the path
//   return(path)
//}

// check convergence
int has_converged(int nprevpath, double prevpath[nprevpath][2], int npath, double path[npath][2])
{
   int i;
   // check if the new and old paths are the same, first check length
   // then their contents
   if(nprevpath==npath){
      for(i=0;i<npath;i++){
         if((prevpath[i][0]!=path[i][0]) & (prevpath[i][1]!=path[i][1])) return 0;
      }
   }else return 0;

   return 1;
}


