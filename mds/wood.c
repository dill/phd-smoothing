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
//   these.paths<-make_bnd_path(p1,p2,bnd)
//   this.path.1<-list(x=c(p1$x,these.paths$path.1$x,p2$x),
//                     y=c(p1$y,these.paths$path.1$y,p2$y))
//   this.path.2<-list(x=c(p1$x,these.paths$path.2$x,p2$x),
//                     y=c(p1$y,these.paths$path.2$y,p2$y))
//
//   # pick the shorter path 
//   if(hull_length(this.path.1)<hull_length(this.path.2)){
//      my.path<-this.path.1
//   }else{
//      my.path<-this.path.2
//   }
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
void make_bnd_path(double p1, double p2, int nbnd, double bnd) /// MORE  STUFF!
{
   // find the first intersection between p1, p2 and the boundary
   // for each point


   // do the bounding box check first, for speed
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
 

   // probably don't need to worry about what happens if lbbindex <1 
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
   

   }else{
      // let the Robot warn us...
      printf("DANGER, WILL ROBINSON! make_bnd_path: lbbindex<1\n");
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




   bbindex<-c(1:(length(bnd$x)-1))[do_intersect(p1,p2,bnd)]
   // hold distances and intersection points temporarily
   dists<-c(); ips<-list(x=c(),y=c())

   for(i in bbindex){
      // calculate and save the intersection
      ip<-intersection_point(p1,p2,pe(bnd,c(i,i+1)))

      ips$x<-c(ips$x,ip$x)      
      ips$y<-c(ips$y,ip$y)      
      // find the distance and save
      dists<-c(dists,sqrt((p1$x-ip$x)^2+(p1$y-ip$y)^2))
   }


   // remove duplicates (ie when dist is zero)
   if(length(ips$x)>3){
      p1.ind<-which((ips$x==p1$x)&(ips$y==p1$y))
      p2.ind<-which((ips$x==p2$x)&(ips$y==p2$y))
   
      if(length(p1.ind)!=0&length(p1.ind)!=0){
         nonzero<-c(p1.ind,p2.ind)
         dists<-dists[-nonzero]
         bbindex<-bbindex[-nonzero]
         ips$x<-ips$x[-nonzero];ips$y<-ips$y[-nonzero]
      }
   }

   // the two intersection points
   ip1<-pe(ips,order(dists)[1])
   ip2<-pe(ips,order(dists,decreasing=TRUE)[1])

   // sort the intersections by their distances from p1 and p2
   ip1.index<-bbindex[order(dists)] 
   ip2.index<-bbindex[order(dists,decreasing=TRUE)] 







   // This is quite horrible code.
   // What we do is: take the ordering that makes sense first
   // eg 1:5 not 5:1, make that set of edges, then take the difference
   // between that set and the complete set of vertices.
   picker<-sort(c(ip1.index[1],(ip1.index[length(ip1.index)]+1)))
   picker<-c(picker[1]:picker[2])

   bnd.1.sort<-pe(bnd,picker)

   // make sure we aren't adding a superfluous vertex (not both end of a side
   // that we don't need)
   // ie. ip1, vertex, vertex... rather than vertex,ip1,vertex

// increased to 2, maybe this fixes things...
   if(length(picker)>2){

      if(on_line(ip1,pe(bnd.1.sort,1:2))){
         bnd.1.sort<-pe(bnd.1.sort,-1)
         picker<-picker[-1]
      }
      ep.1<-length(bnd.1.sort$x)

      if(on_line(ip2,pe(bnd.1.sort,(ep.1-1):ep.1))){
         bnd.1.sort<-pe(bnd.1.sort,-ep.1)
         picker<-picker[-ep.1]
      }
   }


   bnd.2.sort<-pe(pe(bnd,c(1:(length(bnd$x)-1))),setdiff(c(1:(length(bnd$x)-1)),picker))
   bnd.2.sort<-pe(bnd.2.sort,c(rev(1:(picker[1]-1)),length(bnd.2.sort$x):picker[1]))

   // make sure we aren't adding a superfluous vertex (not both end of a side
   // that we don't need)
   if(length(picker)>1){
      if(on_line(ip1,pe(bnd.2.sort,1:2))){
         bnd.2.sort<-pe(bnd.2.sort,-1)
      }
      ep.1<-length(bnd.2.sort$x)
      if(on_line(ip2,pe(bnd.2.sort,(ep.1-1):ep.1))){
         bnd.2.sort<-pe(bnd.2.sort,-ep.1)
      }
   }

   // tackle the index going out of range, ugly but necessary without 
   // a stack structure
   pickend<-picker[length(picker)]
   if(pickend==(length(bnd$x))) pickend<-1
   pickendp<-pickend+1

   pickstart<-picker[1]
   pickstartm<-pickstart-1
   if(pickstart==1) pickstartm<-length(bnd$x)-1
      
   if(on_line(ip2,pe(bnd,pickend:pickendp))&
      on_line(ip1,pe(bnd,pickstartm:pickstart))){
      bnd.1.sort<-list(x=rev(bnd.1.sort$x),
                       y=rev(bnd.1.sort$y))
      bnd.2.sort<-list(x=rev(bnd.2.sort$x),
                       y=rev(bnd.2.sort$y))
   }

   // p1, p1 1st intersection, some of bnd, p2 1st intersection, p2
   bnd.1.sort<-list(x=c(ip1$x,rev(bnd.1.sort$x),ip2$x),
                    y=c(ip1$y,rev(bnd.1.sort$y),ip2$y))
   bnd.2.sort<-list(x=c(ip1$x,rev(bnd.2.sort$x),ip2$x),
                    y=c(ip1$y,rev(bnd.2.sort$y),ip2$y))

   // return both paths
   return(list(path.1=bnd.1.sort,path.2=bnd.2.sort))
}


// iterate over the points in the path:
// delete as many points as possible, making sure that the
// path is still outside
void delete_step(int npath, double *path[npath][2], int nbnd, double bnd[nbnd][2])
{
   // Args:
   //  npath    # elements of current path
   //  path     the current path
   //  nbnd     # elements of boundary
   //  bnd      the boundary
   // Return:
   //           revised path with dropped vertices
   
   int i,j, intbnd[nbnd-1];
   double mytrip[3][2], p1[2], p2[2], xmp, ymp;


   ////// needed later on for the in_out() call
   double bx[nbnd];
   double by[nbnd];
   for(i=0;i<(nbnd-1);i++){
      bx[i]=bnd[i][0];
      by[i]=bnd[i][1];
   }
   
   int in[1];

   // to handle holes, multiple boundaries
   // don't handle this at the moment
   double break_code=1.0e6;
   //////

   // use prevpath to keep a copy of the previous path for comparison            
   //prev.path<-list(x=c(Inf),y=c(Inf))
   double **prevpath = malloc(npath * sizeof(double));

   int nprevpath=npath;
   // move everything down
   for(i=0;i<npath;i++){
      prevpath[i] = malloc(2 * sizeof(double));
      prevpath[i][0]=*path[i][0];
      prevpath[i][1]=*path[i][1];
   }


   // convergence stop
   int conv=0;
   int conv_stop=10;

   // keep going until we don't remove any more points.
   while(!has_converged(nprevpath,prevpath,npath,path)&
         (conv<conv_stop)){

      // save the previous path to compare, above
      //prev.path<-path
      nprevpath=npath;
      // move everything down
      for(i=0;i<npath;i++){
         prevpath[i][0]=*path[i][0];
         prevpath[i][1]=*path[i][1];
      }
      // maybe want some error handling here?
      prevpath=realloc(prevpath,nprevpath);


      // start point for triplet selection
      //i<-2
      //while((i+1)<=length(path$x)){
      for(i=2; i<(npath-1); i++){
         // create the current triplet to inspect
         //my.trip<-pe(path,c(i-1,i,i+1))
         mytrip[0][0]=*path[i-1][0]; mytrip[0][1]=*path[i-1][1];
         mytrip[1][0]=*path[i][0]; mytrip[1][1]=*path[i][1];
         mytrip[2][0]=*path[i+1][0]; mytrip[2][1]=*path[i+1][1];


         // if we are going forward and back again, just
         // remove the point
         // path<-pe(path,-c(i,i+1))
         if(mytrip[0][0]==mytrip[2][0] & 
            mytrip[0][1]==mytrip[2][1]){

            // move everything down
            for(j=i;j<(npath-2);j++){
               path[j][0]=path[j+2][0];
               path[j][1]=path[j+2][1];
            }

            // maybe want some error handling here?
            path=realloc(path,npath-2);
        
         // if deleting the middle point makes the resulting line cross the
         // the boundary then keep it, else get rid of it 
         }else{

            p1[0]=mytrip[0][0];p1[1]=mytrip[0][1];
            p2[0]=mytrip[2][0];p2[1]=mytrip[2][1];

            sp_do_intersect(p1,p2, nbnd, bnd,intbnd);


            // midpoints
            xmp=(mytrip[2][0]+mytrip[0][0])/2;
            ymp=(mytrip[2][1]+mytrip[0][1])/2;

      
            //if(all(!sp_do_intersect(pe(my.trip,1),pe(my.trip,3),bnd))&
            //  inSide(bnd,(pe(my.trip,3)$x+pe(my.trip,1)$x)/2,
            //             (pe(my.trip,3)$y+pe(my.trip,1)$y)/2)){
            if(iarrsum(nbnd-1,intbnd)<(nbnd-1) &
               in_out(bx,by,break_code,xmp,ymp,in, nbnd,1)){

            //path<-pe(path,-i)
            // move everything down
            for(j=i;j<(npath-1);j++){
               path[j][0]=path[j+1][0];
               path[j][1]=path[j+1][1];
            }

            // maybe want some error handling here?
            path=realloc(path,npath-1);

            }
         }
      }
      conv++;
   }
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
//            these.paths<-make_bnd_path(ep1,ep2,bnd)
///////////////////////
//
//
//
//
////            # make sure that the new paths are as short as possible
////### IS THIS NEEDED?
////#            these.paths$path.1<-delete_step(these.paths$path.1,bnd)
////#            these.paths$path.2<-delete_step(these.paths$path.2,bnd)
//
//            // pick the shorter path 
//            if(hull_length(these.paths$path.1)<hull_length(these.paths$path.2)){
//               new.path<-these.paths$path.1
//            }else{
//               new.path<-these.paths$path.2
//            }
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


