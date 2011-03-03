# play with UK election data.

# useful link: http://indiemapper.com/blog/2010/05/uk-election-map-indiemapper-made/

library(shapefiles)
library(ggplot2)

# todo!

# load the Westminster constituency shapefiles...
# this takes a LONG TIME
shapefile<-read.shapefile("westminster_const_region")
# the constituency shapes are in shapefile[[1]][[1]][[1:632]]$points

# list of constituencies
constituencies<-shapefile[[1]][[1]]
# and their names
os.names<-shapefile$dbf$dbf$NAME


# voting data from
# https://spreadsheets.google.com/ccc?key=0AjwyKJQXN7nzdEVoNzRkTS1ucDc0UHpHS0VNRmVON1E&hl=en#gid=2
# based on:
# https://spreadsheets.google.com/ccc?key=tdLut_gO0qo_C0JevIxnZ2g#gid=1
votes<-read.csv("uk_election.csv",header=TRUE)
con.voteshare<-votes$CON_PER/100 # Tory vote share in (0,1)

source("renamecons.R")
# now have the vector mapper which goes FROM guardian names TO OS names
# guardian.names[20] == os.names[629] => mapper[20]==629

# for fun let's also go the other way
bmapper<-match(1:632,mapper)

###############################

list_to_coords<-function(these.coords,i,codei){
   # do something here to cut down on number of points
   fact<-0.25 # fraction of the points to keep
   n.coords<-length(these.coords$X)
   this.seq<-c(1,seq(2,n.coords-1,length=floor(n.coords*fact)),n.coords)
   these.coords<-these.coords[this.seq,]
   
   nreps<-length(these.coords$X)
   this.row<-data.frame(x=these.coords$X,
                        y=these.coords$Y,
                        name=rep(os.names[i],nreps),
                        code=rep(i,nreps),
                        group=rep(codei,nreps),
                        votes=rep(con.voteshare[bmapper[i]],nreps))
   return(this.row)
}


bigframe<-data.frame(x=NA,y=NA,name=NA,code=NA,group=NA,votes=NA)
codei<-1


# how many parts does each constituency have? ISLANDS!
#npa<-c()
#for(k in 1:length(constituencies)){
#   npa<-c(npa,constituencies[[k]]$num.parts)
#}
# how many segments does each part have?
#nseg<-c()
#for(k in 1:length(constituencies)){
#   nseg<-c(nseg,constituencies[[k]]$num.points)
#}

###############################
# some of the constituencies are __very__ big!
# let's get rid of some islands
#[2] "Banff and Buchan"                     
#[3] "St. Ives"                             
#[4] "Ross, Skye and Lochaber"              
#[5] "Caithness, Sutherland and Easter Ross"
#[6] "Argyll and Bute"                      
#[7] "Orkney and Shetland"                  
#[8] "Na h-Eileanan an Iar"                 
too.big<-c(619, 610, 613, 612, 617, 615, 616)
for(i in too.big){
   constituencies[[i]]$num.parts<-25
   constituencies[[i]]$parts<-constituencies[[i]]$parts[1:26]
   X<-constituencies[[i]]$points$X[1:constituencies[[i]]$parts[26]]
   Y<-constituencies[[i]]$points$Y[1:constituencies[[i]]$parts[26]]
   constituencies[[i]]$points<-NULL
   constituencies[[i]]$points<-data.frame(X=X,Y=Y)
}

###############################

# this is inelegant
for(i in 1:length(constituencies)){

   parts<-c(constituencies[[i]]$parts,length(constituencies[[i]]$points$X))

   # do something nice with multiple parts
   if(constituencies[[i]]$num.parts>1){
   
      parts<-c(constituencies[[i]]$parts,length(constituencies[[i]]$points$X))

      for(p.ind in 1:constituencies[[i]]$num.parts){
         these.coords<-constituencies[[i]]$points[parts[p.ind]:(parts[(p.ind+1)]-1),]

         bigframe<-rbind(bigframe,list_to_coords(these.coords,i,codei))
         codei<-codei+1
      }
   }else{
      p.ind<-length(constituencies[[i]]$points$X)
      these.coords<-constituencies[[i]]$points[1:p.ind,]

      bigframe<-rbind(bigframe,list_to_coords(these.coords,i,codei))
      codei<-codei+1
   }
}

# put the speaker and Thirsk and Malton in!!!
# 52.9 Thirsk and Malton
# 47.3 for Buckingham (if we think of Bercow as a Tory... he is)
bigframe$votes[bigframe$code==89]<-47.3/100
bigframe$votes[bigframe$code==523]<-52.9/100


bigframe<-bigframe[-1,]

# constituency polygons
con.geom<-geom_polygon(aes(x=x, y=y, fill=votes, group=group), data=bigframe)

# do the plotting
p<-ggplot()
p<-p+con.geom
p<-p+scale_fill_continuous(low="red",high="blue")

# save!
save.image("done.RData")

p
