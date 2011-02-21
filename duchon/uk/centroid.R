# play with UK election data.
# show a map with constituency names on it

library(shapefiles)
library(ggplot2)

# load the Westminster constituency shapefiles...
# this takes a LONG TIME
shapefile<-read.shapefile("westminster_const_region")
# the constituency shapes are in shapefile[[1]][[1]][[1:632]]$points

# list of constituencies
constituencies<-shapefile[[1]][[1]]
# and their names
os.names<-shapefile$dbf$dbf$NAME

# voting data
votes<-read.csv("uk_election.csv",header=TRUE)
con.voteshare<-votes$CON_PER/100 # Tory vote share in (0,1)

source("renamecons.R")
# now have the vector mapper which goes FROM guardian names TO OS names
# guardian.names[20] == os.names[629] => mapper[20]==629

# for fun let's also go the other way
bmapper<-match(1:632,mapper)

nameframe<-data.frame(x=NA,y=NA,name=NA,code=NA,votes=NA)

some.cons<-1:length(constituencies)

# pull out the centroids of the bounding boxes
for(i in some.cons){
   bbox<-constituencies[[i]]$box
   cent<-c(bbox[3]-bbox[1],bbox[4]-bbox[2])
   nameframe<-rbind(nameframe,data.frame(x=bbox[1],
                                         y=bbox[2],
                                         name=os.names[i],
                                         code=i,
                                         votes=con.voteshare[bmapper[i]]))
}

# put the speaker and Thirsk and Malton in!!!
# 52.9 Thirsk and Malton
# 47.3 for Buckingham (if we think of Bercow as a Tory... he is)

nameframe<-nameframe[-1,]

# plot the names at the centroids
names.text<-geom_text(aes(x=x,y=y,label=name,colour=votes),data=nameframe,size=1)

# do the plotting
p<-ggplot()
p<-p+names.text
p<-p+scale_fill_continuous(low="red",high="blue")
print(p)

