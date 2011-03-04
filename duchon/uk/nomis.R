# love for nomis

library(ggplot2)
load("done.RData")


nomdat<-read.csv(file="nomis.csv")

names(nomdat)<-c("con","code","pay","payC","hours","hoursC")


nom.mapper<-match(nomdat$code,shapefile$dbf$dbf$CODE)
#> nomdat$code[1]
#[1] E14000554
#> shapefile$dbf$dbf$CODE[605]
#[1] E14000554

# other way !!
nom.mapper<-match(shapefile$dbf$dbf$CODE,nomdat$code)

pay.column<-as.numeric(as.character(nomdat$pay[nom.mapper]))
# this will warn that it's put NAs in -- that's GOOD!

na.id<-which(is.na(pay.column))

pay.column<-pay.column[which(!is.na(pay.column))]

these.votes<-con.voteshare[mapper]
these.votes<-these.votes[-mapper[na.id]]

# what's going on here? - this is a mess
vote.dat<-data.frame(votes=these.votes,pay=pay.column)
#p<-ggplot(vote.dat)
#p<-p+geom_point(aes(x=pay,y=votes))
#p

# what about looking at regions?
region.lookup<-read.csv("regiontoconst.csv")

# do some matching again....
# just missing some dots...
region.lookup<-data.frame(constituency=as.character(region.lookup$PCON10NM),
                          region=as.character(region.lookup$GOR09NM))

region.lookup$constituency<-gsub("St ","St. ",region.lookup$constituency)

region.mapper<-match(region.lookup$constituency,os.names)
bregion.mapper<-match(os.names,region.lookup$constituency)


region<-as.character(region.lookup$region[bregion.mapper])
region<-region[-mapper[na.id]]
region[is.na(region)]<-"Scotland/Wales"

vote.dat<-data.frame(votes=these.votes,
                     pay=pay.column,
                     region=region)

p<-ggplot(vote.dat)

p<-p+geom_point(aes(x=pay,y=votes,colour=con))

p<-p+stat_smooth(aes(x=pay,y=votes),se=FALSE)

p<-p+facet_wrap(~region,ncol=3)

p



