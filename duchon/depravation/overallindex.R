# just looking at the overall index at the moment....

library(XLConnect)

# read in the depravation data
wb<-loadWorkbook("1871524.xls")
# pull in the right worksheet
imd.sheet<-readWorksheet(wb,"IMD 2010")

# put this into a frame that is actually useful

imd<-data.frame(lsoa=imd.sheet$LSOA.CODE,score=as.numeric(imd.sheet$IMD.SCORE))


# now load the lookup between the LSOA and the constituency data
wb<-loadWorkbook("Q24523\ LSOA_PCON_LU.xls")
lookup.sheet<-readWorksheet(wb,"LSOA_PCON_LU")

lookup<-data.frame(LSOA=lookup.sheet$LSOA.Code,
                   PCode=lookup.sheet$Parliamentary.Constituency.Code,
                   PName=lookup.sheet$Parliamentary.Constituency.Name)


# load the voting data
votes<-read.csv("uk_election.csv",header=TRUE)

# unload the XLConnect package, Java goes crazy if not
detach("package:XLConnect")

######## RENAME!!! #########################################

guardian.names<-as.vector(votes$CONSTITUENCY)
guardian.names<-c(guardian.names,"Thirsk and Malton")

guardian.names<-gsub("(\\w+), City of$","City of \\1",guardian.names,perl=TRUE)
# do "x, The"
guardian.names<-gsub("(\\w+), The$","The \\1",guardian.names,perl=TRUE)
guardian.names<-gsub("(\\w+) Under (\\w+)$","\\1-under-\\2",guardian.names,perl=TRUE)
guardian.names<-gsub("Hull (\\w*)","Kingston upon Hull \\1",guardian.names,perl=TRUE)


# match up the easy names
#mapper<-match(lookup$PName,guardian.names)
mapper<-match(guardian.names,lookup$PName)

# geography - switch NSEW etc
guardian.names[which(is.na(mapper))]<-gsub("(\\w+) North East$","North East \\1",guardian.names[which(is.na(mapper))],perl=TRUE)
guardian.names[which(is.na(mapper))]<-gsub("(\\w+) South East$","South East \\1",guardian.names[which(is.na(mapper))],perl=TRUE)
guardian.names[which(is.na(mapper))]<-gsub("(\\w+) North West$","North West \\1",guardian.names[which(is.na(mapper))],perl=TRUE)
guardian.names[which(is.na(mapper))]<-gsub("(\\w+) South West$","South West \\1",guardian.names[which(is.na(mapper))],perl=TRUE)
#mapper<-match(lookup$PName,guardian.names)
mapper<-match(guardian.names,lookup$PName)

guardian.names[which(is.na(mapper))]<-gsub("(\\w+) East$","East \\1",guardian.names[which(is.na(mapper))],perl=TRUE)
guardian.names[which(is.na(mapper))]<-gsub("(\\w+) West$","West \\1",guardian.names[which(is.na(mapper))],perl=TRUE)
#mapper<-match(lookup$PName,guardian.names)
mapper<-match(guardian.names,lookup$PName)

guardian.names[which(is.na(mapper))]<-gsub("(\\w+) North$","North \\1",guardian.names[which(is.na(mapper))],perl=TRUE)
guardian.names[which(is.na(mapper))]<-gsub("(\\w+) South$","South \\1",guardian.names[which(is.na(mapper))],perl=TRUE)
guardian.names[which(is.na(mapper))]<-gsub("(\\w+) Mid$","Mid \\1",guardian.names[which(is.na(mapper))],perl=TRUE)
guardian.names[which(is.na(mapper))]<-gsub("(\\w+) Central$","Central \\1",guardian.names[which(is.na(mapper))],perl=TRUE)
#mapper<-match(lookup$PName,guardian.names)
mapper<-match(guardian.names,lookup$PName)

# get rid of commas
guardian.names[which(is.na(mapper))]<-gsub("(Liverpool|Birmingham|Brighton|Sheffield|Manchester|Plymouth|Southampton|Enfield|Ealing|Lewisham) (\\w*)","\\1, \\2",guardian.names[which(is.na(mapper))],perl=TRUE)

# awkward bits...
guardian.names<-gsub("Dorset Mid and North Poole","Mid Dorset and North Poole",guardian.names,perl=TRUE)
guardian.names<-gsub("Devon West and Torridge","Torridge and West Devon",guardian.names,perl=TRUE)
guardian.names<-gsub("Suffolk Central and North Ipswich","Central Suffolk and North Ipswich",guardian.names,perl=TRUE)
guardian.names<-gsub("Basildon South and East Thurrock","South Basildon and East Thurrock",guardian.names,perl=TRUE)
guardian.names<-gsub("Worthing East and Shoreham","East Worthing and Shoreham",guardian.names,perl=TRUE)
guardian.names<-gsub("Ynys Mon","Ynys Môn",guardian.names,perl=TRUE)


mapper<-match(lookup$PName,guardian.names)


#######

# form the dataset

# the IMD data set is shorter than all of the LSOAs, so just pick the 
# values of votes, PName and PCode that correspond to those LSOAs with
# IMD data

# proportion of Tory votes
votedat<-votes$CON_PER[mapper]/100

imd.data<-data.frame(vote=votedat[match(imd$lsoa,lookup$LSOA)],
                     score=imd$score,
                     PName=lookup$PName[match(imd$lsoa,lookup$LSOA)],
                     PCode=lookup$PCode[match(imd$lsoa,lookup$LSOA)])

### plot what's there...
library(ggplot2)
#p<-ggplot(imd.data)
#p<-p+geom_point(aes(score,vote),alpha=0.2)
#p

imd.agg<-data.frame(
    score=aggregate(imd.data$score,list(PName=factor(imd.data$PName)),median)[,2],
    vote=aggregate(imd.data$vote,list(PName=factor(imd.data$PName)),median)[,2])




lab.agg<-data.frame(score=imd.agg$score,
      vote=aggregate((votes$LAB_PER[mapper]/100)[match(imd$lsoa,lookup$LSOA)],list(PName=factor(imd.data$PName)),median)[,2])

lib.agg<-data.frame(score=imd.agg$score,
      vote=aggregate((votes$LIB_PER[mapper]/100)[match(imd$lsoa,lookup$LSOA)],list(PName=factor(imd.data$PName)),median)[,2])



big.agg<-rbind(cbind(lab.agg,plotn=rep("Labour",nrow(lab.agg))),
               cbind(lib.agg,plotn=rep("Liberal Democrats",nrow(imd.agg))),
               cbind(imd.agg,plotn=rep("Conservative",nrow(imd.agg))))

p<-ggplot(big.agg)
p<-p+geom_point(aes(score,vote))
p<-p+facet_wrap(~plotn,nrow=1)
p<-p+labs(y="Proportion voting for party per constituency",
          x="Median multiple depravation score per constituency\n(Large score == more deprived)")
p

