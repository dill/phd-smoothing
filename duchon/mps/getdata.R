# looking at MPs voting records
library(mdspack)

year<-"1997" # "2005"


# first the lookup table between MP numbers, names, parties and URL
lookup<-read.table(file=paste("votematrix-",year,".txt",sep=""),skip=19,header=T,sep="\t",comment.char="")

# indicator over whether the MP is Labour
labbin<-data.frame(lab=as.numeric(lookup$party=="Lab"),mpid=lookup$mpid)

# 2005
#1302 mps
#1506 divisions
#Data Values

# 1997
#678 mps
#1273 divisions


# load in the vote matrix, rowid, date, voteno, Bill name, then MP codes
votes<-read.delim2(file=paste("votematrix-",year,".dat",sep=""),header=T,quote="",fill=FALSE)

## pull out some other bits of data
# mpid in votes
mpid<-names(votes)[5:(dim(votes)[2]-1)]
mpid<-as.numeric(sub("mpid","",mpid))

# the date of the votes
votedates<-strptime(votes$date,"%Y-%m-%d")

# take transpose of the votes so rows are MPs and columns are divisions
votemat<-t(as.matrix(votes[,5:(dim(votes)[2]-1)]))
attr(votemat,"dimnames")[[2]]<-votes$voteno # put the vote number in
# need to recode:
# missing: -9     => 0
# tellaye: 1      => 1
# aye: 2          => 1
# both: 3         => 0
# no: 4           => -1
# tellno: 5       => -1
votemat[votemat==-9]<-0
votemat[votemat==1]<-1
votemat[votemat==2]<-1
votemat[votemat==3]<-0
votemat[votemat==4]<- -1
votemat[votemat==5]<- -1

# binary var saying whether they were labour or not
mpparty<-labbin$lab[match(mpid,labbin$mpid)]
names(mpparty)<-"lab"

## this looks like a cross
#plot(cmdscale(dist(votemat),2))
## this doesn't
