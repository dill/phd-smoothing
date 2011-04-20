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

########




#######

# form the dataset

imd.data<-data.frame(vote=,
                     score=imd$score,
                     PName=lookup$PName[match(imd$lsoa,lookup$LSOA)],
                     PCode=lookup$PCode[match(imd$lsoa,lookup$LSOA)])
