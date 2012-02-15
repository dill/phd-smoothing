# do a raw plot of the data
library(dillhandy)
library(adehabitat)

source("fixit.R")

full<-read.csv(file="database_complete.csv")
fixdat<-fix_it_data(full)

#plotdat<-do_eda(fixdat)
dat<-fixdat
zlim<-c(0,12)
source("eda.R")
