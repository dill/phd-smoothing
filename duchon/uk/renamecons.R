##############################
# map between the two sets of names for constituencies!

# there was no election in Thirsk and Malton because John Boakes died
# so it does not appear in the Guardian figures

guardian.names<-as.vector(votes$CONSTITUENCY)
guardian.names<-c(guardian.names,"Thirsk and Malton")

# first get rid of "Co Const" from the end of all of the
# OS names
os.names<-gsub(" Co Const","",os.names)
os.names<-gsub(" Boro Const","",os.names)
os.names<-gsub(" Burgh Const","",os.names)


# do "x, city of"
guardian.names<-gsub("(\\w+), City of$","City of \\1",guardian.names,perl=TRUE)
# do "x, The"
guardian.names<-gsub("(\\w+), The$","The \\1",guardian.names,perl=TRUE)
guardian.names<-gsub("St (\\w*)","St. \\1",guardian.names,perl=TRUE)
guardian.names<-gsub("(\\w+) Under (\\w+)$","\\1-under-\\2",guardian.names,perl=TRUE)
guardian.names<-gsub("Hull (\\w*)","Kingston upon Hull \\1",guardian.names,perl=TRUE)


# match up the easy names
mapper<-match(guardian.names,os.names)

# geography - switch NSEW etc
guardian.names[which(is.na(mapper))]<-gsub("(\\w+) North East$","North East \\1",guardian.names[which(is.na(mapper))],perl=TRUE)
guardian.names[which(is.na(mapper))]<-gsub("(\\w+) South East$","South East \\1",guardian.names[which(is.na(mapper))],perl=TRUE)
guardian.names[which(is.na(mapper))]<-gsub("(\\w+) North West$","North West \\1",guardian.names[which(is.na(mapper))],perl=TRUE)
guardian.names[which(is.na(mapper))]<-gsub("(\\w+) South West$","South West \\1",guardian.names[which(is.na(mapper))],perl=TRUE)
mapper<-match(guardian.names,os.names)
guardian.names[which(is.na(mapper))]<-gsub("(\\w+) East$","East \\1",guardian.names[which(is.na(mapper))],perl=TRUE)
guardian.names[which(is.na(mapper))]<-gsub("(\\w+) West$","West \\1",guardian.names[which(is.na(mapper))],perl=TRUE)
mapper<-match(guardian.names,os.names)
guardian.names[which(is.na(mapper))]<-gsub("(\\w+) North$","North \\1",guardian.names[which(is.na(mapper))],perl=TRUE)
guardian.names[which(is.na(mapper))]<-gsub("(\\w+) South$","South \\1",guardian.names[which(is.na(mapper))],perl=TRUE)
guardian.names[which(is.na(mapper))]<-gsub("(\\w+) Mid$","Mid \\1",guardian.names[which(is.na(mapper))],perl=TRUE)
guardian.names[which(is.na(mapper))]<-gsub("(\\w+) Central$","Central \\1",guardian.names[which(is.na(mapper))],perl=TRUE)
mapper<-match(guardian.names,os.names)

# commas
guardian.names[which(is.na(mapper))]<-gsub("(Liverpool|Birmingham|Brighton|Sheffield|Manchester|Plymouth|Southampton|Enfield|Ealing|Lewisham) (\\w*)","\\1, \\2",guardian.names[which(is.na(mapper))],perl=TRUE)

# fuck Devon
guardian.names<-gsub("Devon West and Torridge","Torridge and West Devon",guardian.names)
mapper<-match(guardian.names,os.names)

# the awkward squad
awkward<-guardian.names[which(is.na(mapper))]
awkward<-gsub("(\\w+) (West|South|Central|North|East|Mid) and (\\w+)","\\2 \\1 and \\3",awkward)
guardian.names[which(is.na(mapper))]<-awkward

# DONE!
mapper<-match(guardian.names,os.names)


