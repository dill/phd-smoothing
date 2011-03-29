# Microarray Data from Yeoh et al. in MACAT format
# http://www.bioconductor.org/help/bioc-views/release/data/experiment/html/stjudem.html
# 
# Bioconductor version: Release (2.7)
# 
# This is a microarray data set on acute lymphoblastic leukemia, published in 2002 (Yeoh et al.Cancer Cell 2002). The experiments were conducted in the St.Jude Children's Research Hospital, Memphis, Tenessee, USA. The raw data was preprocessed by variance stabilizing normalization (Huber et al.) on probe and subsequent summarization of probe expression values into probe set expression values using median polish.
# 
# Author: Benjamin Georgi, Matthias Heinig, Sebastian Schmeier, Joern Toedling
# 
# Maintainer: Joern Toedling 

# pull the package from BioConductor
source("http://www.bioconductor.org/biocLite.R")
biocLite("stjudem")    

# load 
library(stjudem)

# format the data nicely...

# microarray data 12600 probe sets on 327 patients
ma<-t(as.matrix(stjude$expr))

# patient diagnoses
diagno<-stjude$labels


# push that into one frame
leuk<-data.frame(ma,diagno)

# remove the original data, ma and diagno
rm(stjude)
rm(ma)
rm(diagno)

# save
save.image("leuk.RData")
