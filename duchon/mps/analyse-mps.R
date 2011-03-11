# what happened in that sim?

# load the data and calculate a summary
load("mps-results.RData")
res<-colSums(result)

# create an object to plot...
res<-data.frame(count=res,id=1:length(res))


p<-ggplot(res)
p<-p+geom_bar(aes(y=count,id),binwidth=1,stat="identity")
p



