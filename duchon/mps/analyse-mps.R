# what happened in that sim?
library(ggplot2)

# load the data and calculate a summary
load("mps-results.RData")
res<-colSums(result)

# create an object to plot...
res<-data.frame(count=res,id=1:length(res),lab=mpparty)


theme_set(theme_bw())

plot.rows<-2
plot.cols<-1
Layout <- grid.layout(nrow = plot.rows, ncol = plot.cols,
                      widths = unit(rep(3,plot.rows*plot.cols),"null"),
                      heights = unit(rep(3,plot.rows*plot.cols), "null"))


subplot <- function(x, y) viewport(layout.pos.row = x,layout.pos.col = y)
vplayout <- function(...) {
     grid.newpage()
     pushViewport(viewport(layout = Layout))
}



grid.newpage()
pushViewport(viewport(layout = Layout))

p<-ggplot(res)
p<-p+geom_bar(aes(y=count,id,fill=lab),binwidth=1,stat="identity")
p<-p+scale_fill_discrete("Party",breaks=c(0,1),limits=c(0,1),h=c(10,200))
p<-p+labs(x="MP ID",y="Count")
print(p,vp=subplot(1,1))


# now for each sim how many were wrong?
res.wrong<-rowSums(result)

# create an object to plot...
res.wrong<-data.frame(count=res.wrong,id=1:dim(result)[1])
p<-ggplot(res.wrong)
p<-p+geom_bar(aes(y=count,id),binwidth=1,stat="identity")
p<-p+labs(x="Sim number",y="Number of incorrect classifications")
print(p,vp=subplot(2,1))




