# plot the mean squared difference between matrix of WADs
# minus Euclidean distance in MDS space

library(ggplot2)
library(plyr)
library(reshape)

# data!

aral<-c(1519.736,347.221,128.202,119.549,117.643,116.668,116.167,115.596,115.309,115.188)

comb<-c(3610.210,1280.189,459.946,457.16,456.07,452.436,
        450.934,450.559,449.883,449.463)

pen<-c(109.134,54.603,22.068,12.166,10.237,9.958,9.873,9.853,9.79,9.747)

dat<-cbind(dim=2:(length(aral)+1),aral,comb,pen)

dat<-melt(as.data.frame(dat),id.vars=c("dim"))

names(dat)<-c("dim","domain","eigen")


levels(dat$domain)<-c("Aral sea","Comb","Peninsulae")

theme_set(theme_bw())
p<-ggplot(dat)
p<-p+geom_line(aes(x=dim,y=log(eigen),colour=domain))
#p<-p+facet_wrap(~domain,nrow=1)

p<-p+opts(panel.grid.major=theme_blank(),
          panel.grid.minor=theme_blank(),
          legend.key = theme_blank(),
          panel.background=theme_rect())
p<-p+labs(x="Dimension",y="Log spectral norm",colour="Domain")

print(p) 

ggsave(file="eigenplot.pdf",height=5,width=7)
