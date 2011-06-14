# give some summary results from the breast cancer CV study

res<-c()

# load the Normal error data
load("npi-cv-gaussian.RData")

res<-rbind(res,cbind(dsgcv.mse.cv),cbind(dsml.mse.cv))

# load the quasi error data
load("npi-cv-quasi.RData")

res<-rbind(res,cbind(dsgcv.mse.cv),cbind(dsml.mse.cv),cbind(lasso.mse.cv))
rownames(res)<-NULL

mod.names<-c(rep("msg\n (GCV, normal)",45),
             rep("msg\n (ML, normal)",45),
             rep("msg\n (GCV, quasi)",45),
             rep("msg\n (ML, quasi)",45),
             rep("lasso",45))
method.names<-c(rep("normal",90),
                rep("quasi",90),
                rep("lasso",45))

res<-cbind(as.data.frame(res),mod.names,method.names)
names(res)<-c("CV","model","method")

library(ggplot2)
theme_set(theme_bw())
p<-ggplot(res)
p<-p+geom_histogram(aes(CV))
p<-p+facet_grid(model~.)
p<-p+labs(y="Frequency",x="Cross-validation score")
p<-p+opts(panel.grid.major=theme_blank(),
          panel.grid.minor=theme_blank(),
          legend.background=theme_blank(),
          legend.key=theme_blank(),
          panel.background=theme_rect())

print(p)

ggsave("breastcancer-cv-plot.pdf",width=6.3,height=7.75)
