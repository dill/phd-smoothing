# give some summary results from the breast cancer CV study

res<-c()

# load the Normal error data
load("npi-cv-gaussian.RData")

res<-rbind(res,cbind(dsgcv.mse.cv),cbind(dsml.mse.cv))

dsgcv.mse.cv1<-dsgcv.mse.cv
dsml.mse.cv1<-dsml.mse.cv

# load the quasi error data
load("npi-cv-quasi.RData")

dsgcv.mse.cv1<-c(dsgcv.mse.cv1,dsgcv.mse.cv)
dsml.mse.cv1<-c(dsml.mse.cv1,dsml.mse.cv)

res<-rbind(res,cbind(dsgcv.mse.cv),cbind(dsml.mse.cv),cbind(lasso.mse.cv))
rownames(res)<-NULL

## are they significantly different?
## REMEMBER the msg results have different error distns
## compare against the lasso
wilcox.res<-c(wilcox.test(lasso.mse.cv,dsgcv.mse.cv1[1:45], ,paired=T)$p.value,
              wilcox.test(lasso.mse.cv,dsgcv.mse.cv1[46:90],,paired=T)$p.value,
              wilcox.test(lasso.mse.cv,dsml.mse.cv1[1:45],  ,paired=T)$p.value,
              wilcox.test(lasso.mse.cv,dsml.mse.cv1[46:90], ,paired=T)$p.value
             )
#wilcox.med<-c(median(dsgcv.mse.cv1[1:45]-lasso.mse.cv),
#              median(dsgcv.mse.cv1[46:90]-lasso.mse.cv),
#              median(dsml.mse.cv1[1:45]-lasso.mse.cv),
#              median(dsml.mse.cv1[46:90]-lasso.mse.cv)
#             )
#ind<-wilcox.res<0.01
#wilcox.res[ind & wilcox.med > 0]<-"red"
#wilcox.res[ind & wilcox.med < 0]<-"green"
#wilcox.res[wilcox.res!="red" & wilcox.res!="green"]<-"white"

##### NOT SIGNIFICANTLY DIFFERENT!!!


mod.names<-c(rep("MDS+DS\n (GCV, normal)",45),
             rep("MDS+DS\n (ML, normal)",45),
             rep("MDS+DS\n (GCV, quasi)",45),
             rep("MDS+DS\n (ML, quasi)",45),
             rep("lasso",45))
method.names<-c(rep("normal",90),
                rep("quasi",90),
                rep("lasso",45))

res<-cbind(as.data.frame(res),mod.names,method.names)
names(res)<-c("CV","model","method")

pdf(file="breastcancer-cv-plot.pdf",width=6,height=4)

library(ggplot2)
theme_set(theme_bw())
p<-ggplot(res)
p<-p+geom_boxplot(aes(x=factor(model),y=CV))
p<-p+labs(y="LOOCV score",x="")
p<-p+opts(panel.grid.major=theme_blank(),
          panel.grid.minor=theme_blank(),
          legend.background=theme_blank(),
          legend.key=theme_blank(),
          panel.background=theme_rect())
print(p)
grid.text(label="Model", x = 0.5, y = 0.03) # HACK


dev.off()



#p<-p+geom_histogram(aes(CV))
#p<-p+facet_grid(model~.)
#p<-p+labs(y="Frequency",x="Cross-validation score")
#p<-p+opts(panel.grid.major=theme_blank(),
#          panel.grid.minor=theme_blank(),
#          legend.background=theme_blank(),
#          legend.key=theme_blank(),
#          panel.background=theme_rect())
#
#print(p)

#ggsave("breastcancer-cv-plot.pdf",width=6,height=4)
