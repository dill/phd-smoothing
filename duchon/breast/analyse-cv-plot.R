# give some summary results from the breast cancer CV study

res<-c()

# load the Normal error data
load("npi-cv-gaussian.RData")

res<-rbind(res,cbind(dsgcv.mse.cv),cbind(dsml.mse.cv))

# load the quasi error data
load("npi-cv-quasi.RData")

res<-rbind(res,cbind(dsgcv.mse.cv),cbind(dsml.mse.cv),cbind(lasso.mse.cv))
rownames(res)<-NULL

mod.names<-c(rep("mdsds\n (GCV, normal)",45),
             rep("mdsds\n (ML, normal)",45),
             rep("mdsds\n (GCV, quasi)",45),
             rep("mdsds\n (ML, quasi)",45),
             rep("lasso",45))
#mod.names<-c(rep("mdsds (GCV)",45),
#             rep("mdsds (ML)",45),
#             rep("mdsds (GCV)",45),
#             rep("mdsds (ML)",45),
#             rep("lasso",45))
method.names<-c(rep("normal",90),
                rep("quasi",90),
                rep("lasso",45))

res<-cbind(as.data.frame(res),mod.names,method.names)
names(res)<-c("CV","model","method")

library(ggplot2)
p<-ggplot(res)
p<-p+geom_histogram(aes(CV))
p<-p+facet_grid(model~.)
p<-p+labs(y="Frequency",x="Cross-validation score")

print(p)


