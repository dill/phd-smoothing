# analyse ALL data
library(ggplot2)

res<-c()


for(ltype in c("ALL","TEL")){

   load(paste("simtest-",ltype,".RData",sep=""))
   
   if(ltype=="ALL") ltype<-"T-ALL"
   if(ltype=="TEL") ltype<-"TEL-AML1"

   # print results
   cat(ltype,"\n")

   
   # MSE
   res<-rbind(res,cbind(dsml.mse.cv,
                        rep("MSE",length(dsml.mse.cv)),
                        rep(ltype,length(dsml.mse.cv)),
                        rep("MDS+DS\n (ML)",length(dsml.mse.cv))))
   res<-rbind(res,cbind(dsgcv.mse.cv,
                        rep("MSE",length(dsgcv.mse.cv)),
                        rep(ltype,length(dsgcv.mse.cv)),
                        rep("MDS+DS\n (GCV)",length(dsgcv.mse.cv))))
   res<-rbind(res,cbind(lasso.mse.cv,
                        rep("MSE",length(lasso.mse.cv)),
                        rep(ltype,length(lasso.mse.cv)),
                        rep("lasso",length(lasso.mse.cv))))

   # Brier
   res<-rbind(res,cbind(dsml.brier.cv,
                        rep("Brier",length(dsml.brier.cv)),
                        rep(ltype,length(dsml.brier.cv)),
                        rep("MDS+DS\n (ML)",length(dsml.brier.cv))))
   res<-rbind(res,cbind(dsgcv.brier.cv,
                        rep("Brier",length(dsgcv.brier.cv)),
                        rep(ltype,length(dsgcv.brier.cv)),
                        rep("MDS+DS\n (GCV)",length(dsgcv.brier.cv))))
   res<-rbind(res,cbind(lasso.brier.cv,
                        rep("Brier",length(lasso.brier.cv)),
                        rep(ltype,length(lasso.brier.cv)),
                        rep("lasso",length(lasso.brier.cv))))
   # print
   cat("lasso & ",   round(mean(lasso.mse.cv),2),"&",
                     round(median(lasso.mse.cv),2),"&",
                     round(sd(lasso.mse.cv)/sqrt(length(lasso.mse.cv)),2),"&",
                     round(mean(lasso.brier.cv),2),"&",
                     round(median(lasso.brier.cv),2),"&",
                     round(sd(lasso.brier.cv)/sqrt(length(lasso.brier.cv)),2),"\\\\\n")
   cat("MSG (GCV) &",round(mean(dsgcv.mse.cv),2),"&",
                     round(median(dsgcv.mse.cv),2),"&",
                     round(sd(dsgcv.mse.cv)/sqrt(length(dsgcv.mse.cv)),2),"&",
                     round(mean(dsgcv.brier.cv),2),"&",
                     round(median(dsgcv.brier.cv),2),"&",
                     round(sd(dsgcv.brier.cv)/sqrt(length(dsgcv.brier.cv)),2),"\\\\\n")
   cat("MSG (ML) & ",round(mean(dsml.mse.cv),2),"&",
                     round(median(dsml.mse.cv),2),"&",
                     round(sd(dsml.mse.cv)/sqrt(length(dsml.mse.cv)),2),"&",
                     round(mean(dsml.brier.cv),2),"&",
                     round(median(dsml.brier.cv),2),"&",
                     round(sd(dsml.brier.cv)/sqrt(length(dsml.brier.cv)),2),"\\\\\n")
}

res<-data.frame(Score=as.numeric(res[,1]),
                type=res[,2],
                sim=res[,3],
                Model=res[,4])

theme_set(theme_bw())
p<-ggplot(res)
p<-p+geom_boxplot(aes(Model,Score))
p<-p+facet_grid(sim~type,scales="free_y")

p<-p+opts(panel.grid.major=theme_blank(),
          panel.grid.minor=theme_blank(),
          legend.background=theme_blank(),
          legend.key=theme_blank(),
          panel.background=theme_rect())
print(p)

ggsave("sim-msebrier.pdf",height=7,width=7)

