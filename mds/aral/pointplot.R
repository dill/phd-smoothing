# load the fitted model data file and look at what happened.


load("aral/aralfit.RData")

pdf(file="../thesis/mds/figs/aral-pp.pdf",width=5,height=2)
par(mfrow=c(1,2),las=1,mgp=c(1.5,0.75,0),mar=c(3,3,2,2),cex.axis=0.5,cex.lab=0.7)


plot(pred.grid,pch=".",xlab="x",ylab="y",asp=1)
plot(pred.grid.mds,pch=".",xlab="x*",ylab="y*",asp=1)

dev.off()
