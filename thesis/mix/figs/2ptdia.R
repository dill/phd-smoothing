# generate a diagram of 2-point mixtures...


library(mmds)

these.pars<-c(log(0.8),log(0.2),0.5)
width=1


pdf(file="2ptdia.pdf",width=4,height=4)

plot.ds.mixture(list(pars=these.pars,width=width,mix.terms=2),style="comp",xlim=c(0,1))

dev.off()
