# make the diagnostic plots!

library(soap)

load("fullmod-Tweedie(1.2).RData") # load the data

# diagnostics - deviance residuals
#postscript(file="diag-deviance.eps",width=7,height=5)
pdf(file="diag-deviance.pdf",width=7)
#diagnostic(it.soap,res=5,resid.type="deviance")
model<-it.soap
res<-5
resid.type<-"deviance"
source("diagnostics.R")
dev.off()

## diagnostics - Pearson residuals
#postscript(file="diag-pearson.eps",width=6,height=6)
#diagnostic(it.soap,res=5,resid.type="pearson")
#dev.off()

