# EDA for the Italian data
#library(maps)
#library(mapdata)
#library(soap)
#library(maps)
#library(adehabitat)
#

#do_eda<-function(dat,zlim=c(0,12)){

   # bult the image plot of the raw data
#   postscript("rawplot.ps",width=9)
   pdf(file="rawplot.pdf",width=9)
   par(mfrow=c(2,3),mar=c(4.5,4.5,2,2))

   # use adehabitat to make the grid for us...


   for(year in 2003:2008){

      grid.res<-c(60,100)

      mapx<-c(fixdat$italy$map$km.e,
              fixdat$sicily$map$km.e,
              fixdat$sardinia$map$km.e)
      mapy<-c(fixdat$italy$map$km.n,
              fixdat$sicily$map$km.n,
              fixdat$sardinia$map$km.n)

      xgrid<-seq(min(mapx),max(mapx),len=grid.res[1])
      ygrid<-seq(min(mapy),max(mapy),len=grid.res[2])

      x.start<-xgrid[1:(length(xgrid)-1)]
      x.stop <-xgrid[2:length(xgrid)]
      y.start<-ygrid[1:(length(ygrid)-1)]
      y.stop <-ygrid[2:length(ygrid)]

      ne.km<-data.frame(x=c(dat$italy$dat$km.e[dat$italy$dat$year==year],
                            dat$sicily$dat$km.e[dat$sicily$dat$year==year],
                            dat$sardinia$dat$km.e[dat$sardinia$dat$year==year]),
                        y=c(dat$italy$dat$km.n[dat$italy$dat$year==year],
                            dat$sicily$dat$km.n[dat$sicily$dat$year==year],
                            dat$sardinia$dat$km.n[dat$sardinia$dat$year==year]))

      share_100<-c(dat$italy$dat$share_100[dat$italy$dat$year==year],
                   dat$sicily$dat$share_100[dat$sicily$dat$year==year],
                   dat$sardinia$dat$share_100[dat$sardinia$dat$year==year])
      

      
      # image matrix
      im.mat<-matrix(NA,grid.res[1],grid.res[2])


      # put the observations into the grid
      for(i in 1:length(x.start)){
         for(j in 1:length(y.start)){
      
            ind<-ne.km$y>=y.start[j] & ne.km$y<y.stop[j] &
                       ne.km$x>=x.start[i] & ne.km$x<x.stop[i]
      
            # take the mean of the standardised proportion of foreign
            # population, ignoring NAs
            sq<-mean(share_100[ind],na.rm=T)
      
            im.mat[i,j]<-sq
         }
      }

      im.copy<-im.mat
      im.mat[is.nan(im.mat)]<-NA
      im.copy[is.nan(im.copy)]<-0
      seqlen<-sum(rowSums(im.copy)>=0)
      
      x.start<-x.start[rowSums(im.copy)>=0]
      #y.start<-y.stop[rowSums(im.copy)>0]
      x.stop<-x.start[rowSums(im.copy)>=0]
      #y.stop<-y.stop[rowSums(im.copy)>0]
      im.copy<-im.copy[rowSums(im.copy)>=0,]

      # create the grid sequences
      xs<-xgrid
      ys<-ygrid

      # xlim and ylim   
      xlim=c(xs[1]-25,xs[length(xs)]+25)
      ylim=c(ys[1]-25,ys[length(ys)]+25)
      
      # plot with map overlay
      image(z=im.mat,x=xs,y=ys,
            col=heat.colors(100),xlab="km (e)",ylab="km (n)",
            main=paste(year),asp=1,zlim=zlim,cex.main=1.4,
            cex.lab=1.4,cex.axis=1.3,xlim=xlim,ylim=ylim)

   # find the midpoints
   mgrid<-expand.grid(x=xgrid,y=ygrid)
   mxgrid<-mgrid[1:(nrow(mgrid)-1),1]+diff(mgrid[,1])[1]/2
   mygrid<-mgrid[1:(nrow(mgrid)-1),2]+diff(mgrid[,2])[1]/2
   # are they in or out?

   inout<-inSide(list(x=fixdat$italy$map$km.e,
                      y=fixdat$italy$map$km.n),mxgrid,mygrid) |
          inSide(list(x=fixdat$sicily$map$km.e,
                      y=fixdat$sicily$map$km.n),mxgrid,mygrid) |
          inSide(list(x=fixdat$sardinia$map$km.e,
                      y=fixdat$sardinia$map$km.n),mxgrid,mygrid)


      im2<-im.mat
      nas<-is.na(im2)
      im2[nas]<-1
      im2[!inout]<-NA
      im2[!nas]<-NA
      image(z=im2,x=xs,y=ys,col="blue",add=T)


      lines(fixdat$italy$map$km.e,fixdat$italy$map$km.n,lwd=2)
      lines(fixdat$sicily$map$km.e,fixdat$sicily$map$km.n,lwd=2)
      lines(fixdat$sardinia$map$km.e,fixdat$sardinia$map$km.n,lwd=2)

   }

   dev.off()
#} 
