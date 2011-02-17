# love for nomis

nomdat<-read.csv(file="nomis.csv")

names(nomdat)<-c("con","code","pay","payC","hours","hoursC")


nom.mapper<-match(nomdat$code,shapefile$dbf$dbf$CODE)
#> nomdat$code[1]
#[1] E14000554
#> shapefile$dbf$dbf$CODE[605]
#[1] E14000554



