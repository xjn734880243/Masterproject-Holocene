###########################################Present GLM and observed distribution#################################
####3676glm
tem<-read.csv("D://Project/resolution0.5/changesolar/8000/difference1.csv")
library(raster)
library(sf)
europe_raster<-raster(xmn=-10,xmx=50,ymn=35,ymx=75,res=0.5,crs='+init=epsg:4326')
temwgs<-st_as_sf(tem, coords=c('lon', 'lat'), crs=4326)
tem_raster<-rasterize(as(temwgs, 'Spatial'),europe_raster)

library(dplyr)
totalH<-filter(tem,relationH==1)
totalHwgs<-st_as_sf(totalH, coords=c('lon', 'lat'), crs=4326)
totalH_raster<-rasterize(as(totalHwgs, 'Spatial'),europe_raster)
totalP<-filter(tem,relationP==1)
totalPwgs<-st_as_sf(totalP, coords=c('lon', 'lat'), crs=4326)
totalP_raster<-rasterize(as(totalPwgs, 'Spatial'),europe_raster)

ne_110 <- st_read('D://Project/ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp')
europe <- st_crop(ne_110, extent(-10,50,35,75))

###glm (winter+summer)
tem<-read.csv("D://Project/resolution0.5/changesolar/8000/difference1.csv")
glm_model<-glm(relationP~lakemaxP+I(lakemaxP^2)+lakeminP+I(lakeminP^2),dat=tem,family=binomial(link = "logit"))
tem$predictedP<-predict(glm_model,tem,type='response')
library(visreg)
pdf(file="D://Project/Holocene/glm.pdf")
visreg(glm_model,'lakemaxP',scale='response',cex.lab=1.4,xlab='Lake summer temperature(¡ãC)',ylab='Predicted probability')
dev.off()
library(data.table)
fwrite(x=tem,file="D://Project/Holocene/predictwinter.csv")

##spec_sens
all<-read.csv("D://Project/Holocene/predictwinter.csv")
predict_absence<-filter(all,relationP=='0')
predict_presence<-filter(all,relationP=='1')
library(evaluate)
library(dismo)
glm_eval1<-evaluate(predict_presence,predict_absence,model=glm_model)
max_kappa1 <- plogis(threshold(glm_eval1, stat='spec_sens'))

predictwgs<-st_as_sf(all, coords=c('lon', 'lat'), crs=4326)
predict_raster<-rasterize(as(predictwgs, 'Spatial'),europe_raster)

glm_map1 <- predict_raster >= max_kappa1
pdf(file="D://Project/Holocene/present3676.pdf")
par(mfrow=c(1,1),mar=c(3,3,3,3))
plot(glm_map1$predictedP, legend=FALSE, col=c('grey','goldenrod1'))
plot(europe$geometry,add=TRUE)
plot(totalPwgs,pch=4,cex=0.5,color='black',add=TRUE)
dev.off()


####2483glm
tem<-read.csv("D://Project/resolution0.5/origin3676/2483.csv")
europe_raster<-raster(xmn=-10,xmx=50,ymn=35,ymx=75,res=0.5,crs='+init=epsg:4326')
glm_model<-glm(relationP~lakemaxP+I(lakemaxP^2)+lakeminP+I(lakeminP^2),dat=tem,family=binomial(link = "logit"))
tem$predictedP<-predict(glm_model,tem,type='response')
library(data.table)
fwrite(x=tem,file="D://Project/Holocene/predictwinter.csv")

##spec_sens
all<-read.csv("D://Project/Holocene/predictwinter.csv")
predict_absence<-filter(all,relationP=='0')
predict_presence<-filter(all,relationP=='1')
library(evaluate)
library(dismo)
glm_eval1<-evaluate(predict_presence,predict_absence,model=glm_model)
max_kappa1 <- plogis(threshold(glm_eval1, stat='spec_sens'))

have<-filter(all,predictedP>=max_kappa1)
nothave<-filter(all,predictedP<max_kappa1)
have$relationnew<-1
nothave$relationnew<-0
point<-rbind(have,nothave)
pointwgs<-st_as_sf(point, coords=c('lon', 'lat'), crs=4326)
point_raster<-rasterize(as(pointwgs, 'Spatial'),europe_raster)
pdf(file="D://Project/Holocene/present2483.pdf")
par(mfrow=c(1,1),mar=c(3,3,3,3))
plot(point_raster$relationnew,col=c('grey','goldenrod1'),legend=FALSE)
plot(europe$geometry,add=TRUE)
plot(totalPwgs,pch=4,cex=0.5,color='black',add=TRUE)
dev.off()
##########################################change solar radiation only########################################
###observed distribution with lake temperature
tem<-read.csv("D://Project/resolution0.5/changesolar/8000/difference1.csv")
library(raster)
library(sf)
europe_raster<-raster(xmn=-10,xmx=50,ymn=35,ymx=75,res=0.5,crs='+init=epsg:4326')
temwgs<-st_as_sf(tem, coords=c('lon', 'lat'), crs=4326)
tem_raster<-rasterize(as(temwgs, 'Spatial'),europe_raster)

library(dplyr)
totalH<-filter(tem,relationH==1)
totalHwgs<-st_as_sf(totalH, coords=c('lon', 'lat'), crs=4326)
totalH_raster<-rasterize(as(totalHwgs, 'Spatial'),europe_raster)
totalP<-filter(tem,relationP==1)
totalPwgs<-st_as_sf(totalP, coords=c('lon', 'lat'), crs=4326)
totalP_raster<-rasterize(as(totalPwgs, 'Spatial'),europe_raster)

ne_110 <- st_read('D://Project/ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp')
europe <- st_crop(ne_110, extent(-10,50,35,75))



###glm (winter+summer)
tem<-read.csv("D://Project/resolution0.5/changesolar/8000/difference1.csv")
glm_model<-glm(relationP~lakemaxP+I(lakemaxP^2)+lakeminP+I(lakeminP^2),dat=tem,family=binomial(link = "logit"))
asd<-data.frame(tem$lakemax)
colnames(asd) <-c("lakemaxP")
asd<-cbind(asd,tem$lon,tem$lat,tem$relationH,tem$relationP,tem$lakemin)
colnames(asd) <-c("lakemaxP","lon","lat","relationH","relationP","lakeminP")
asd$predictedH<-predict(glm_model,asd,type='response')
library(data.table)
fwrite(x=asd,file="D://Project/Holocene/predictwinter.csv")

##spec_sens
all<-read.csv("D://Project/Holocene/predictwinter.csv")
predict_absence<-filter(all,relationH=='0')
predict_presence<-filter(all,relationH=='1')
library(evaluate)
library(dismo)
glm_eval1<-evaluate(predict_presence,predict_absence,model=glm_model)
max_kappa1 <- plogis(threshold(glm_eval1, stat='spec_sens'))

predictwgs<-st_as_sf(all, coords=c('lon', 'lat'), crs=4326)
predict_raster<-rasterize(as(predictwgs, 'Spatial'),europe_raster)

glm_map1 <- predict_raster >= max_kappa1
pdf(file="D://Project/Holocene/8000solar.pdf")
par(mfrow=c(1,1),mar=c(3,3,3,3))
plot(glm_map1$predictedH, legend=FALSE, col=c('grey','aquamarine2'))
plot(europe$geometry,add=TRUE)
plot(totalHwgs,pch=4,cex=0.5,color='black',add=TRUE)
dev.off()

###lake difference and solar difference
tem<-read.csv("D://Project/resolution0.5/changesolar/8000/difference1.csv")
europe_raster<-raster(xmn=-10,xmx=50,ymn=35,ymx=75,res=0.5,crs='+init=epsg:4326')
temwgs<-st_as_sf(tem, coords=c('lon', 'lat'), crs=4326)
tem_raster<-rasterize(as(temwgs, 'Spatial'),europe_raster)
pdf(file="D://Project/Holocene/lake summer difference8000.pdf")
par(mfrow=c(1,1),mar=c(3,3,3,3))
plot(tem_raster$lakemaxdiff2)
plot(europe$geometry,add=TRUE)
dev.off()
pdf(file="D://Project/Holocene/solar difference8000.pdf")
par(mfrow=c(1,1),mar=c(3,3,3,3))
plot(tem_raster$solardiff2,col=hcl.colors(20,palette='inferno'))
plot(europe$geometry,add=TRUE)
dev.off()

tem<-read.csv("D://Project/resolution0.5/changesolar/difference.csv")
temwgs<-st_as_sf(tem, coords=c('lon', 'lat'), crs=4326)
tem_raster<-rasterize(as(temwgs, 'Spatial'),europe_raster)
pdf(file="D://Project/Holocene/lakediff (8vs9).pdf")
par(mfrow=c(1,1),mar=c(3,3,3,3))
plot(tem_raster$lakemaxdiff)
plot(europe$geometry,add=TRUE)
dev.off()
pdf(file="D://Project/Holocene/solardiff (8vs9).pdf")
par(mfrow=c(1,1),mar=c(3,3,3,3))
plot(tem_raster$solardiff,col=hcl.colors(20,palette='inferno'))
plot(europe$geometry,add=TRUE)
dev.off()

##########################################change temperature only##################################
###observed distribution with lake temperature
tem<-read.csv("D://Project/resolution0.5/changeT/8000/difference.csv")
library(raster)
library(sf)
europe_raster<-raster(xmn=-10,xmx=50,ymn=35,ymx=75,res=0.5,crs='+init=epsg:4326')
temwgs<-st_as_sf(tem, coords=c('lon', 'lat'), crs=4326)
tem_raster<-rasterize(as(temwgs, 'Spatial'),europe_raster)

ne_110 <- st_read('D://Project/ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp')
europe <- st_crop(ne_110, extent(-10,50,35,75))

pdf(file="D://Project/Holocene/lakemindiff8000.pdf")
par(mfrow=c(1,1), mar=c(3,3,3,3))
plot(tem_raster$lakemindiff,axes=TRUE)
plot(europe$geometry,add=TRUE)
dev.off()

pdf(file="D://Project/Holocene/airmin8000.pdf")
par(mfrow=c(1,1), mar=c(3,3,3,3))
plot(tem_raster$airmin,axes=TRUE,col=hcl.colors(20,palette='Spectral'))
plot(europe$geometry,add=TRUE)
dev.off()

pdf(file="D://Project/Holocene/airmax8000.pdf")
par(mfrow=c(1,1), mar=c(3,3,3,3))
plot(tem_raster$airmax,axes=TRUE,col=hcl.colors(20,palette='Spectral'))
plot(europe$geometry,add=TRUE)
dev.off()

pdf(file="D://Project/Holocene/lakemax8000.pdf")
par(mfrow=c(1,1), mar=c(3,3,3,3))
plot(tem_raster$lakemax,axes=TRUE)
plot(europe$geometry,add=TRUE)
dev.off()

pdf(file="D://Project/Holocene/lakemin8000.pdf")
par(mfrow=c(1,1), mar=c(3,3,3,3))
plot(tem_raster$lakemin,axes=TRUE)
plot(europe$geometry,add=TRUE)
dev.off()

pdf(file="D://Project/Holocene/airmaxdiff8000.pdf")
par(mfrow=c(1,1), mar=c(3,3,3,3))
plot(tem_raster$airmaxdiff,axes=TRUE,col=hcl.colors(20,palette='Spectral'))
plot(europe$geometry,add=TRUE)
dev.off()


pdf(file="D://Project/Holocene/airmindiff8000.pdf")
par(mfrow=c(1,1), mar=c(3,3,3,3))
plot(tem_raster$airmindiff,axes=TRUE,col=hcl.colors(20,palette='Spectral'))
plot(europe$geometry,add=TRUE)
dev.off()

pdf(file="D://Project/Holocene/lakemaxdiff8000.pdf")
par(mfrow=c(1,1), mar=c(3,3,3,3))
plot(tem_raster$lakemaxdiff,axes=TRUE)
plot(europe$geometry,add=TRUE)
dev.off()

###glm (winter+summer)
tem<-read.csv("D://Project/resolution0.5/changeT/8000/difference.csv")
glm_model<-glm(relationP~lakemaxP+I(lakemaxP^2)+lakeminP+I(lakeminP^2),dat=tem,family=binomial(link = "logit"))
asd<-data.frame(tem$lakemax)
colnames(asd) <-c("lakemaxP")
asd<-cbind(asd,tem$lon,tem$lat,tem$relationH,tem$relationP,tem$lakemin)
colnames(asd) <-c("lakemaxP","lon","lat","relationH","relationP","lakeminP")
asd$predictedH<-predict(glm_model,asd,type='response')
library(data.table)
fwrite(x=asd,file="D://Project/Holocene/predictwinter.csv")

##spec_sens
all<-read.csv("D://Project/Holocene/predictwinter.csv")
predict_absence<-filter(all,relationH=='0')
predict_presence<-filter(all,relationH=='1')
library(evaluate)
library(dismo)
glm_eval1<-evaluate(predict_presence,predict_absence,model=glm_model)
max_kappa1 <- plogis(threshold(glm_eval1, stat='spec_sens'))

predictwgs<-st_as_sf(all, coords=c('lon', 'lat'), crs=4326)
predict_raster<-rasterize(as(predictwgs, 'Spatial'),europe_raster)

glm_map1 <- predict_raster >= max_kappa1
pdf(file="D://Project/Holocene/8000T.pdf")
par(mfrow=c(1,1),mar=c(3,3,3,3))
plot(glm_map1$predictedH, legend=FALSE, col=c('grey','aquamarine2'))
plot(europe$geometry,add=TRUE)
plot(totalHwgs,pch=4,cex=0.5,color='black',add=TRUE)
dev.off()

tem<-read.csv("D://Project/resolution0.5/changeT/difference1.csv")
temwgs<-st_as_sf(tem, coords=c('lon', 'lat'), crs=4326)
tem_raster<-rasterize(as(temwgs, 'Spatial'),europe_raster)

pdf(file="D://Project/Holocene/lakemindiff(8vs9).pdf")
par(mfrow=c(1,1),mar=c(3,3,3,3))
plot(tem_raster$lakemindiff)
plot(europe$geometry,add=TRUE)
dev.off()



pdf(file="D://Project/Holocene/lakemaxdiff(8vs9).pdf")
par(mfrow=c(1,1),mar=c(3,3,3,3))
plot(tem_raster$lakemaxdiff)
plot(europe$geometry,add=TRUE)
dev.off()

pdf(file="D://Project/Holocene/lakemindiff(8vs9).pdf")
par(mfrow=c(1,1),mar=c(3,3,3,3))
plot(tem_raster$lakemindiff)
plot(europe$geometry,add=TRUE)
dev.off()

pdf(file="D://Project/Holocene/airmaxdiff(8vs9).pdf")
par(mfrow=c(1,1),mar=c(3,3,3,3))
plot(tem_raster$airmaxdiff,col=hcl.colors(20,palette='Spectral'))
plot(europe$geometry,add=TRUE)
dev.off()

pdf(file="D://Project/Holocene/airmindiff(8vs9).pdf")
par(mfrow=c(1,1),mar=c(3,3,3,3))
plot(tem_raster$airmindiff,col=hcl.colors(20,palette='Spectral'))
plot(europe$geometry,add=TRUE)
dev.off()

##################################change solar and temperature######################################
###observed distribution with lake temperature
tem<-read.csv("D://Project/resolution0.5/changesolarT/8000/difference.csv")
library(raster)
library(sf)
europe_raster<-raster(xmn=-10,xmx=50,ymn=35,ymx=75,res=0.5,crs='+init=epsg:4326')
temwgs<-st_as_sf(tem, coords=c('lon', 'lat'), crs=4326)
tem_raster<-rasterize(as(temwgs, 'Spatial'),europe_raster)

ne_110 <- st_read('D://Project/ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp')
europe <- st_crop(ne_110, extent(-10,50,35,75))
pdf(file="D://Project/Holocene/lakemaxdiff8000.pdf")
par(mfrow=c(1,1), mar=c(3,3,3,3))
plot(tem_raster$lakemaxdiff,axes=TRUE)
plot(europe$geometry,add=TRUE)
dev.off()


###glm (winter+summer)
tem<-read.csv("D://Project/resolution0.5/changesolarT/8000/difference.csv")
glm_model<-glm(relationP~lakemaxP+I(lakemaxP^2)+lakeminP+I(lakeminP^2),dat=tem,family=binomial(link = "logit"))
asd<-data.frame(tem$lakemax)
colnames(asd) <-c("lakemaxP")
asd<-cbind(asd,tem$lon,tem$lat,tem$relationH,tem$relationP,tem$lakemin)
colnames(asd) <-c("lakemaxP","lon","lat","relationH","relationP","lakeminP")
asd$predictedH<-predict(glm_model,asd,type='response')
library(data.table)
fwrite(x=asd,file="D://Project/Holocene/predictwinter.csv")

##spec_sens
all<-read.csv("D://Project/Holocene/predictwinter.csv")
predict_absence<-filter(all,relationH=='0')
predict_presence<-filter(all,relationH=='1')
library(evaluate)
library(dismo)
glm_eval1<-evaluate(predict_presence,predict_absence,model=glm_model)
max_kappa1 <- plogis(threshold(glm_eval1, stat='spec_sens'))

predictwgs<-st_as_sf(all, coords=c('lon', 'lat'), crs=4326)
predict_raster<-rasterize(as(predictwgs, 'Spatial'),europe_raster)

glm_map1 <- predict_raster >= max_kappa1
pdf(file="D://Project/Holocene/8000solarT.pdf")
par(mfrow=c(1,1),mar=c(3,3,3,3))
plot(glm_map1$predictedH, legend=FALSE, col=c('grey','aquamarine2'))
plot(europe$geometry,add=TRUE)
plot(totalHwgs,pch=4,cex=0.5,color='black',add=TRUE)
dev.off()

tem<-read.csv("D://Project/resolution0.5/changesolarT/difference1.csv")
temwgs<-st_as_sf(tem, coords=c('lon', 'lat'), crs=4326)
tem_raster<-rasterize(as(temwgs, 'Spatial'),europe_raster)
pdf(file="D://Project/Holocene/lakemaxdiff(8vs9).pdf")
par(mfrow=c(1,1), mar=c(3,3,3,3))
plot(tem_raster$lakemaxdiff,axes=TRUE)
plot(europe$geometry,add=TRUE)
dev.off()

pdf(file="D://Project/Holocene/lakemindiff(8vs9).pdf")
par(mfrow=c(1,1),mar=c(3,3,3,3))
plot(tem_raster$lakemindiff)
plot(europe$geometry,add=TRUE)
dev.off()

#############################difference between present and holocene###########################
####Present and solar
tem<-read.csv("D://Project/resolution0.5/changesolar/8000/difference1.csv")
glm_model<-glm(relationP~lakemaxP+I(lakemaxP^2)+lakeminP+I(lakeminP^2),dat=tem,family=binomial(link = "logit"))
allH<-read.csv("D://Project/Holocene/predictwinterS8000.csv",header=TRUE)
library(dplyr)
predict_absence1<-filter(allH,relationH=='0')
predict_presence1<-filter(allH,relationH=='1')
library(evaluate)
library(dismo)
glm_eval1<-evaluate(predict_presence1,predict_absence1,model=glm_model)
max_kappa1 <- plogis(threshold(glm_eval1, stat='spec_sens'))

allP<-read.csv("D://Project/Holocene/predictwinter3676.csv",header=TRUE)
predict_absence2<-filter(allP,relationP=='0')
predict_presence2<-filter(allP,relationP=='1')
library(evaluate)
library(dismo)
glm_eval2<-evaluate(predict_presence2,predict_absence2,model=glm_model)
max_kappa2 <- plogis(threshold(glm_eval2, stat='spec_sens'))

haveH<-filter(allH,predictedH>=max_kappa1)
haveP<-filter(allP,predictedP>=max_kappa2)
nothaveH<-filter(allH,predictedH<max_kappa1)
nothaveHnew<-cbind(nothaveH$lon,nothaveH$lat)
nothaveHnew<-data.frame(nothaveHnew)
nothaveHnew$relationnew<-0

Hwgs<-st_as_sf(haveH, coords=c('lon', 'lat'), crs=4326)
H_raster<-rasterize(as(Hwgs, 'Spatial'),europe_raster)
Pwgs<-st_as_sf(haveP, coords=c('lon', 'lat'), crs=4326)
P_raster<-rasterize(as(Pwgs, 'Spatial'),europe_raster)
relation<-extract(P_raster,as(Hwgs,'Spatial'))
relation<-cbind(Hwgs,relation)
relation[is.na(relation)]<-0
relation1<-filter(relation,lakemax==0)
fwrite(x=relation1,file="D://Project/Holocene/relation1.csv")
relation1<-read.csv("D://Project/Holocene/relation1.csv")
library(tidyr)
relation1<-relation1%>%separate(geometry,c("lon","lat"),"[|]")
detach("package:tidyr")
relation1new<-cbind(relation1$lon,relation1$lat)
relation1new<-data.frame(relation1new)
relation1new$relationnew<-2
relation2<-filter(relation,lakemax!=0)
fwrite(x=relation2,file="D://Project/Holocene/relation2.csv")
relation2<-read.csv("D://Project/Holocene/relation2.csv")
library(tidyr)
relation2<-relation2%>%separate(geometry,c("lon","lat"),"[|]")
detach("package:tidyr")
relation2new<-cbind(relation2$lon,relation2$lat)
relation2new<-data.frame(relation2new)
relation2new$relationnew<-1
relationfinal<-rbind(relation1new,relation2new,nothaveHnew)
colnames(relationfinal)<-c('lon','lat','relationnew')
fwrite(x=relationfinal,file="D://Project/Holocene/relationfinal.csv")

pointwgs<-st_as_sf(relationfinal, coords=c('lon', 'lat'), crs=4326)
point_raster<-rasterize(as(pointwgs, 'Spatial'),europe_raster)

pdf(file="D://Project/Holocene/solar8000.pdf")
par(mfrow=c(1,1),mar=c(3,3,3,3))
plot(point_raster$relationnew,col=c('grey','goldenrod1','red'),legend=FALSE)
plot(europe$geometry,add=TRUE)
dev.off()

####Present and temperature
tem<-read.csv("D://Project/resolution0.5/changeT/8000/difference.csv")
glm_model<-glm(relationP~lakemaxP+I(lakemaxP^2)+lakeminP+I(lakeminP^2),dat=tem,family=binomial(link = "logit"))

allH<-read.csv("D://Project/Holocene/predictwinterT8000.csv",header=TRUE)
library(dplyr)
predict_absence1<-filter(allH,relationH=='0')
predict_presence1<-filter(allH,relationH=='1')
library(evaluate)
library(dismo)
glm_eval1<-evaluate(predict_presence1,predict_absence1,model=glm_model)
max_kappa1 <- plogis(threshold(glm_eval1, stat='spec_sens'))

allP<-read.csv("D://Project/Holocene/predictwinter2483.csv",header=TRUE)
predict_absence2<-filter(allP,relationP=='0')
predict_presence2<-filter(allP,relationP=='1')
library(evaluate)
library(dismo)
glm_eval2<-evaluate(predict_presence2,predict_absence2,model=glm_model)
max_kappa2 <- plogis(threshold(glm_eval2, stat='spec_sens'))

haveH<-filter(allH,predictedH>=max_kappa1)
haveP<-filter(allP,predictedP>=max_kappa2)
nothaveH<-filter(allH,predictedH<max_kappa1)
nothaveHnew<-cbind(nothaveH$lon,nothaveH$lat)
nothaveHnew<-data.frame(nothaveHnew)
nothaveHnew$relationnew<-0

Hwgs<-st_as_sf(haveH, coords=c('lon', 'lat'), crs=4326)
H_raster<-rasterize(as(Hwgs, 'Spatial'),europe_raster)
Pwgs<-st_as_sf(haveP, coords=c('lon', 'lat'), crs=4326)
P_raster<-rasterize(as(Pwgs, 'Spatial'),europe_raster)
relation<-extract(P_raster,as(Hwgs,'Spatial'))
relation<-cbind(Hwgs,relation)
relation[is.na(relation)]<-0
relation1<-filter(relation,lakemaxP.1==0)
fwrite(x=relation1,file="D://Project/Holocene/relation1.csv")
relation1<-read.csv("D://Project/Holocene/relation1.csv")
library(tidyr)
relation1<-relation1%>%separate(geometry,c("lon","lat"),"[|]")
detach("package:tidyr")
relation1new<-cbind(relation1$lon,relation1$lat)
relation1new<-data.frame(relation1new)
relation1new$relationnew<-2
relation2<-filter(relation,lakemaxP.1!=0)
fwrite(x=relation2,file="D://Project/Holocene/relation2.csv")
relation2<-read.csv("D://Project/Holocene/relation2.csv")
library(tidyr)
relation2<-relation2%>%separate(geometry,c("lon","lat"),"[|]")
detach("package:tidyr")
relation2new<-cbind(relation2$lon,relation2$lat)
relation2new<-data.frame(relation2new)
relation2new$relationnew<-1
relationfinal<-rbind(relation1new,relation2new,nothaveHnew)
colnames(relationfinal)<-c('lon','lat','relationnew')
fwrite(x=relationfinal,file="D://Project/Holocene/relationfinal8000.csv")

pointwgs<-st_as_sf(relationfinal, coords=c('lon', 'lat'), crs=4326)
point_raster<-rasterize(as(pointwgs, 'Spatial'),europe_raster)

pdf(file="D://Project/Holocene/T8000.pdf")
par(mfrow=c(1,1),mar=c(3,3,3,3))
plot(point_raster$relationnew,col=c('grey','goldenrod1','red'),legend=FALSE)
plot(europe$geometry,add=TRUE)
dev.off()

##8ka temperature and 9ka temperature
tem<-read.csv("D://Project/resolution0.5/changeT/8000/difference.csv",header=TRUE)
glm_model<-glm(relationP~lakemaxP+I(lakemaxP^2)+lakeminP+I(lakeminP^2),dat=tem,family=binomial(link = "logit"))

allH<-read.csv("D://Project/Holocene/predictwinterT8000.csv",header=TRUE)
library(dplyr)
predict_absence1<-filter(allH,relationH=='0')
predict_presence1<-filter(allH,relationH=='1')
library(evaluate)
library(dismo)
glm_eval1<-evaluate(predict_presence1,predict_absence1,model=glm_model)
max_kappa1 <- plogis(threshold(glm_eval1, stat='spec_sens'))

allP<-read.csv("D://Project/Holocene/predictwinterT8000.csv",header=TRUE)
predict_absence2<-filter(allP,relationH=='0')
predict_presence2<-filter(allP,relationH=='1')
library(evaluate)
library(dismo)
glm_eval2<-evaluate(predict_presence2,predict_absence2,model=glm_model)
max_kappa2 <- plogis(threshold(glm_eval2, stat='spec_sens'))

haveH<-filter(allH,predictedH>=max_kappa1)
haveP<-filter(allP,predictedH>=max_kappa2)
nothaveH<-filter(allH,predictedH<max_kappa1)
nothaveHnew<-cbind(nothaveH$lon,nothaveH$lat)
nothaveHnew<-data.frame(nothaveHnew)
nothaveHnew$relationnew<-0


Hwgs<-st_as_sf(haveH, coords=c('lon', 'lat'), crs=4326)
H_raster<-rasterize(as(Hwgs, 'Spatial'),europe_raster)
Pwgs<-st_as_sf(haveP, coords=c('lon', 'lat'), crs=4326)
P_raster<-rasterize(as(Pwgs, 'Spatial'),europe_raster)
relation<-extract(P_raster,as(Hwgs,'Spatial'))
relation<-cbind(Hwgs,relation)
relation[is.na(relation)]<-0
relation1<-filter(relation,lakemaxP.1==0)
fwrite(x=relation1,file="D://Project/Holocene/relation1.csv")
relation1<-read.csv("D://Project/Holocene/relation1.csv")
library(tidyr)
relation1<-relation1%>%separate(geometry,c("lon","lat"),"[|]")
detach("package:tidyr")
relation1new<-cbind(relation1$lon,relation1$lat)
relation1new<-data.frame(relation1new)
relation1new$relationnew<-2
relation2<-filter(relation,lakemaxP.1!=0)
fwrite(x=relation2,file="D://Project/Holocene/relation2.csv")
relation2<-read.csv("D://Project/Holocene/relation2.csv")
library(tidyr)
relation2<-relation2%>%separate(geometry,c("lon","lat"),"[|]")
detach("package:tidyr")
relation2new<-cbind(relation2$lon,relation2$lat)
relation2new<-data.frame(relation2new)
relation2new$relationnew<-1


relation3<-extract(H_raster,as(Pwgs,'Spatial'))
relation3<-cbind(Pwgs,relation3)
relation3[is.na(relation3)]<-0
relation4<-filter(relation3,lakemaxP.1==0)
fwrite(x=relation4,file="D://Project/Holocene/relation4.csv")
relation4<-read.csv("D://Project/Holocene/relation4.csv")
library(tidyr)
relation4<-relation4%>%separate(geometry,c("lon","lat"),"[|]")
detach("package:tidyr")
relation4new<-cbind(relation4$lon,relation4$lat)
relation4new<-data.frame(relation4new)
relation4new$relationnew<-3

relationfinal<-rbind(relation1new,relation2new,relation4new)
colnames(relationfinal)<-c('lon','lat','relationnew')

allwgs<-st_as_sf(allP, coords=c('lon', 'lat'), crs=4326)
all_raster<-rasterize(as(allwgs, 'Spatial'),europe_raster)
relationfinalwgs<-st_as_sf(relationfinal, coords=c('lon', 'lat'), crs=4326)
relationfinal_raster<-rasterize(as(relationfinalwgs, 'Spatial'),europe_raster)
presence<-extract(relationfinal_raster,as(allwgs,'Spatial'))
presence<-cbind(allwgs,presence)
presence[is.na(presence)]<-0
fwrite(x=presence,file="D://Project/Holocene/relationfinalST80008000.csv")

pointwgs<-st_as_sf(presence, coords=c('lon', 'lat'), crs=4326)
point_raster<-rasterize(as(pointwgs, 'Spatial'),europe_raster)
pdf(file="D://Project/Holocene/T8vs9.pdf")
par(mfrow=c(1,1),mar=c(3,3,3,3))
plot(point_raster$relationnew,col=c('grey','aquamarine2','red','blue'),legend=FALSE)
plot(europe$geometry,add=TRUE)
dev.off()


####Present and solar/temperature
tem<-read.csv("D://Project/resolution0.5/changeT/8000/difference.csv")
glm_model<-glm(relationP~lakemaxP+I(lakemaxP^2)+lakeminP+I(lakeminP^2),dat=tem,family=binomial(link = "logit"))

allH<-read.csv("D://Project/Holocene/predictwinterST8000.csv",header=TRUE)
library(dplyr)
predict_absence1<-filter(allH,relationH=='0')
predict_presence1<-filter(allH,relationH=='1')
library(evaluate)
library(dismo)
glm_eval1<-evaluate(predict_presence1,predict_absence1,model=glm_model)
max_kappa1 <- plogis(threshold(glm_eval1, stat='spec_sens'))

allP<-read.csv("D://Project/Holocene/predictwinter2483.csv",header=TRUE)
predict_absence2<-filter(allP,relationP=='0')
predict_presence2<-filter(allP,relationP=='1')
library(evaluate)
library(dismo)
glm_eval2<-evaluate(predict_presence2,predict_absence2,model=glm_model)
max_kappa2 <- plogis(threshold(glm_eval2, stat='spec_sens'))

haveH<-filter(allH,predictedH>=max_kappa1)
haveP<-filter(allP,predictedP>=max_kappa2)
nothaveH<-filter(allH,predictedH<max_kappa1)
nothaveHnew<-cbind(nothaveH$lon,nothaveH$lat)
nothaveHnew<-data.frame(nothaveHnew)
nothaveHnew$relationnew<-0

Hwgs<-st_as_sf(haveH, coords=c('lon', 'lat'), crs=4326)
H_raster<-rasterize(as(Hwgs, 'Spatial'),europe_raster)
Pwgs<-st_as_sf(haveP, coords=c('lon', 'lat'), crs=4326)
P_raster<-rasterize(as(Pwgs, 'Spatial'),europe_raster)
relation<-extract(P_raster,as(Hwgs,'Spatial'))
relation<-cbind(Hwgs,relation)
relation[is.na(relation)]<-0
relation1<-filter(relation,lakemaxP.1==0)
fwrite(x=relation1,file="D://Project/Holocene/relation1.csv")
relation1<-read.csv("D://Project/Holocene/relation1.csv")
library(tidyr)
relation1<-relation1%>%separate(geometry,c("lon","lat"),"[|]")
detach("package:tidyr")
relation1new<-cbind(relation1$lon,relation1$lat)
relation1new<-data.frame(relation1new)
relation1new$relationnew<-2
relation2<-filter(relation,lakemaxP.1!=0)
fwrite(x=relation2,file="D://Project/Holocene/relation2.csv")
relation2<-read.csv("D://Project/Holocene/relation2.csv")
library(tidyr)
relation2<-relation2%>%separate(geometry,c("lon","lat"),"[|]")
detach("package:tidyr")
relation2new<-cbind(relation2$lon,relation2$lat)
relation2new<-data.frame(relation2new)
relation2new$relationnew<-1
relationfinal<-rbind(relation1new,relation2new,nothaveHnew)
colnames(relationfinal)<-c('lon','lat','relationnew')
fwrite(x=relationfinal,file="D://Project/Holocene/relationfinal.csv")

pointwgs<-st_as_sf(relationfinal, coords=c('lon', 'lat'), crs=4326)
point_raster<-rasterize(as(pointwgs, 'Spatial'),europe_raster)
pdf(file="D://Project/Holocene/solarT8000.pdf")
par(mfrow=c(1,1),mar=c(3,3,3,3))
plot(point_raster$relationnew,col=c('grey','goldenrod1','red'),legend=FALSE)
plot(europe$geometry,add=TRUE)
dev.off()



###solarT8ka vs solarT9ka
tem<-read.csv("D://Project/resolution0.5/changesolarT/8000/difference.csv")
glm_model<-glm(relationP~lakemaxP+I(lakemaxP^2)+lakeminP+I(lakeminP^2),dat=tem,family=binomial(link = "logit"))

allH<-read.csv("D://Project/Holocene/predictwinterST8000.csv",header=TRUE)
library(dplyr)
predict_absence1<-filter(allH,relationH=='0')
predict_presence1<-filter(allH,relationH=='1')
library(evaluate)
library(dismo)
glm_eval1<-evaluate(predict_presence1,predict_absence1,model=glm_model)
max_kappa1 <- plogis(threshold(glm_eval1, stat='spec_sens'))

allP<-read.csv("D://Project/Holocene/predictwinterST8000.csv",header=TRUE)
predict_absence2<-filter(allP,relationH=='0')
predict_presence2<-filter(allP,relationH=='1')
library(evaluate)
library(dismo)
glm_eval2<-evaluate(predict_presence2,predict_absence2,model=glm_model)
max_kappa2 <- plogis(threshold(glm_eval2, stat='spec_sens'))

haveH<-filter(allH,predictedH>=max_kappa1)
haveP<-filter(allP,predictedH>=max_kappa2)
nothaveH<-filter(allH,predictedH<max_kappa1)
nothaveHnew<-cbind(nothaveH$lon,nothaveH$lat)
nothaveHnew<-data.frame(nothaveHnew)
nothaveHnew$relationnew<-0

Hwgs<-st_as_sf(haveH, coords=c('lon', 'lat'), crs=4326)
H_raster<-rasterize(as(Hwgs, 'Spatial'),europe_raster)
Pwgs<-st_as_sf(haveP, coords=c('lon', 'lat'), crs=4326)
P_raster<-rasterize(as(Pwgs, 'Spatial'),europe_raster)
relation<-extract(P_raster,as(Hwgs,'Spatial'))
relation<-cbind(Hwgs,relation)
relation[is.na(relation)]<-0
relation1<-filter(relation,lakemaxP.1==0)
fwrite(x=relation1,file="D://Project/Holocene/relation1.csv")
relation1<-read.csv("D://Project/Holocene/relation1.csv")
library(tidyr)
relation1<-relation1%>%separate(geometry,c("lon","lat"),"[|]")
detach("package:tidyr")
relation1new<-cbind(relation1$lon,relation1$lat)
relation1new<-data.frame(relation1new)
relation1new$relationnew<-2
relation2<-filter(relation,lakemaxP.1!=0)
fwrite(x=relation2,file="D://Project/Holocene/relation2.csv")
relation2<-read.csv("D://Project/Holocene/relation2.csv")
library(tidyr)
relation2<-relation2%>%separate(geometry,c("lon","lat"),"[|]")
detach("package:tidyr")
relation2new<-cbind(relation2$lon,relation2$lat)
relation2new<-data.frame(relation2new)
relation2new$relationnew<-1


relation3<-extract(H_raster,as(Pwgs,'Spatial'))
relation3<-cbind(Pwgs,relation3)
relation3[is.na(relation3)]<-0
relation4<-filter(relation3,lakemaxP.1==0)
fwrite(x=relation4,file="D://Project/Holocene/relation4.csv")
relation4<-read.csv("D://Project/Holocene/relation4.csv")
library(tidyr)
relation4<-relation4%>%separate(geometry,c("lon","lat"),"[|]")
detach("package:tidyr")
relation4new<-cbind(relation4$lon,relation4$lat)
relation4new<-data.frame(relation4new)
relation4new$relationnew<-3

relationfinal<-rbind(relation1new,relation2new,relation4new)
colnames(relationfinal)<-c('lon','lat','relationnew')

allwgs<-st_as_sf(allP, coords=c('lon', 'lat'), crs=4326)
all_raster<-rasterize(as(allwgs, 'Spatial'),europe_raster)
relationfinalwgs<-st_as_sf(relationfinal, coords=c('lon', 'lat'), crs=4326)
relationfinal_raster<-rasterize(as(relationfinalwgs, 'Spatial'),europe_raster)
presence<-extract(relationfinal_raster,as(allwgs,'Spatial'))
presence<-cbind(allwgs,presence)
presence[is.na(presence)]<-0
fwrite(x=presence,file="D://Project/Holocene/relationfinalST80008000.csv")

pointwgs<-st_as_sf(presence, coords=c('lon', 'lat'), crs=4326)
point_raster<-rasterize(as(pointwgs, 'Spatial'),europe_raster)
pdf(file="D://Project/Holocene/solarT8vs9.pdf")
par(mfrow=c(1,1),mar=c(3,3,3,3))
plot(point_raster$relationnew,col=c('grey','aquamarine2','red','blue'),legend=FALSE)
plot(europe$geometry,add=TRUE)
dev.off()

###T vs solarT
tem<-read.csv("D://Project/resolution0.5/changesolarT/8000/difference.csv")
glm_model<-glm(relationP~lakemaxP+I(lakemaxP^2)+lakeminP+I(lakeminP^2),dat=tem,family=binomial(link = "logit"))

allH<-read.csv("D://Project/Holocene/predictwinterT8000.csv",header=TRUE)
library(dplyr)
predict_absence1<-filter(allH,relationH=='0')
predict_presence1<-filter(allH,relationH=='1')
library(evaluate)
library(dismo)
glm_eval1<-evaluate(predict_presence1,predict_absence1,model=glm_model)
max_kappa1 <- plogis(threshold(glm_eval1, stat='spec_sens'))

allP<-read.csv("D://Project/Holocene/predictwinterST8000.csv",header=TRUE)
predict_absence2<-filter(allP,relationH=='0')
predict_presence2<-filter(allP,relationH=='1')
library(evaluate)
library(dismo)
glm_eval2<-evaluate(predict_presence2,predict_absence2,model=glm_model)
max_kappa2 <- plogis(threshold(glm_eval2, stat='spec_sens'))

haveH<-filter(allH,predictedH>=max_kappa1)
haveP<-filter(allP,predictedH>=max_kappa2)
nothaveH<-filter(allH,predictedH<max_kappa1)
nothaveHnew<-cbind(nothaveH$lon,nothaveH$lat)
nothaveHnew<-data.frame(nothaveHnew)
nothaveHnew$relationnew<-0

Hwgs<-st_as_sf(haveH, coords=c('lon', 'lat'), crs=4326)
H_raster<-rasterize(as(Hwgs, 'Spatial'),europe_raster)
Pwgs<-st_as_sf(haveP, coords=c('lon', 'lat'), crs=4326)
P_raster<-rasterize(as(Pwgs, 'Spatial'),europe_raster)
relation<-extract(P_raster,as(Hwgs,'Spatial'))
relation<-cbind(Hwgs,relation)
relation[is.na(relation)]<-0
relation1<-filter(relation,lakemaxP.1==0)
fwrite(x=relation1,file="D://Project/Holocene/relation1.csv")
relation1<-read.csv("D://Project/Holocene/relation1.csv")
library(tidyr)
relation1<-relation1%>%separate(geometry,c("lon","lat"),"[|]")
detach("package:tidyr")
relation1new<-cbind(relation1$lon,relation1$lat)
relation1new<-data.frame(relation1new)
relation1new$relationnew<-2
relation2<-filter(relation,lakemaxP.1!=0)
fwrite(x=relation2,file="D://Project/Holocene/relation2.csv")
relation2<-read.csv("D://Project/Holocene/relation2.csv")
library(tidyr)
relation2<-relation2%>%separate(geometry,c("lon","lat"),"[|]")
detach("package:tidyr")
relation2new<-cbind(relation2$lon,relation2$lat)
relation2new<-data.frame(relation2new)
relation2new$relationnew<-1

relation3<-extract(H_raster,as(Pwgs,'Spatial'))
relation3<-cbind(Pwgs,relation3)
relation3[is.na(relation3)]<-0
relation4<-filter(relation3,lakemaxP.1==0)
fwrite(x=relation4,file="D://Project/Holocene/relation4.csv")
relation4<-read.csv("D://Project/Holocene/relation4.csv")
library(tidyr)
relation4<-relation4%>%separate(geometry,c("lon","lat"),"[|]")
detach("package:tidyr")
relation4new<-cbind(relation4$lon,relation4$lat)
relation4new<-data.frame(relation4new)
relation4new$relationnew<-3

relationfinal<-rbind(relation1new,relation2new,relation4new)
colnames(relationfinal)<-c('lon','lat','relationnew')

allwgs<-st_as_sf(allP, coords=c('lon', 'lat'), crs=4326)
all_raster<-rasterize(as(allwgs, 'Spatial'),europe_raster)
relationfinalwgs<-st_as_sf(relationfinal, coords=c('lon', 'lat'), crs=4326)
relationfinal_raster<-rasterize(as(relationfinalwgs, 'Spatial'),europe_raster)
presence<-extract(relationfinal_raster,as(allwgs,'Spatial'))
presence<-cbind(allwgs,presence)
presence[is.na(presence)]<-0
fwrite(x=presence,file="D://Project/Holocene/relationfinalSTT8000.csv")

pointwgs<-st_as_sf(presence, coords=c('lon', 'lat'), crs=4326)
point_raster<-rasterize(as(pointwgs, 'Spatial'),europe_raster)
pdf(file="D://Project/Holocene/TvssolarT8000.pdf")
par(mfrow=c(1,1),mar=c(3,3,3,3))
plot(point_raster$relationnew,col=c('grey','aquamarine2','red','blue'),legend=FALSE)
plot(europe$geometry,add=TRUE)
dev.off()


####################################lm between lairmax, and air maxmin diff/solar####################
tem<-read.csv("D://Project/resolution0.5/origin3676/difference1.csv",header=TRUE)
library(dplyr)
tem<-filter(tem,lairdiff>=3)
fit<-lm(lairdiff~airdiff+solarP,data=tem)
summary(fit)
library(visreg)
par(mfrow=c(1,2),mar=c(4,4,4,4))
visreg(fit,'airdiff',cex.lab=1.2,xlab='Air temperature amplitude(¡ãC)',ylab='The difference between lake and air summer temperature(¡ãC)')
visreg(fit,'solarP',cex.lab=1.2,xlab='July surface solar radiation(W/m^2)',ylab='The difference between lake and air summer temperature(¡ãC)')

fit<-lm(airmaxP~airminP+solarP,data=tem)
summary(fit)
library(visreg)
par(mfrow=c(1,2),mar=c(4,4,4,4))
visreg(fit,'airminP',cex.lab=1.2,xlab='Air winter temperature(¡ãC)',ylab='Air summer temperature(¡ãC)')
visreg(fit,'solarP',cex.lab=1.2,xlab='July surface solar radiation(W/m^2)',ylab='Air summer temperature(¡ãC)')


tem<-read.csv("D://Project/resolution0.5/changesolar/difference.csv",header=TRUE)
tem<-filter(tem,lairdiff8>=3)
fit<-lm(lairdiff8~airdiff8+solar8,data=tem)
summary(fit)
par(mfrow=c(1,2),mar=c(4,4,4,4))
visreg(fit,'airdiff8',xlab='Air temperature amplitude(¡ãC)',ylab='The difference between lake and air summer temperature(¡ãC)')
visreg(fit,'solar8',xlab='July surface solar radiation(W/m^2)',ylab='The difference between lake and air summer temperature(¡ãC)')

tem<-read.csv("D://Project/resolution0.5/changesolar/difference.csv",header=TRUE)
tem<-filter(tem,lairdiff9>=3)
fit<-lm(lairdiff9~airdiff9+solar9,data=tem)
summary(fit)
par(mfrow=c(1,2),mar=c(4,4,4,4))
visreg(fit,'airdiff9',xlab='Air temperature amplitude(¡ãC)',ylab='The difference between lake and air summer temperature(¡ãC)')
visreg(fit,'solar9',xlab='July surface solar radiation(W/m^2)',ylab='The difference between lake and air summer temperature(¡ãC)')


tem<-read.csv("D://Project/resolution0.5/changeT/difference1.csv",header=TRUE)
tem<-filter(tem,lairdiff8>=3)
fit<-lm(lairdiff8~airdiff8+solarP,data=tem)
summary(fit)
par(mfrow=c(1,2),mar=c(4,4,4,4))
visreg(fit,'airdiff8',xlab='Air temperature amplitude(¡ãC)',ylab='The difference between lake and air summer temperature(¡ãC)')
visreg(fit,'solarP',xlab='July surface solar radiation(W/m^2)',ylab='The difference between lake and air summer temperature(¡ãC)')


tem<-read.csv("D://Project/resolution0.5/changeT/difference1.csv",header=TRUE)
tem<-filter(tem,lairdiff9>=3)
fit<-lm(lairdiff9~airdiff9+solarP,data=tem)
summary(fit)
par(mfrow=c(1,2),mar=c(4,4,4,4))
visreg(fit,'airdiff9',xlab='Air temperature amplitude(¡ãC)',ylab='The difference between lake and air summer temperature(¡ãC)')
visreg(fit,'solarP',xlab='July surface solar radiation(W/m^2)',ylab='The difference between lake and air summer temperature(¡ãC)')



tem<-read.csv("D://Project/resolution0.5/changesolarT/difference1.csv",header=TRUE)
tem<-filter(tem,lairdiff8>=3)
fit<-lm(lairdiff8~airdiff8+solar8,data=tem)
summary(fit)
par(mfrow=c(1,2),mar=c(4,4,4,4))
visreg(fit,'airdiff8',xlab='Air temperature amplitude(¡ãC)',ylab='The difference between lake and air summer temperature(¡ãC)')
visreg(fit,'solar8',xlab='July surface solar radiation(W/m^2)',ylab='The difference between lake and air summer temperature(¡ãC)')


tem<-read.csv("D://Project/resolution0.5/changesolarT/difference1.csv",header=TRUE)
tem<-filter(tem,lairdiff9>=3)
fit<-lm(lairdiff9~airdiff9+solar9,data=tem)
summary(fit)
par(mfrow=c(1,2),mar=c(4,4,4,4))
visreg(fit,'airdiff9',xlab='Air temperature amplitude(¡ãC)',ylab='The difference between lake and air summer temperature(¡ãC)')
visreg(fit,'solar9',xlab='July surface solar radiation(W/m^2)',ylab='The difference between lake and air summer temperature(¡ãC)')
