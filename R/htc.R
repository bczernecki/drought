library(parallel)
library(pbmcapply)
library(rasterVis)
library(raster)
library(dplyr)
library(lubridate)
library(rwrfhydro)
library(rgdal)
library(tmap)
library(rgeos)
library(rgdal)
library(sp)

wojewodztwa <- readOGR("data/POL_adm1.shp")
pol <- readOGR("data/POL_adm0.shp")
rzeki <- readOGR("data/rzekiPL.shp") 
jeziora <- readOGR("data/ne_10m_lakes.shp")

proj4 <- "+proj=tmerc +lat_0=0 +lon_0=19 +k=0.9993 +x_0=500000 +y_0=-5300000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

wojewodztwa <- spTransform(wojewodztwa,proj4)
pol <- spTransform(pol, proj4)
#jeziora <- (crop(pol, jeziora))
rzeki <- spTransform(rzeki, proj4)


##########################  opad  #######################################
df1 <- data.frame(date = seq.Date(as.Date("2015-01-01"), as.Date("2015-12-31"), by="day"))
df1$decade <- ceiling((day(df1$date)-0.1)/10)
df1$decade <- ifelse(df1$decade>3, 3, df1$decade) + (month(df1$date)-1)*3
df1$filename_opad <- paste0("opad_", format(as.Date(df1$date), "%Y.%m.%d"), ".tif")
df1$filename_opadIDW <- paste0("opad_IDW_", format(as.Date(df1$date), "%Y-%m-%d"), ".tif")
df1$filename_temp <- paste0("tavg_", format(as.Date(df1$date), "%Y.%m.%d"), ".tif")
 

sumfunction<- function(input="inp"){
  r1<-stack(input)
  r1[r1 < 0.08] <- 0
  r1<- calc(r1, function(x) sum(x, na.rm=T))
}


# dodanie progu termicznego 5*C:
sumfunction_temp<- function(input="inp"){
  r1<-stack(input)
  r1[r1 < 5] <- 0
  r1<- calc(r1, function(x) sum(x, na.rm=T))
}

meanfunction<- function(input="inp"){
  r1<-stack(input)
  r1<- calc(r1, function(x) mean(x, na.rm=T))
}


getdecade <- function(no = 4){
  ind <- which(df1$decade %in% no:(no-3))
  list(opadfiles = df1$filename_opad[ind], tempfiles = df1$filename_temp[ind], opadfilesIDW = df1$filename_opadIDW[ind])
}


########################### HTC ######################
htc_decade <- function(x = 5){
temp <- meanfunction( input  = stack(paste0("data/",getdecade(no = x)$tempfiles)))
opad <- sumfunction( input  = stack(paste0("data/",getdecade(no = x)$opadfiles)))
opad <- mask(opad, pol)
temp <- mask(temp, pol)
temp[temp[]<0] <- 0
htc <- opad/round(temp,2)
return(htc)
}

#### HTC version 2: #####################
htc_decade2 <- function(x = 5){
  temp <- sumfunction( input  = stack(paste0("data/",getdecade(no = x)$tempfiles)))
  opad <- sumfunction( input  = stack(paste0("data/",getdecade(no = x)$opadfiles)))*10
  opad <- mask(opad, pol)
  temp <- mask(temp, pol)
  temp[temp[]<0] <- 0
  htc <- opad/round(temp,2)
  return(htc)
}


#htc_decade(6)

htc_maps <- pbmclapply(10:36, htc_decade, mc.cores = 6)
htc_maps <- stack(htc_maps)
opadek <- pbmclapply(10:36, function(x) sumfunction(stack(paste0("data/",getdecade(no = x)$opadfiles))), mc.cores = 2)
opadek <- stack(opadek)
opadekIDW <- pbmclapply(10:36, function(x) sumfunction(stack(paste0("data/",getdecade(no = x)$opadfilesIDW))), mc.cores = 2)
opadekIDW <- stack(opadekIDW)

plot(opadek)
#htc_maps <- mclapply(10:36 , htc_decade, mc.cores = 4)
#plot(htc_maps)

# names:
nazwy <- function( no = 4){
  ind <- which(df1$decade %in% no)
  paste0("dek_", no, "__",df1$date[ind][length(ind)])
}

# plotting htc:
png(filename = "htc_wrf.png", width = 1201, height = 981, pointsize = 30)
names(htc_maps) <- sapply(10:36, FUN = function(x) nazwy(x))
col = c("black","brown","red","orange","orange","yellow","skyblue","blue2","purple")
p=spplot(htc_maps[[1:20]], zlim=c(0,15), at=c(0,0.5,1:5,10,15), col.regions=col, layout =c(5,4))
p=p + layer(sp.polygons(pol, lwd=2.2, col='gray10',fill=F))
print(p)
dev.off()

# plotting precipitation:
png(filename = "opad_wrf.png", width = 1201, height = 981, pointsize = 30)
names(opadek) <- sapply(10:36, FUN = function(x) nazwy(x))
opadek <- mask(opadek, pol)
col = c("black","brown","red","orange","orange","yellow","yellowgreen", "skyblue","blue2","steelblue","purple")
p=spplot(opadek[[1:20]], zlim=c(0,150), at=c(0,2,5,10,20,30,50,75, 100,150), col.regions=col, layout =c(5,4))
p=p + layer(sp.polygons(pol, lwd=2.2, col='gray10',fill=F))
print(p)
dev.off()

# observational precip:
png(filename = "opad_imgw.png", width = 1201, height = 981, pointsize = 30)
names(opadekIDW) <- sapply(10:36, FUN = function(x) nazwy(x))
opadekIDW <- mask(opadekIDW, pol)
col = c("black","brown","red","orange","orange","yellow","yellowgreen", "skyblue","blue2","steelblue","purple")
p=spplot(opadekIDW[[1:20]], zlim=c(0,150), at=c(0,2,5,10,20,30,50,75, 100,150), col.regions=col, layout =c(5,4))
p=p + layer(sp.polygons(pol, lwd=2.2, col='gray10',fill=F))
print(p)
dev.off()

