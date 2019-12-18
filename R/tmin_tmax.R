# skrypt dla tmin / tmax

library(raster)
library(rgdal)

args = commandArgs(trailingOnly=TRUE)
print(args[1])
print(args[2])
#args <- c(20191217,1)
start_wrf <- as.Date(paste0(args[1]), format="%Y%m%d")
dzien <- as.numeric(as.character(args[2])) # tutaj chodzi o dzien wyprzedzenia wzgledem startu prognozy (np. +1, +2)
patt <- as.character(start_wrf+dzien)
pathway <- paste0("/media/wh/dysk12/wrfout/", format(start_wrf, "%Y%m%d") ,"/wrfprd")

# warstwy GIS:
#load(file = "data/gisy.Rdata")
# liczymy ekstrema dla temperatur

tempfil<-dir(path=pathway, pattern = "T2", full.names = T)
tempfil <- tempfil[grep(pattern = "tif", x = tempfil)] # i tylko domena 03
r <- raster::stack(tempfil)


tmin1 <- min(r[[1:24]])
tmin2 <- min(r[[25:48]])
tmin3 <- min(r[[49:72]])

tmax1 <- max(r[[1:24]])
tmax2 <- max(r[[25:48]])
tmax3 <- max(r[[49:72]])



writeRaster(tmin1, filename = paste0("/home/wh/tmin/tmin_",patt,".tif"),overwrite=TRUE)
patt2 = as.Date(patt, "%Y-%m-%d")+1
writeRaster(tmin2, filename = paste0("/home/wh/tmin/tmin_",patt2,".tif"),overwrite=TRUE)
patt3 = as.Date(patt, "%Y-%m-%d")+2
writeRaster(tmin3, filename = paste0("/home/wh/tmin/tmin_",patt3,".tif"),overwrite=TRUE)

writeRaster(tmax1, filename = paste0("/home/wh/tmax/tmax_",patt,".tif"),overwrite=TRUE)
writeRaster(tmax2, filename = paste0("/home/wh/tmax/tmax_",patt2,".tif"),overwrite=TRUE)
writeRaster(tmax3, filename = paste0("/home/wh/tmax/tmax_",patt3,".tif"),overwrite=TRUE)
