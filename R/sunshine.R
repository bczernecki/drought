library(raster)
#library(dplyr)
#library(lubridate)
library(rwrfhydro)
library(rgdal)
library(tmap)
library(parallel)

###############################################################
################# Parametry do ustawienia #####################
###############################################################
args = commandArgs(trailingOnly=TRUE)
print(args[1])
print(args[2])
#args <- c(20191217,1)
start_wrf <- as.Date(paste0(args[1]), format="%Y%m%d")
dzien <- as.numeric(as.character(args[2])) # tutaj chodzi o dzien wyprzedzenia wzgledem startu prognozy (np. +1, +2)
patt <- as.character(start_wrf+dzien)
#print()
#patt<- "2019-05-20"
pathway <- paste0("/media/wh/dysk12/wrfout/", format(start_wrf, "%Y%m%d") ,"/wrfprd")
day <- dir(path=pathway , pattern = patt,full.names = T)
day <- day[grep(pattern = "00$", x = day)] # bez geotiffow
if(length(day[grep(pattern = "d03", x = day)])>0) day <- day[grep(pattern = "d03", x = day)] # i tylko domena 03
day <- as.list(day) 

print("jestem tu 1")
# musimy pamietac, ze tu jest wiecej dni do przetrawienia
day2 <- dir(path=pathway , pattern = as.character(as.Date(patt)+1),full.names = T)
day2 <- day2[grep(pattern = "00$", x = day2)] # bez geotiffow
if(length(day2[grep(pattern = "d03", x = day2)])>0) day2 <- day2[grep(pattern = "d03", x = day2)] # i tylko domena 03
day2 <- as.list(day2) 

day3 <- dir(path=pathway , pattern = as.character(as.Date(patt)+2),full.names = T)
day3 <- day3[grep(pattern = "00$", x = day3)] # bez geotiffow
if(length(day3[grep(pattern = "d03", x = day3)])>0) day3 <- day3[grep(pattern = "d03", x = day3)] # i tylko domena 03
day3 <- as.list(day3) 



# warstwy GIS:
load(file = "data/gisy.Rdata")
# wojewodztwa <- readOGR("data/POL_adm1.shp")
# pol <- readOGR("data/POL_adm0.shp")
# rzeki <- readOGR("data/rzekiPL.shp") 
# jeziora <- readOGR("data/jeziora.shp")
# proj4 <- "+proj=lcc +lat_1=49.826000213623 +lat_2=49.826000213623 +lat_0=51.8421516418457 +lon_0=16.2469997406006 +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs"
# wojewodztwa <- spTransform(wojewodztwa,proj4)
# pol <- spTransform(pol, proj4)
# jeziora <- spTransform(jeziora, proj4)
# rzeki <- spTransform(rzeki, proj4)
# centroidy <-  gCentroid(wojewodztwa,byid=TRUE)

source("~/github/drought/R/Export2Raster.R")

myfunction_for_converting <- function(input = 'netcdf',  variable = "var"){
  geofile <- input
  proj4<- GetProj(geofile)
  output <- paste0(input, "_", variable, "_.tif")
  Export2Raster(inFile = input, inVar = variable, outFile = output)
}

mclapply(day, function(x) myfunction_for_converting(input = x, variable="SWDOWN"), mc.cores = 24)
mclapply(day2, function(x) myfunction_for_converting(input = x, variable="SWDOWN"), mc.cores = 24)
mclapply(day3, function(x) myfunction_for_converting(input = x, variable="SWDOWN"), mc.cores = 24)


# powinnismy miec teraz stworzone geotify


##########################################################################
# liczymy srednie uslonecznienie z promieniowania
tempfil<-dir(path=pathway, pattern = "SWDOWN", full.names = T)
tempfil <- tempfil[grep(pattern = paste0("d03_"), x = tempfil)] # i tylko domena 03
tempfil<- stack(tempfil)


r <- calc(tempfil, fun=function(x){ifelse(x>120, 1, 0)})



r1 <- sum(r[[1:24]])
r2 <- sum(r[[25:48]])
r3 <- sum(r[[49:72]])


writeRaster(r1, filename = paste0("/home/wh/sunshine/sunshine_",patt,".tif"),overwrite=TRUE)
patt = as.Date(patt, "%Y-%m-%d")+1
writeRaster(r2, filename = paste0("/home/wh/sunshine/sunshine_",patt,".tif"),overwrite=TRUE)
patt = as.Date(patt, "%Y-%m-%d")+1
writeRaster(r3, filename = paste0("/home/wh/sunshine/sunshine_",patt,".tif"),overwrite=TRUE)
