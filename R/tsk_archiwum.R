# wez wszystkie warstwy z historycznego wrfa i zrob z tego godzinowe geotiffy:

library(raster)
library(rwrfhydro)
library(rgdal)
library(tmap)
library(parallel)
library(raster)

r_baza = raster("~/tavg/tavg_2019-10-01.tif")
crs <- crs(r_baza)

###############################################################
################# Parametry do ustawienia #####################
###############################################################
files = NULL
for (i in 1998:2018){
  print(i)
  setwd(paste0("~/nas/03_WRF_wyniki_wrfout_d03"))
  files = c(files, dir(path = paste0(i,"/"), pattern="wrfout", full.names = T))
}

files[length(files)]
table(nchar(files))

files = paste0("~/nas/03_WRF_wyniki_wrfout_d03/",files)

# warstwy GIS:
load(file = "~/github/drought/data/gisy.Rdata")

myfunction_for_converting <- function(input = 'netcdf',  variable = "var"){
  geofile <- input
  proj4<- GetProj(geofile)
  output <- paste0(input, "_tsk.tif")
  output = gsub(x = output, pattern = '~/nas/03_WRF_wyniki_wrfout_d03/', '/home/wh/tsk/')
  output = paste0("/home/wh/tsk/godzinowe/",basename(output))
  output = gsub(x = output, pattern = 'wrfout_d03_', '')
  output = gsub(x = output, pattern = ":", replacement = "_")
  ExportGeogrid(inFile = input, inVar = variable, outFile = output)
  r = raster(output)
  r = projectRaster(from = r, to = r_baza)
  writeRaster(r, filename = output, overwrite=T)
  
}

for (i in 1998:2018){
  dir.create(paste0("/home/wh/tsk/", i))
}


mclapply(files, function(x) myfunction_for_converting(input = x, variable="TSK"), mc.cores = 31)
# jeszcze w bashu:
# for i in `seq 1998 2020` ; do echo $i ; mv $i/*tif . ; done
# powinnismy miec teraz stworzone geotify

# jeszcze ogarnac geotiffy dla operacyjnych warstw:
# ktore sa tutaj: /nas/07_DYSK_KOPISA_4T/wrfout$ 

# dorzucic wszystkie pliki za 2019 do chwili obecnej
df = seq.Date(from = as.Date("2018-12-31"), to = as.Date("2019-12-31"), by = "day")
df2 = rep(paste0("~/nas/07_DYSK_KOPISA_4T/wrfout/",
                 format(df, "%Y%m%d"), 
                 "/wrfprd/wrfout_d03_",
                 format(df+1, "%Y-%m-%d_")), each = 24)
linki = paste0(df2,      
               sprintf("%02d", 0:23),
               ":00:00")
df = data.frame(path = linki, jestnadysku = file.exists(linki), stringsAsFactors = F)
df = df[-which(df$jestnadysku==F),]
df$path
mclapply(df$path, function(x) myfunction_for_converting(input = x, variable="TSK"), mc.cores = 31)


# na koniec od 2020:
# dorzucic wszystkie pliki za 2019 do chwili obecnej
df = seq.Date(from = as.Date("2019-12-31"), to = as.Date("2020-05-01"), by = "day")
df2 = rep(paste0("/media/wh/dysk12/wrfout/",
                 format(df, "%Y%m%d"), 
                 "/wrfprd/wrfout_d03_",
                 format(df+1, "%Y-%m-%d_")), each = 24)
linki = paste0(df2,      
               sprintf("%02d", 0:23),
               ":00:00")
df = data.frame(path = linki, jestnadysku = file.exists(linki), stringsAsFactors = F)
df = df[-which(df$jestnadysku==F),]
df$path
mclapply(df$path, function(x) myfunction_for_converting(input = x, variable="TSK"), mc.cores = 31)

