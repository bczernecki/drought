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
#args <- c(20200101,1)
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


myfunction_for_converting <- function(input = 'netcdf',  variable = "var"){
  geofile <- input
  proj4<- GetProj(geofile)
  output <- paste0(input, "_", variable, "_.tif")
  ExportGeogrid(inFile = input, inVar = variable, outFile = output)
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

napromieniowanie1 <- sum(tempfil[[1:24]])
napromieniowanie2 <- sum(tempfil[[25:48]])
napromieniowanie3 <- sum(tempfil[[49:72]])


writeRaster(r1, filename = paste0("/home/wh/sunshine/sunshine_",patt,".tif"),overwrite=TRUE)
writeRaster(r2, filename = paste0("/home/wh/sunshine/sunshine_",as.Date(patt, "%Y-%m-%d")+1,".tif"),overwrite=TRUE)
writeRaster(r3, filename = paste0("/home/wh/sunshine/sunshine_",as.Date(patt, "%Y-%m-%d")+2,".tif"),overwrite=TRUE)




skala=c('white','yellow','orange','red','red3','magenta', 'brown')
color.gradient <- function(x, colors=skala, colsteps=k) {
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min,max2, length.out=colsteps)) ] )
}
rbPal <- colorRampPalette(skala)
tempcolores <- rbPal(30)

# rysowanie mapy dla uslonecznienia

temperatura_map<- function(input="inp", output="outp", title = "tytul"){
  
  obj1<- mask(input, pol)
  centroidy$column <- sprintf(round(as.vector(raster::extract(obj1, centroidy)),1),fmt = '%#.1f')
  breaks2 <-round(seq(0, 1000, length.out = length(tempcolores)))

  tempcolores2 <- tempcolores
  
  tm_shape(obj1) +
    tm_raster(title= title,
              interval.closure = "left",legend.hist = T,
              palette = tempcolores2, breaks=breaks2, 
              legend.is.portrait = FALSE,
              interpolate = FALSE)  +
    
    #Border  
    tm_shape(pol) +
    tm_polygons(alpha = 0.001, lwd=1.5) +
    
    #Border of counties 
    tm_shape(wojewodztwa)+
    tm_polygons(alpha = 0.01, lwd=0.7)+
    
    #Rivers    
    tm_shape(rzeki)+
    tm_lines(col="#2669d6", lwd=1.1) +
    
    #Lakes
    tm_shape(jeziora)+
    tm_polygons(col="#2669d6") +
    
    
    #Title of the figure
    tm_layout(
      aes.palette = "div",
      sepia.intensity = 0.2,
      legend.just = "right",
      title.color = "blue",
      compass.type = "arrow",
      title.bg.color = "white", 
      title.bg.alpha = 0.5,
      title.size = 45,
      #title.position = c(0.02,0.06),
      legend.outside = T,
      legend.outside.position =  "bottom",
      legend.width = 5,
      legend.hist.width = 0.9,
      legend.hist.height = 0.6,
      legend.title.size = 0.90,
      legend.text.size = 0.5,
      #legend.position = c("right","bottom"),
      legend.bg.color = "#FFFFFF60",
      legend.height = 0.9,
      legend.frame.lwd = 0.2,
      legend.frame = F,
      legend.bg.alpha = 1,
      space.color="grey90",
      legend.format = list(text.separator = " ", format = formatC("f")))+
    
    #Lon/Lat    
    tm_grid(projection = "longlat", x = 10:30, y=40:60, labels.col = "black", col = "gray",lwd = 0.5,
            labels.size = 0.4, labels.inside.frame = T) + 
    #Mean values of counties
    tm_shape(centroidy)+
    tm_text("column", size = 0.6) +   
    
    #Compass
    tm_compass(size = 1, fontsize = 0.7,
               position = c(0.04,0.9), color.light = "grey90") +
    
    # scale bar
    tm_scale_bar(width = 0.12,size = 0.35,breaks = c(0,50,100,150), position = c("left","bottom")) +
    
    # windhydro credits
    tm_credits("(c) WIND-HYDRO 2020", position = c("left", "bottom"), 
               size = 0.35, bg.color = "white")
  
}


www_path <- gsub(x = pathway, pattern = "wrfprd","www")
dir.create(www_path)


# dorzucenie kodu do plotowania rastrow godzinowych:
nazwy <- gsub( x = gsub(x = gsub(x = names(tempfil), pattern = "wrfout_d03_", ""), pattern = "00_SWDOWN", ""), pattern = "_", " ")
fname <- gsub(x = gsub(nazwy, pattern = " ", replacement = ""), pattern = '.', replacement = "", fixed=T)
for (i in 1:length(names(tempfil))){
  
  p <- temperatura_map(input=tempfil[[i]], title = paste0("Promieniowanie słoneczne  [W/m2] \n", nazwy[i], "UTC"))
  tmap_save(p + tm_layout(inner.margins = c(-0.1, -0.06, -0.1, -0.1)), 
            filename = paste0(www_path, "/promieniowanie_",fname[i],".png"), width=1000, height=1300)
  
}



### rysowanie rastrow dobowych:

skala=c('white','yellow','orange','red','red3','magenta', 'brown')
rbPal <- colorRampPalette(skala)
tempcolores <- rbPal(30)


temperatura_map<- function(input="inp", output="outp", title = "tytul"){
  
  obj1<- mask(input, pol)
  centroidy$column <- sprintf(round(as.vector(raster::extract(obj1, centroidy)),1),fmt = '%#.1f')
  breaks2 <-round(seq(0, 8000, length.out = length(tempcolores)))
  
  tempcolores2 <- tempcolores
  
  tm_shape(obj1) +
    tm_raster(title= title,
              interval.closure = "left",legend.hist = T,
              palette = tempcolores2, breaks=breaks2, 
              legend.is.portrait = FALSE,
              interpolate = FALSE)  +
    
    #Border  
    tm_shape(pol) +
    tm_polygons(alpha = 0.001, lwd=1.5) +
    
    #Border of counties 
    tm_shape(wojewodztwa)+
    tm_polygons(alpha = 0.01, lwd=0.7)+
    
    #Rivers    
    tm_shape(rzeki)+
    tm_lines(col="#2669d6", lwd=1.1) +
    
    #Lakes
    tm_shape(jeziora)+
    tm_polygons(col="#2669d6") +
    
    
    #Title of the figure
    tm_layout(
      aes.palette = "div",
      sepia.intensity = 0.2,
      legend.just = "right",
      title.color = "blue",
      compass.type = "arrow",
      title.bg.color = "white", 
      title.bg.alpha = 0.5,
      title.size = 45,
      #title.position = c(0.02,0.06),
      legend.outside = T,
      legend.outside.position =  "bottom",
      legend.width = 5,
      legend.hist.width = 0.9,
      legend.hist.height = 0.6,
      legend.title.size = 0.90,
      legend.text.size = 0.5,
      #legend.position = c("right","bottom"),
      legend.bg.color = "#FFFFFF60",
      legend.height = 0.9,
      legend.frame.lwd = 0.2,
      legend.frame = F,
      legend.bg.alpha = 1,
      space.color="grey90",
      legend.format = list(text.separator = " ", format = formatC("f")))+
    
    #Lon/Lat    
    tm_grid(projection = "longlat", x = 10:30, y=40:60, labels.col = "black", col = "gray",lwd = 0.5,
            labels.size = 0.4, labels.inside.frame = T) + 
    #Mean values of counties
    tm_shape(centroidy)+
    tm_text("column", size = 0.6) +   
    
    #Compass
    tm_compass(size = 1, fontsize = 0.7,
               position = c(0.04,0.9), color.light = "grey90") +
    
    # scale bar
    tm_scale_bar(width = 0.12,size = 0.35,breaks = c(0,50,100,150), position = c("left","bottom")) +
    
    # windhydro credits
    tm_credits("(c) WIND-HYDRO 2020", position = c("left", "bottom"), 
               size = 0.35, bg.color = "white")
  
}


cat(paste("dzien pierwszy", patt))
p <- temperatura_map(input=napromieniowanie1, title = paste0("Suma dobowa napromieniowania [W/m2] \n", patt , " (00-23 UTC)"))
tmap_save(p + tm_layout(inner.margins = c(-0.1, -0.06, -0.1, -0.1)), 
          filename = paste0(www_path, "/promieniowanie_",patt,".png"), width=1000, height=1300)
writeRaster(napromieniowanie1, filename = paste0(www_path,"/promieniowanie_",patt,".tif"),overwrite=TRUE)


patt2 = as.Date(patt)+1
cat(paste("dzien drugi", patt2))
p2 <- temperatura_map(input=napromieniowanie2, title = paste0("Suma dobowa napromieniowania [W/m2] \n", patt2 , " (00-23 UTC)"))
tmap_save(p2 + tm_layout(inner.margins = c(-0.1, -0.06, -0.1, -0.1)), 
          filename = paste0(www_path, "/promieniowanie_",patt2,".png"), width=1000, height=1300)
writeRaster(napromieniowanie2, filename = paste0(www_path,"/promieniowanie_",patt2,".tif"),overwrite=TRUE)


patt3 = as.Date(patt)+2
cat(paste("dzien trzeci", patt3))
p3 <- temperatura_map(input=napromieniowanie3, title = paste0("Suma dobowa napromieniowania [W/m2] \n", patt3 , " (00-23 UTC)"))
tmap_save(p3 + tm_layout(inner.margins = c(-0.1, -0.06, -0.1, -0.1)), 
          filename = paste0(www_path, "/promieniowanie_",patt3,".png"), width=1000, height=1300)
writeRaster(napromieniowanie3, filename = paste0(www_path,"/promieniowanie_",patt3,".tif"),overwrite=TRUE)

