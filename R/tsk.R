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
#args <- c(20200502,1)
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

# warstwy GIS:
load(file = "/home/wh/github/drought/data/gisy2.Rdata")
library(raster)
r_baza = raster("~/tavg/tavg_2019-10-01.tif")
crs <- crs(r_baza)



myfunction_for_converting <- function(input = 'netcdf',  variable = "var"){
  geofile <- input
  proj4<- GetProj(geofile)
  output <- paste0(input, "_", variable, "_.tif")
  ExportGeogrid(inFile = input, inVar = variable, outFile = output)
  r = raster(output)
  r = projectRaster(from = r, to = r_baza)
  writeRaster(r, filename = output, overwrite=T)
}

mclapply(day, function(x) myfunction_for_converting(input = x, variable="TSK"), mc.cores = 24)
# powinnismy miec teraz stworzone geotify

# od razu je przekopiujmy do home'a:
tski = dir(path=pathway, pattern = "TSK_.tif",full.names = T)
oryg = as.POSIXct(basename(tski), format = "wrfout_d03_%Y-%m-%d_%H:%M:%S_TSK_.tif")
nowe = paste0("/home/wh/tsk/godzinowe/", as.character(format(oryg, "%Y-%m-%d_%H_%M_%S_tsk.tif")))
file.copy(tski, nowe, overwrite = T)

##########################################################################
# liczymy srednia z poszczegolnych warstw:
tempfil<-dir(path=pathway, pattern = "TSK", full.names = T)
if(length(tempfil[grep(pattern = patt, x = tempfil)])>24) {
  tempfil <- tempfil[grep(pattern = paste0(patt), x = tempfil)] # tylko dany dzien
}
tempfil<- stack(tempfil[grepl(pattern=patt, tempfil)])
mintemp<- calc(tempfil,min)
# zapis rastra
writeRaster(mintemp, filename = paste0("/home/wh/tsk/tmin_dobowe/", 
                                       patt, "_tsk_tmin.tif"),
            overwrite=TRUE)

##########################################################################
# teraz czas na policzenie liczby godzin w dobie
rc = reclassify(tempfil, c(-Inf,273.15,1, 273.15,+Inf,0))
liczba_godzin = calc(x = rc, sum)
# TODO:
# zapis jako geotif
writeRaster(liczba_godzin, filename = paste0("/home/wh/tsk/czas_przymrozku_dobowo/", 
                                             patt, "_tsk_czas_przymrozku.tif"),
            overwrite=TRUE)


##########################################################################
## i na koniec zliczenie skumulowanej liczby dni na plusie
###############################################
## plan jest taki, zeby liczyc to na podstawie dobowek

setwd("/home/wh/tsk/tmin_dobowe/")
rok = format(Sys.Date()-1, "%Y")
files = dir(pattern = as.character(rok))
r = stack(files)
rc = reclassify(r, c(-Inf,273.15,1, 273.15,+Inf,0))
doy = rc # empty raster for doy
for (i in 1:length(names(doy)))    doy[[i]][] = i
doy = rc * doy
  
# ostatni mroz mozna policzyc jako:
maksy = calc(doy, max)
    # usuniecie zer glownie dla baltyku:
maksy[maksy==0] = NA
    # odejmujemy nr dzisiejszego dnia:
ostatni_mroz = i-maksy
#plot(ostatni_mroz)
writeRaster(ostatni_mroz, filename = paste0("/home/wh/tsk/ostatni_mroz/", 
                                             patt, "_tsk_ostatni_mroz.tif"),
            overwrite=TRUE)


######### ROZPOCZECIE RYSOWANEK: ##########

# Creating figure 4 temp
# color scale

tempcolores<- c("#f6c39f","#e3ac89","#cb9881","#b58575","#9c716e","#865c62","#704754",
                "#57344a","#3f1f3f","#240d2b","#260225","#2e0331","#370938","#420a40",
                "#431243","#481046","#571658","#5e185e","#5f1b60","#671e67","#6d2069",
                "#853a85","#964299","#9a559d","#a665a3","#ae74a9","#b485b3","#ba93b9",
                "#c6a5c5","#cbb4cb","#d3c1d2","#c3cad5","#b6b7c6","#9ca4b9","#8992b0",
                "#5e689b","#5e699d","#48528f","#374182","#1d2e77","#0b1761","#162e74",
                "#234080","#37578c","#456f9a","#5a88ab","#78b2c4","#9fdbdc","#b1f0ee",
                "#83c9a7","#72c29a","#67b78c","#69ba8f","#61b080","#56a573","#4c9d64",
                "#3a9152","#368a45","#2a7f39","#2b7234","#1d681c","#29741a","#44851e",
                "#578c25","#759c2b","#84a935","#afbf3c","#d8d952","#d4d755","#efe362",
                "#e9d04f","#e1b845","#d9a53f","#c68f3d","#cc8c38","#c27b31","#ba6323",
                "#b74d22","#ac4e28","#9f2715","#7b1b11","#80110c","#741105","#6f0d07",
                "#630c06","#5a0c0c","#540904","#4b0504","#400401","#3f0101","#2d0708",
                "#442321","#583e3a","#6f5652","#866e6a","#9c8982","#b2a59c","#c8bcb1",
                "#c9bdb1","#ddd5c9","#f5efe3","#f4efe3")



temperatura_map<- function(input="inp", output="outp", title = "tytul"){
  
  obj1<- mask(input-273.15, pol)
  centroidy$column <- sprintf(round(as.vector(raster::extract(obj1, centroidy)),1),fmt = '%#.1f')
  breaks <-round(seq(-28, 35, length.out = length(tempcolores)),1)
  range_min <- floor(quantile(obj1, p=0.01))
  range_max <- ceiling(max(maxValue(obj1)))
  
  ind <- which(breaks> range_min & breaks < range_max)
  breaks2 <- round(breaks[ind],1)
  tempcolores2 <- tempcolores[ind[-length(ind)]]
  
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
    tm_credits("esusza.pl\n(c) WIND-HYDRO 2020", position = c("left", "bottom"), 
               size = 0.35, bg.color = "white")
}

www_path <- gsub(x = pathway, pattern = "wrfprd","www")
dir.create(www_path)

p <- temperatura_map(input=mintemp, title = paste0("Minimalna dobowa temperatura gruntu [°C] \n", patt , " (00-23 UTC)"))

# ciecie w inner margins: dol, lewa, gora, prawa
tmap_save(p + tm_layout(inner.margins = c(-0.1, -0.06, -0.1, -0.1)), 
          filename = paste0(www_path, "/tsk_min_",patt,".png"), width=1000, height=1300)
tmap_save(p + tm_layout(inner.margins = c(-0.1, -0.06, -0.1, -0.1)), 
          filename = paste0("/home/wh/tsk/tmin_dobowe_png/", patt, "_tsk_tmin.png"), width=1000, height=1300)

#writeRaster(mintemp-273.15, filename = paste0(www_path,"/tsk_tmin_",patt,".tif"),overwrite=TRUE)

# dorzucenie kodu do plotowania rastrow godzinowych:
nazwy <- gsub( x = gsub(x = gsub(x = names(tempfil), pattern = "wrfout_d03_", ""), pattern = "00_TSK_", ""), pattern = "_", " ")
fname <- gsub(x = gsub(nazwy, pattern = " ", replacement = ""), pattern = '.', replacement = "", fixed=T)
for (i in 1:length(names(tempfil))){
  
  p <- temperatura_map(input=tempfil[[i]], title = paste0("Temperatura gruntu  [°C] \n", nazwy[i], "UTC"))
  
  tmap_save(p + tm_layout(inner.margins = c(-0.1, -0.06, -0.1, -0.1)), 
            filename = paste0(www_path, "/tsk_",fname[i],".png"), width=1000, height=1300)
}

#######
##### Rysowanki dla dnia z ostatnim mrozem

library(fields)
SKALA=c('purple4','lightblue4','mediumseagreen','yellow2','orange','darkorange2','red','darkred','maroon','hotpink')
rbPal <- colorRampPalette(SKALA)
tempcolores = rbPal(91)

temperatura_map<- function(input="inp", output="outp", title = "tytul"){
  
  obj1<- mask(input, pol)
  centroidy$column <- round(as.vector(raster::extract(obj1, centroidy)))
  breaks2 <-round(seq(0, 90, length.out = length(tempcolores)),1)
  
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
      space.color="grey90"
      #legend.format = list(text.separator = " ", format = formatC("f"))
    )+
    
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
    tm_credits("esusza.pl\n(c) WIND-HYDRO 2020", position = c("left", "bottom"), 
               size = 0.35, bg.color = "white")
}

  p = temperatura_map(input=ostatni_mroz, title = paste0("Liczba dni od ostatniego przymrozku\n", patt))
  tmap_save(p,
    filename = paste0("/home/wh/tsk/ostatni_mroz_png/", 
                      patt, 
                      "_tsk_ostatni_mroz.png"), 
    width=1000, height=1300)

  
  
###############################
### i na koniec rysowanki dla czasu przymrozku:

SKALA=c('white', "lightblue", 'blue', "darkblue", "violet")
rbPal <- colorRampPalette(SKALA)
tempcolores = rbPal(24)

  
  temperatura_map = function(input="inp", output="outp", title = "tytul"){
    
    obj1<- mask(input, pol)
    
    centroidy$column <- round(as.vector(raster::extract(obj1, centroidy)))
    
    breaks2 <-round(seq(0, 24, length.out = length(tempcolores)),1)
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
        space.color="grey90"
        #legend.format = list(text.separator = " ", format = formatC("f"))
      )+
      
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
      tm_credits("esusza.pl\n(c) WIND-HYDRO 2020", position = c("left", "bottom"), 
                 size = 0.35, bg.color = "white")
    
  }
  

    p = temperatura_map(input=liczba_godzin, title = paste0("Czas przymrozku w ciągu doby (h)\n", patt))
    tmap_save(p,
      filename = paste0("/home/wh/tsk/czas_przymrozku_dobowo_png/", 
                        patt, 
                        "_tsk_czas_przymrozku.png"), 
      width=1000, height=1300)

    
# koniec, dymbiec, pentla!