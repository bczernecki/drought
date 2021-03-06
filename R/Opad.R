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

mclapply(day, function(x) myfunction_for_converting(input = x, variable="RAINNC"), mc.cores = 24)
mclapply(day2, function(x) myfunction_for_converting(input = x, variable="RAINNC"), mc.cores = 24)
mclapply(day3, function(x) myfunction_for_converting(input = x, variable="RAINNC"), mc.cores = 24)
# powinnismy miec teraz stworzone geotify


##########################################################################
# liczymy srednia z poszczegolnych warstw:
tempfil<-dir(path=pathway, pattern = "RAINNC", full.names = T)
if(length(tempfil[grep(pattern = patt, x = tempfil)])>24) tempfil <- tempfil[grep(pattern = paste0("d03_",patt), x = tempfil)] # i tylko domena 03
#tempfil<- stack(tempfil[grepl(pattern=patt, tempfil)])
meantemp1 <- raster(tempfil[24])
meantemp2 <- raster(tempfil[48])
meantemp3 <- raster(tempfil[72])
meantemp2 <- meantemp2-meantemp1
meantemp3 <- meantemp3-meantemp2
# if(args[2] == 2) meantemp<- tempfil@layers[[48]]
# if(args[2] == 3) meantemp<- tempfil@layers[[72]]



#beginCluster(4)
#meantemp <- clusterR(tempfil, calc, args=list(mean, na.rm=T))
#endCluster()

# Creating figure
#color scale

tempcolores<- c("white", "#ebf7f9", "#d1ecf9", "#c1e1f5", "#a1cde7", "#87c3e2", "#81b9db", "#7b9fc0", "#7794c3",
             "#7685b8", "#737aa8", "#716a9c", "#6a569b", "#6b478b", "#6e3882", "#6a2878", "#52265e",
             "#284c4e", "#2c6546", "#3b7f33", "#508d33", "#54a637", "#63b31c", "#77ba15", "#88c015",
             "#9ec61a", "#b3cf1e", "#bfca3a", "#f8ec3e", "#fed522", "#fdc220", "#fab118", "#fb9c18",
             "#f8891f", "#e76931", "#f35820", "#eb4917", "#e33f1e", "#d73216", "#ce2518", "#cd251c",
             "#c41b18", "#9c3e3b", "#973941", "#8a3144", "#7b2e46", "#792e4c", "#6f2a4b", "#602650",
             "#5c245d", "#752178", "#7f2686", "#883094", "#8d3da3", "#9745b4", "#9f4fbe", "#a759cc",
             "#ad68d4", "#da6ae6", "#e477ea", "#e78bed", "#ee98f1", "#f1a8f2", "#f0b9f7", "#fac7fc", 
             "#f8ddfb", "#dbd9da", "#d0d0d0", "#c4c5c5", "#b9b9b9", "#b0b0b0", "#a3a3a3", "#9a9a9a", 
             "#8e8e8e", "#858585", "#797979", "#6f6f6f", "#666666", "#5a5a5a", "#4c4c4c", "#4e4e4e", 
             "#363636", "#232323", "#1c1613", "#372416", "#46331d", "#544327", "#534025", "#5d4b2b", 
             "#675a35", "#73673c", "#7f7746", "#89834a", "#969153", "#a29e62", "#acaa69", "#b9b872", 
             "#c1c780", "#c6e08b", "#b8da84", "#b2d17a", "#a5c57b", "#9dbf75", "#8eb16c", "#87a96c",
             "#6f9864", "#5c845d", "#5a815d", "#507b5a", "#507b5c", "#447056", "#2d5d4b", "#2e614e", 
             "#2d5d4b", "#23554b", "#214946", "#193841", "#2d3f4c", "#414750", "#574b53", "#6a575d", 
             "#7a5d65", "#8e646a", "#9e6a70", "#c77b7b", "#ec8c87", "#ef8c8c", "#e38685", "#d77e7c", 
             "#cc7674", "#c0706d", "#b76767", "#a6605f", "#8d5350", "#814a4a", "#724042", "#6f4546", 
             "#653d3b", "#633939", "#583634", "#573836", "#583333")


skala=c('white','lightblue4','olivedrab3','yellow2','orange','red','magenta')
color.gradient <- function(x, colors=skala, colsteps=k) {
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min,max2, length.out=colsteps)) ] )
}
rbPal <- colorRampPalette(skala)
tempcolores <- rbPal(87)



temperatura_map<- function(input="inp", output="outp", title = "tytul"){
  
  obj1<- mask(input, pol)
  
  centroidy$column <- sprintf(round(as.vector(raster::extract(obj1, centroidy)),1),fmt = '%#.1f')
  
  breaks <- c(0, 0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.8, 2:10/2, seq(6,49, by=1), seq(50,100,2))
  
  range_min <- 0
  range_max <- ceiling(quantile(obj1, p=0.99))
  
  ind <- which(breaks>= range_min & breaks <= range_max)
  
  # to dla zerowych opadow
  if(length(ind)==1) {
    breaks2 = c(0, 0.001) 
    tempcolores2 = tempcolores[1:2]
  } else {
    breaks2 <- round(breaks[ind],1)
    tempcolores2 <- tempcolores[ind[-length(ind)]]
    }
  
  
  tm_shape(obj1) +
    tm_raster(title= title,
              interval.closure = "left",legend.hist = T,
              palette = tempcolores2, breaks=breaks2, 
              legend.is.portrait = FALSE,
              interpolate = TRUE) +
    
    #Border  
    tm_shape(pol) +
    tm_polygons(alpha = 0.001, lwd=1.5) +
    
    #Border of counties 
    tm_shape(wojewodztwa)+
    tm_polygons(alpha = 0.01, lwd=0.7) +
    
    #Rivers    
    tm_shape(rzeki)+
    tm_lines(col="#2669d6", lwd=1.1) +
    
    #Lakes
    tm_shape(jeziora, lwd=0.5)+
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

p1 <- temperatura_map(input=meantemp1, title = paste0("Suma dobowa opadu [mm] \n", patt , " (00-23 UTC)"))

# ciecie w inner margins: dol, lewa, gora, prawa
tmap_save(p1 + tm_layout(inner.margins = c(-0.1, -0.06, -0.1, -0.1)), 
          filename = paste0(www_path, "/prec_",patt,".png"), width=1000, height=1300)
writeRaster(meantemp1, filename = paste0(www_path,"/prec_",patt,".tif"),overwrite=TRUE)
writeRaster(meantemp1, filename = paste0("/home/wh/opad/opad_",patt,".tif"),overwrite=TRUE)
# 

patt <- as.Date(patt)+1
p2 <- temperatura_map(input=meantemp2, title = paste0("Suma dobowa opadu [mm] \n", patt , " (00-23 UTC)"))

# ciecie w inner margins: dol, lewa, gora, prawa
tmap_save(p2 + tm_layout(inner.margins = c(-0.1, -0.06, -0.1, -0.1)), 
          filename = paste0(www_path, "/prec_",patt,".png"), width=1000, height=1300)
writeRaster(meantemp2, filename = paste0(www_path,"/prec_",patt,".tif"),overwrite=TRUE)
writeRaster(meantemp2, filename = paste0("/home/wh/opad/opad_",patt,".tif"),overwrite=TRUE)

patt <- as.Date(patt)+1
p3 <- temperatura_map(input=meantemp3, title = paste0("Suma dobowa opadu [mm] \n", patt , " (00-23 UTC)"))
# ciecie w inner margins: dol, lewa, gora, prawa
tmap_save(p3 + tm_layout(inner.margins = c(-0.1, -0.06, -0.1, -0.1)), 
          filename = paste0(www_path, "/prec_",patt,".png"), width=1000, height=1300)
writeRaster(meantemp3, filename = paste0(www_path,"/prec_",patt,".tif"),overwrite=TRUE)
writeRaster(meantemp3, filename = paste0("/home/wh/opad/opad_",patt,".tif"),overwrite=TRUE)



# dorzucenie kodu do plotowania rastrow godzinowych:
tempfil<- stack(tempfil)
nazwy <- gsub( x = gsub(x = gsub(x = names(tempfil), pattern = "wrfout_d03_", ""), pattern = "00_RAINNC_", ""), pattern = "_", " ")
fname <- gsub(x = gsub(nazwy, pattern = " ", replacement = ""), pattern = '.', replacement = "", fixed=T)
for (i in 1:(length(names(tempfil))-1)){
  
  p <- temperatura_map(input=tempfil[[i+1]]-tempfil[[i]], title = paste0("Suma opadu [mm] \n", nazwy[i], "UTC"))
  
  tmap_save(p + tm_layout(inner.margins = c(-0.1, -0.06, -0.1, -0.1)), 
            filename = paste0(www_path, "/prec_",fname[i],".png"), width=1000, height=1300)
  
}

