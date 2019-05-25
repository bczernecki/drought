library(raster)
library(dplyr)
library(lubridate)
library(rwrfhydro)
library(rgdal)
library(tmap)
library(rgeos)
library(rgdal)
library(ncdf4)



patt<- "2019-05-20"
pathway <- paste0("/home/bartosz/", gsub(pattern = "-", replacement = "", x = as.Date(patt)-1),"/wrfprd")
#pathway<-"F:\\wrf\\20190519\\wrfprd"


daily_function<- function(input="inp"){


 
 myfunction_for_converting <- function(input = 'sth',  variable = "var"){
    geofile <- input
    proj4<- GetProj(geofile)
    output <- paste0(input, "_", variable, "_.tif")
    ExportGeogrid(inFile = input, inVar = variable, outFile = output)
 }
  
  myfile<- input 
 
  lapply(myfile, function(x) myfunction_for_converting(input = x, variable="T2"))
  lapply(myfile, function(x) myfunction_for_converting(input = x, variable="RAINNC")) # prec
 }


day<- dir(path=pathway , pattern = patt,full.names = T)
day <- as.list(day) 


##########################################################################
# Objects
tempfil<-dir(path=pathway, pattern = "T2", full.names = T)
tempfil<- stack(tempfil[grepl(pattern=patt, tempfil)])
opadfil<- dir(path=pathway,pattern = "RAINNC", full.names = T)
opadfil<-stack(opadfil[grepl(pattern=patt, opadfil)])

meantemp<- calc(tempfil,mean)
sumopad<- opadfil@layers[[24]]

# Creating figure
#color scale

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

opadcolores<-c("#fbfbfb","#d5d4d6","#c1c3be","#b1cdaf","#88bb85","#7eb379","#58b651",
               "#3c9a34","#1f7e19","#35b16a","#2f938e","#2c75a9","#2a6cba","#2051dd",
               "#2034e8","#1a6bf8","#1489f5","#1ea3f9","#13d8f6","#0ff2ff","#6ae695",
               "#abe74c","#fcf71a","#ffd113","#fea622","#f98814","#fe761b","#f96a15",
               "#f85d1c","#fb4c24","#fe2f20","#fe1409","#cd1718","#be1918","#a71717",
               "#961a15","#871c1d","#6b1e22","#8e1962","#b52dad","#cf59ce","#d776d5",
               "#f2a5f1","#ffc7ff","#efeeef","#a3a3a3","#4a4e4d","#4f3933","#887349",
               "#cebb67","#b6a88e","#907792","#6638a7","#5e359b","#802c81","#bb1f57")


proj4<- projection(meantemp)


wojewodztwa <- readOGR("data/POL_adm1.shp")
pol <- readOGR("data/POL_adm0.shp")
rzeki <- readOGR("data/rzekiPL.shp") 
jeziora <- readOGR("data/jeziora.shp")


wojewodztwa <- spTransform(wojewodztwa,proj4)
pol <- spTransform(pol, proj4)
jeziora <- spTransform(jeziora, proj4)
rzeki <- spTransform(rzeki, proj4)


temperatura_map<- function(input="inp", output="outp"){

  obj1<- mask(input-273.15, pol)
  
  centroidy = gCentroid(wojewodztwa,byid=TRUE)
  centroidy$column <- round(as.vector(raster::extract(obj1, centroidy)),1)  

  breaks <-seq(-28, 35, length.out = length(tempcolores))
  
  range_min <- floor(min(minValue(obj1)))
  range_max <- ceiling(max(maxValue(obj1)))
  
  ind <- which(breaks> range_min & breaks < range_max)
  breaks2 <- breaks[ind]
  tempcolores2 <- tempcolores[ind[-length(ind)]]
  
  tm_shape(obj1) +
    tm_raster(title= paste0("Średnia dobowa temperatura powietrza [°C] \n", patt , " (00-23 UTC)"),
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
      legend.width = 3,
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
      legend.format = list(text.separator = " ", format = formatC("d")))+
    
    #Lon/Lat    
    tm_grid(projection = "longlat", x = 10:30, y=40:60, labels.col = "black", col = "gray",lwd = 0.5,
            labels.size = 0.4, labels.inside.frame = T) + 
    #Mean values of counties
    tm_shape(centroidy)+
    tm_text("column", size = 0.7) +   
    
    #Compass
    tm_compass(size = 1, fontsize = 0.7,
               position = c(0.04,0.9), color.light = "grey90") +
    
    tm_scale_bar(width = 0.12,size = 0.35,breaks = c(0,50,100,150), position = c("left","bottom")) +
    
    tm_credits("(c) Wind-Hydro 2019", position = c("left", "bottom"), 
               size = 0.35, bg.color = "white")
    
 }

#png(filename= paste0("Daily mean of temperature ",patt,".png"), width = 1000, height = 800)
p <- temperatura_map(input=meantemp, output="Daily mean of temperature.png")

# ciecie w inner margins: dol, lewa, gora, prawa
tmap_save(p + tm_layout(inner.margins = c(-0.1, -0.06, -0.1, -0.1)), 
              filename = "World_map2.png", width=1000, height=1300)
#dev.off()


figures_prec<- function(input="in", output= "outp"){
 # print(input)
  
  obj2<- mask(input, pol)
  
  centroidy = gCentroid(wojewodztwa,byid=TRUE)
  centroidy$var <- round(extract(obj2, centroidy),1)
  
  breaks <-seq(0, 300, length.out = length(opadcolores))
  range_min <- floor(min(minValue(obj2)))
  range_max <- ceiling(max(maxValue(obj2)))
  
  ind <- which(breaks> range_min & breaks <= range_max)
  breaks2 <- breaks[ind]
  tempcolores2 <- opadcolores[ind[-length(ind)]]

  tm_shape(obj2) +
    tm_raster(title= paste0("Daily sum of precipitation [mm] \n",patt)  ,palette = tempcolores2, breaks=breaks2, 
              legend.is.portrait = FALSE,
              interpolate = T)  +
    
    #Border  
    tm_shape(pol) +
    tm_polygons(alpha = 0.001, lwd=3.5) +
    
    #Border of counties 
    tm_shape(wojewodztwa)+
    tm_polygons(alpha = 0.01, lwd=0.5)+
    
    #Rivers    
    tm_shape(rzeki)+
    tm_lines(col="#2669d6", lwd=1.5) +
    
    #Lakes
    #tm_shape(jeziora)+
    #tm_polygons(col="#2669d6") +
    
    #Mean values of counties
     tm_shape(centroidy)+
     tm_text("var") +   
    
    #Title of the figure
    tm_layout(#title = "2019-06-22",title.size = .75,
      aes.palette = "div",
      sepia.intensity = 0.2,legend.just = "right",title.color = "blue",
      compass.type = "arrow",title.bg.color = "white", title.bg.alpha = 0.5,
      title.position = c(0.02,0.06),
      legend.outside = T,
      legend.outside.position =  "bottom",
      legend.width = 1.0,
      legend.title.size = 1.5,
      legend.text.size = 0.7,
      #legend.position = c("right","bottom"),
      legend.bg.color = "#FFFFFF60",
      legend.height = 2,
      legend.frame.lwd = 1,
      legend.frame = F,
      legend.bg.alpha = 1,
      space.color="grey90",
      legend.format = list(text.separator = " "))+
    #Compass
    tm_compass(position = c("left","top"), color.light = "grey90") +
    
    tm_credits("(c) WindHydro",position = c(.77, .00005), size = 0.55) +
    
    #Lon/Lat    
    tm_grid(projection = "longlat", x = 10:30, y=40:60, labels.col = "black", 
            labels.size = 0.8, labels.inside.frame = T)
}

 png(filename = paste0( "Daily sum of precipitation ",patt,".png"))
 figures_prec(input =sumopad, output = "Daily sum of precipitation.png" )
 dev.off()

 
 