# rysowanie obrazow archiwalnych:
# robimy stacka przykladowego dla jednego roku

# konwersja szejpów:
# load(file = "/home/wh/github/drought/data/gisy.Rdata")
# input = tempfil[[200]]
# crs(input)
# wojewodztwa = spTransform(wojewodztwa, CRSobj = crs(input))
# jeziora = spTransform(jeziora, CRSobj = crs(input))
# pol = spTransform(pol, CRSobj = crs(input))
# rzeki = spTransform(rzeki, CRSobj = crs(input))
# save(centroidy, jeziora, pol, rzeki, wojewodztwa, file = "/home/wh/github/drought/data/gisy2.Rdata")

library(raster)
library(tmap)

# tempfil<-dir(path="/home/wh/tsk/ostatni_mroz", pattern = "2019", full.names = T)
# tempfil<- stack(tempfil)
# input = tempfil[[200]]
load(file = "/home/wh/github/drought/data/gisy2.Rdata")

library(fields)
SKALA=c('purple4','lightblue4','mediumseagreen','yellow2','orange','darkorange2','red','darkred','maroon','hotpink')
#SKALA=c('purple4','blue','royalblue1','yellow','orange','red','black')
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


### poczatek rysowania:
setwd("/home/wh/tsk/ostatni_mroz/")

rysuj = function(plik){
  input = raster(plik)
  patt = substr(plik, 1,10)
  p = temperatura_map(input=input, title = paste0("Liczba dni od ostatniego przymrozku\n", patt))
  # ciecie w inner margins: dol, lewa, gora, prawa
  tmap_save(#p + tm_layout(inner.margins = c(-0.1, -0.06, -0.1, -0.1)), 
    p,
    
            filename = paste0("/home/wh/tsk/ostatni_mroz_png/", 
                              patt, 
                              "_tsk_ostatni_mroz.png"), 
            width=1000, height=1300)
}

# teraz rysuj poszczegolne lata
library(pbmcapply)
for (i in 1998:2020){
  print(i)
  files = dir(pattern = as.character(i))
  pbmclapply(as.list(files), FUN = rysuj, mc.cores = 32)
  niewyszly = which(!file.exists(paste0("/home/wh/tsk/ostatni_mroz_png/",gsub(x = files, "tif", "png"))))
  if(length(niewyszly)>0) lapply(as.list(files[niewyszly]), FUN = rysuj)
}


# tempfil<-dir(path="/home/wh/tsk/ostatni_mroz", pattern = "2019", full.names = T)
# tempfil<- stack(tempfil)
