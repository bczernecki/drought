##########################################################################
# liczymy minimalna dobowa z poszczegolnych warstw:
library(raster)
library(pbmcapply)
library(parallel)
# wczesniej musimy przeniesc pliki do katalogu godzinowe:
setwd("~/tsk/godzinowe/")
files = dir(pattern = "_tsk.tif", full.names = F, recursive = T)
files[1]

df = seq.Date(from = as.Date("1998-01-01"), to = as.Date("2020-05-01"), by = "day")

srednia = function(x){
  r = stack(dir(pattern = as.character(x)))
  r = calc(x = r, fun = min)
  writeRaster(r, filename = paste0("../tmin_dobowe/",as.character(x),"_tsk_tmin.tif"))
}

pbmclapply(as.list(df), srednia, mc.cores = 31)


###############################################
###############################################

### teraz liczenie godzin z przekroczeniem progu:
setwd("/home/wh/tsk/godzinowe/")
df = seq.Date(from = as.Date("1998-01-01"), to = as.Date("2020-05-01"), by = "day")
czas_przymrozku = function(x){
  r = stack(dir(pattern = as.character(x)))
  rc <- reclassify(r, c(-Inf,273.15,1, 273.15,+Inf,0))
  r = calc(x = rc, sum)
  writeRaster(r, 
              filename = paste0("/home/wh/tsk/czas_przymrozku_dobowo/",
                                as.character(x),
                                "_tsk_czas_przymrozku.tif"),
              overwrite = T)
}

pbmclapply(as.list(df), czas_przymrozku, mc.cores = 31)



###############################################
###############################################
# i na koniec skumulowana liczba dni na plusie
# plan jest taki, zeby liczyc to na podstawie dobowek

setwd("/home/wh/tsk/tmin_dobowe/")

dany_rok = function(rok){
  files = dir(pattern = as.character(rok))
  r = stack(files)
  rc <- reclassify(r, c(-Inf,273.15,1, 273.15,+Inf,0))
  
  doy = rc # empty raster for doy

  for (i in 1:length(names(doy))){
    #print(i)
    doy[[i]][] = i
  }

  doy = rc * doy
  
  
  # ostatni mroz mozna nawet policzyc w petli jako:
  
  for (ind in 1:length(files)){
    print(ind)
    maksy = calc(doy[[1:ind]], max)
    
    # usuniecie zer glownie dla baltyku:
    maksy[maksy==0] = NA
    
    # odejmujemy nr dzisiejszego dnia:
    ostatni_mroz = ind-maksy
    ostatni_mroz
    writeRaster(ostatni_mroz, 
                filename = paste0("/home/wh/tsk/ostatni_mroz/",
                                  substr(files[ind],1,10),
                                  "_tsk_ostatni_mroz.tif"),
                overwrite=TRUE)
    
  }
  
  }


pbmclapply(as.list(1998:2019), dany_rok, mc.cores = 31)


pbmclapply(as.list(df), ostatni_mroz, mc.cores = 31)

  

ostatni_mroz = function(x){
  ind = grep(pattern = as.character(x), x = files)
  # mapa na dzis to doy z dzis
  # plot(doy[[ind]])
  # max z wszystkich warstewek od 1 dnia do dzis:
  maksy = calc(doy[[1:ind]], max)
  
}

  #sp::spplot(ostatni_mroz) # czyli ostatni dzien z przymrozkiem

pbmclapply(as.list(df), ostatni_mroz, mc.cores = 31)



# jak dorobic braki plikow?
 # linki = paste0(seq.Date(from = as.Date("1998-01-01"), to = as.Date("2020-05-01"), by = "day"), "_tsk_tmin.tif")
 # df = data.frame(path = linki, jestnadysku = file.exists(linki), stringsAsFactors = F)
 # head(df)
 # table(df$jestnadysku)
# file.copy(from = df$path[which(!df$jestnadysku)-1], to = df$path[which(!df$jestnadysku)])
