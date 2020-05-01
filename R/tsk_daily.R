##########################################################################
# liczymy minimalna dobowa z poszczegolnych warstw:
library(raster)
library(pbmcapply)
library(parallel)
# wczesniej musimy przeniesc pliki do katalogu godzinowe:
setwd("~/tsk/godzinowe/")
files = dir(pattern = "_tsk.tif", full.names = F, recursive = T)
files[1]

df = seq.Date(from = as.Date("2019-12-31"), to = as.Date("2020-05-01"), by = "day")

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
czas_przymrozku = function(x){
  r = stack(dir(pattern = as.character(x)))
  rc <- reclassify(r, c(-Inf,273.15,1, 273.15,+Inf,0))
  r = calc(x = rc, sum)
  writeRaster(r, filename = paste0("/home/wh/tsk/czas_przymrozku_dobowo/",as.character(x),"_tsk_czas_przymrozku.tif"))
}

pbmclapply(as.list(df), czas_przymrozku, mc.cores = 31)



###############################################
###############################################
# i na koniec skumulowana liczba dni na plusie
# plan jest taki, zeby liczyc to na podstawie dobowek

setwd("/home/wh/tsk/tmin_dobowe/")
files = dir(pattern = "2020")
r = stack(files)
rc <- reclassify(r, c(-Inf,273.15,1, 273.15,+Inf,0))

doy = rc # empty raster for doy

for (i in 1:length(names(doy))){
  print(i)
  doy[[i]][] = i
}

doy = rc * doy

ostatni_mroz = function(x){
  ind = grep(pattern = as.character(x), x = files)
  # mapa na dzis to doy z dzis
  # plot(doy[[ind]])
  # max z wszystkich warstewek od 1 dnia do dzis:
  maksy = calc(doy[[1:ind]], max)
  # usuniecie zer glownie dla baltyku:
  maksy[maksy==0] = NA
  
  # odejmujemy nr dzisiejszego dnia:
  ostatni_mroz = ind-maksy
  ostatni_mroz
  writeRaster(ostatni_mroz, filename = paste0("/home/wh/tsk/ostatni_mroz/",as.character(x),"_tsk_ostatni_mroz.tif"),
              overwrite=TRUE)
}

  #sp::spplot(ostatni_mroz) # czyli ostatni dzien z przymrozkiem

pbmclapply(as.list(df), ostatni_mroz, mc.cores = 31)
