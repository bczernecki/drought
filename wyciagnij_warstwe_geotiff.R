
setwd('/home/wh/github/gridowanie/data/output')
files <- dir()


for (f in files){
  print(f)
a=readRDS(f)

df <- data.frame(RMSEk1 = a$RMSE_k1,
           RMSEk2 = a$RMSE_k2, RMSEk3 = a$RMSE_k3)
df[is.na(df)] <- 0

df$min <- apply(df, 1, which.min)
df$param <- a$my_var
df

table(df$min)
head(df)

df$date <- a$my_date
df$filename <- paste0(df$param, '_', df$date,'.tif')

head(df)
for (i in 1:nrow(df)){
  model <- df$min[i]
  if(model == 1) tmp <- a$pred_k1[[i]]
  if(model == 2) tmp <- a$pred_k2[[i]]
  if(model == 3) tmp <- a$pred_k3[[i]]
  
  writeRaster(x = tmp, filename = paste0('/home/wh/nas/06_IGIK_GeoTIFF/02_Dane_WRF_uporzadkowane/',df$filename[i]), overwrite=T)
  rm(tmp)
  print(df$filename[i])
}



} # koniec petli dla lat

