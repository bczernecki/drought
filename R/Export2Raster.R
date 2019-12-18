Export2Raster = function (inFile, inVar = "SWDOWN", outFile, inCoordFile = NA, inLyr = NA, 
          inLyrPos = 2, inProj4 = NA) {
  
  # inFile = ("/media/wh/dysk12/wrfout/20191217/wrfprd/wrfout_d03_2019-12-20_12:00:00")
  # inVar = "SWDOWN"
  # inLyrPos = 2
  # inLyr = NA
  # inCoordFile = NA
  # inProj4 = NA
  # outfile = 'sth2.tif'
  inNC <- tryCatch(suppressWarnings(ncdf4::nc_open(inFile)), 
                   error = function(cond) {
                     message(cond)
                     return(NA)
                   })
  if (!all(is.na(inNC))) {
    inNCVar <- ncdf4::ncvar_get(inNC, inVar)
    if (!is.na(inLyr)) {
      if (inLyrPos == 2) {
        inNCVar <- inNCVar[, inLyr, ]
      }
      else if (inLyrPos == 1) {
        inNCVar <- inNCVar[inLyr, , ]
      }
      else if (inLyrPos == 3) {
        inNCVar <- inNCVar[, , inLyr]
      }
    }
    varList <- names(inNC$var)
  }
  
  typlist <- list(byte = "Byte", short = "Int16", int = "Int32", 
                  long = "Int32", float = "Float32", real = "Float32", 
                  double = "Float64", ubyte = "qmin_cfs", ushort = "UInt16", 
                  uint = "UInt32", int64 = "Int64", uint64 = "UInt64")
  if (is.na(inCoordFile)) {
    coordNC <- inNC
    coordvarList <- varList
  }  else {
    coordNC <- ncdf4::nc_open(inCoordFile)
    coordvarList <- names(coordNC$var)
  }
  
  # coordinates for XLONG and XLAT:
    inNCLon <- ncdf4::ncvar_get(coordNC, "XLONG")
    inNCLat <- ncdf4::ncvar_get(coordNC, "XLAT")
  
  
  
  x <- as.vector(inNCLon[, ncol(inNCLon):1])
  y <- as.vector(inNCLat[, ncol(inNCLat):1])
  coords <- as.matrix(cbind(x, y))
  map_proj <- ncdf4::ncatt_get(coordNC, varid = 0, attname = "MAP_PROJ")$value
  cen_lat <- ncdf4::ncatt_get(coordNC, varid = 0, attname = "CEN_LAT")$value
  cen_lon <- ncdf4::ncatt_get(coordNC, varid = 0, attname = "STAND_LON")$value
  truelat1 <- ncdf4::ncatt_get(coordNC, varid = 0, attname = "TRUELAT1")$value
  truelat2 <- ncdf4::ncatt_get(coordNC, varid = 0, attname = "TRUELAT2")$value
  if (is.na(inProj4)) {
    if (map_proj == 1) {
      geogrd.proj <- paste0("+proj=lcc +lat_1=", truelat1, 
                            " +lat_2=", truelat2, " +lat_0=", cen_lat, " +lon_0=", 
                            cen_lon, " +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs")
    }
    else {
      stop("Error: Projection type not supported (currently this tool only works for Lambert Conformal Conic projections).")
    }
  }
  
  dx <- ncdf4::ncatt_get(coordNC, varid = 0, attname = "DX")$value
  dy <- ncdf4::ncatt_get(coordNC, varid = 0, attname = "DY")$value
  if (dx != dy) {
    stop(paste0("Error: Asymmetric grid cells not supported. DX=", 
                dx, ", DY=", dy))
  }
  projcoords <- rgdal::project(coords, geogrd.proj)
  ul <- projcoords[1, ]
  gt0 = ul[1] - dx/2
  gt1 = dx
  gt2 = 0
  gt3 = ul[2] + dy/2
  gt4 = 0
  gt5 = -dy
  gt = c(gt0, gt1, gt2, gt3, gt4, gt5)
  d.drv <- new("GDALDriver", "GTiff")
  if (!all(is.na(inNC))) {
    typ <- typlist[[inNC$var[[inVar]]$prec]]
  }
  
  tds.out <- new("GDALTransientDataset", driver = d.drv, rows = dim(inNCVar)[2], 
                 cols = dim(inNCVar)[1], bands = 1, type = typ)
  .Call("RGDAL_SetGeoTransform", tds.out, gt, PACKAGE = "rgdal")
  .Call("RGDAL_SetProject", tds.out, geogrd.proj, PACKAGE = "rgdal")
  inNCVarRast <- raster::as.matrix(raster::raster(inNCVar))
  inNCVarRast <- inNCVarRast[, ncol(inNCVarRast):1]
  inNCVarRast[is.na(inNCVarRast)] <- NaN
  rgdal::putRasterData(tds.out, as.matrix(inNCVarRast))
  rgdal::saveDataset(tds.out, outFile)
  rgdal::GDAL.close(tds.out)
  if (!all(is.na(inNC))) ncdf4::nc_close(inNC)
  if (!is.na(inCoordFile))   ncdf4::nc_close(coordNC)
}
# 
# Export2Raster(inFile = "/media/wh/dysk12/wrfout/20191217/wrfprd/wrfout_d03_2019-12-20_12:00:00",
#               outFile = "sth3.tif", inVar = "SWDOWN"
#                 )
