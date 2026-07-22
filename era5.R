path <- '/home/bigdatauser/rene/deeplearning/spyder_proj/OWF_MODIS_P3/ERA WindData/'
files <- list.files(path, pattern = '.nc')

rad2deg <- function(rad) {(rad * 180) / (pi)}

uv2dirmag <- function(u, v, convention = 'from') {
  #convention from: wind current
  # !from: water current
  nmax <- length(u)
  
  uvdir <- vector(mode = 'numeric', length = nmax)
  uvmag <- vector(mode = 'numeric', length = nmax)
  
  if(convention == 'from') {
    fac <- -1
  } else {
    fac <- 1
  }
  
  if(nmax == 1) {
    uvmag <- sqrt(u^2 + v^2)
    
    if(u == 0 & v == 0) {
      uvdir <- 0
    } else {
      uvdir <- 90. - rad2deg(atan2(fac * u, fac * v))
      if(uvdir <= 0) {
        uvdir <- uvdir + 360
      }
    }
  } else {
    uvmag <- sqrt(u^2 + v^2)
    
    uvdir <- 90 - rad2deg(atan2(fac * u, fac * v))
    uvdir[uvdir <= 0] <- uvdir[uvdir <= 0] + 360
    uvdir[u == 0 & v == 0] <- 0
  }
  return(data.frame(uvmag = uvmag, uvdir = uvdir))
}


for (f in 1:length(files)) {
  dat <- ncdf4::nc_open(paste0(path,files[f]))
  names(dat$var)
  
  lon <- dat$var$u10$dim[[1]]$vals
  lat <- dat$var$u10$dim[[2]]$vals
  time <- dat$var$u10$dim[[3]]$vals
  
  
  u <- ncdf4::ncvar_get(dat,"u10")
  v <- ncdf4::ncvar_get(dat,"v10")
  dim(u)
  
  dimnames(u) <- list(lon=lon,lat=lat,time=time)
  u <- reshape2::melt(u,id="lon", value.name = 'u')
  dimnames(v) <- list(lon=lon,lat=lat,time=time)
  v <- reshape2::melt(v,id="lon", value.name = 'v')
  
  dirmag <- uv2dirmag(u$u, v$v)
  
  data <- dplyr::left_join(u, v)
  data <- cbind(data, dirmag)
  str(data)
  
  # extract time information
  days.since.1900 <- data$time / 24
  days.since.1900
  
  year.2digits <- trunc((days.since.1900/365),0)    
  year.2digits
  
  year <- 1900 + year.2digits
  year
  
  day.in.current.year <- days.since.1900 - (year.2digits * 365)
  day.in.current.year
  
  # iN R dates are represented as the number of days since 1970-01-01, with negative values for earlier dates
  secs.since.1900 <- days.since.1900 * 86400 +86400 # 86400 seconds in a day
  
  DateTime <- as.POSIXct(secs.since.1900,origin = '1900-01-01', tz = 'UTC')
  DateTime  # "2014-07-01 12:00:00 UTC"
  
  data$time <- DateTime
  data <- tidyr::separate(data, time, c('Date', 'Time'), ' ')
  data <- tidyr::separate(data, Date, c('Year', 'Month', 'Day'), '-')
  
  fe <- file.exists('era5_05_09.txt')
  write.table(data, file = 'era5_05_09.txt', append = fe, row.names = F, col.names = !fe)
}

