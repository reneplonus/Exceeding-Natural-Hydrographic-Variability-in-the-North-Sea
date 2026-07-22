# path <- "/home/rene/Plankton/SFgridAnalysesExternals/SFgridData"
path <- "/home/rene/Plankton/SFgridCoarseData"
files <- list.files(path, full.names = TRUE)
i <- 10
dat <- R.matlab::readMat(files[i])
str(dat)
depth <- nrow(dat$gridPres)
dat <- data.frame(Lat = rep(as.vector(dat$gridLat), each=depth),
                   Lon = rep(as.vector(dat$gridLon), each=depth),
                   Time = rep(as.vector(dat$gridTime), each=depth),
                   Dist = rep(as.vector(dat$gridEdist), each=depth),
                   # WindSpd = rep(as.vector(dat$WindSpdDS), each=depth),#not in 445!
                   # WindDir = rep(as.vector(dat$WindDirDS), each=depth),#not in 445!
                   Depth = as.vector(dat$gridPres),
                   Temp = as.vector(dat$TempNTC),#in 445 ohne LL
                   Sal = as.vector(dat$Sali),
                   Density = as.vector(dat$Theta),
                   O2Sat = as.vector(dat$O2Sat),
                   OxyMcon = as.vector(dat$OxyMcon),
                   OxyVcon = as.vector(dat$OxyVcon))
ggplot2::ggplot()+
  ggplot2::geom_tile(ggplot2::aes(x = Dist, y = -Depth, fill = Temp), dat)#[!is.na(temp$Temp),], size = 3)

name <- paste0('/media/rene/share/Paper/Dipole/data/gridSFrolf/', substr(files[i], 38, 52), '.rda')
name
# save(dat, file = name)
i <- i+1

