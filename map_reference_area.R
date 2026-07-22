`%>%` <- magrittr::`%>%`
load('data/OWF_positions.rda')
loadGrids <- function(x) {
  load(x)
  nMax <- nchar(x)
  out <- as.data.frame(tibble::add_column(.data = dat, .before = 1, cru = paste0('HE', substr(x, nMax-6,nMax-4))))
  return(out)
}
#entire area
files <- list.files(path = 'data/gridSFrolf', full.names = TRUE)[-c(4, 7, 10)]#319 336 365 werden nicht genutzt
dat <- purrr::map_dfr(files, loadGrids)

nLL <- dplyr::distinct(dat, cru, Dist, .keep_all = T) %>%
  dplyr::group_by(Lon = round(Lon, 1), Lat = round(Lat, 1)) %>%
  dplyr::count()
nLL <- nLL[!is.na(nLL$Lon),]

lat <- dat$Lat
lon <- dat$Lon
dat$Lat <- round(dat$Lat, 1)
dat$Lon <- round(dat$Lon, 1)
dat <- dplyr::left_join(dat, nLL)
dat$Lat <- lat
dat$Lon <- lon


# ovSF <- ggplot2::ggplot()+
#   ggplot2::geom_point(ggplot2::aes(x = Lon, y = Lat), dat)+
#   ggplot2::facet_wrap(~cru)
# save(ovSF, file = 'figs/ovSF.rda')


lon_mi <- 3.5
lon_ma <- 9.
lat_mi <- 53.5
lat_ma <- 56.

c = marmap::getNOAA.bathy(lon1 = lon_mi, lon2 = lon_ma, lat1 = lat_mi, lat2 = lat_ma,
                          resolution = 1)

cf = marmap::fortify.bathy(c)
cf <- dplyr::mutate(dplyr::group_by(cf, z), water_depth = floor(z / 10) * 10)
cf <- cf[cf$water_depth < 0,]
cf$water_depth[cf$water_depth < -61] <- -61

dat <- dplyr::group_by(dat, Lat = round(Lat,2), Lon = round(Lon,2)) %>%
  dplyr::summarise(n = sum(n))


p <- ggplot2::ggplot() +
  #polygon depths
  ggplot2::scale_fill_brewer(name = "Water depth [m]", direction = -1,
                             labels = c("60-50m", "50-40m", "40-30m", "30-20m", "20-10m", "10-0m")) +
  ggplot2::geom_contour_filled(data = cf[,-3],
                               ggplot2::aes(x=x, y=y, z = water_depth), binwidth = 10,
                               color = "white")+
  #black frame around map
  ggplot2::annotate("rect", xmin = lon_mi, xmax = lon_ma, ymin = lat_mi, ymax = lat_ma,
                    fill = NA, colour = "black") +
  #specify region
  ggplot2::coord_quickmap(xlim = c(lon_mi, lon_ma), ylim = c(lat_mi, lat_ma), expand = FALSE) +
  #remove raster
  ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 #make scale bigger
                 axis.ticks = ggplot2::element_line(linewidth = 1),
                 #manipulate labels
                 axis.title = ggplot2::element_text(size = 10, face = "bold"),
                 #same background as land to mask gaps
                 panel.background = ggplot2::element_rect(fill = "grey60"),
                 aspect.ratio = .5,
                 legend.position = 'bottom') +
  #add SF houls
  ggplot2::geom_point(ggplot2::aes(x = Lon, y = Lat, color = log(n)), dat[!is.na(dat$n),])+
  ggplot2::scale_color_gradient(name = 'N', low = "red", high = "green", breaks = c(4,6,8,10), labels = round(exp(c(4,6,8,10))))+
  #owfs
  ggplot2::geom_point(ggplot2::aes(y = WEA.PositionsLAT, x = WEA.PositionsLON),WEA.Positions,
                      colour = 'black')+
  ggplot2::labs(x = "Longitude [°E]", y = "Latitude [°N]")
# save(p, file = 'figs/map.rda')

ggplot2::ggplot()+
  ggplot2::geom_line(ggplot2::aes(x = Section_Distance_km_grid, y = maxD),grid[grid$cru=='HE308',])

bla <- dplyr::left_join(nats, dat[,c(1:5)],by = c('id'='cru','x'='Section_Distance_km_grid'))
# 
ggplot2::ggplot()+
  ggplot2::geom_boxplot(ggplot2::aes(x = q80<var, y = tc),bla[bla$range==10,])

ggplot2::ggplot()+
  ggplot2::geom_point(ggplot2::aes(x = Lon_degE, y = Lat_degN),bla[bla$q80<bla$var,])
