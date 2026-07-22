load_rda <- function(x) {
  load(x)
  cru <- substr(x, 16, 26)
  out <- tibble::add_column(dataset, .before = 1, cruise = cru)
  return(out)
}
`%>%` <- magrittr::`%>%`
#zu wenig speicher
# memory.limit(size = 8000) #=8000MB RAM

load('data/OWF_positions.rda')


#HE466
files <- list.files('data/output_AE/', pattern = 'HE466', full.names = T)[-c(2,5:7)]
dat <- purrr::map_dfr(files, load_rda)
head(dat)

test <- dat[dat$cruise=='HE466_H05T1' & round(dat$Section_Distance_grid_km) == 47 | dat$cruise=='HE466_H05T1' & round(dat$Section_Distance_grid_km) == 62 |
              dat$cruise=='HE466_H05T2' & round(dat$Section_Distance_grid_km) == 95 | dat$cruise=='HE466_H05T2' & round(dat$Section_Distance_grid_km) == 110 |
              dat$cruise=='HE466_H05T3' & round(dat$Section_Distance_grid_km) == 136 | dat$cruise=='HE466_H05T3' & round(dat$Section_Distance_grid_km) == 152 |
              dat$cruise=='HE466_H06T3' & round(dat$Section_Distance_grid_km) == 137 | dat$cruise=='HE466_H06T3' & round(dat$Section_Distance_grid_km) == 150 |
              dat$cruise=='HE466_H07T3' & round(dat$Section_Distance_grid_km) == 100 | dat$cruise=='HE466_H07T3' & round(dat$Section_Distance_grid_km) == 115,]
test <- dplyr::group_by(test, cruise, round(Section_Distance_grid_km)) %>%
  dplyr::summarise(lat = mean(Lat_N), lon = mean(Lon_E))
test$lat[!grepl(pattern = 'H05', x = test$cruise)] <- test$lat[!grepl(pattern = 'H05', x = test$cruise)] + .03
pDat <- dplyr::distinct(dat, cruise, Lat_N, Lon_E)
pDat$Lat_N[!grepl(pattern = 'H05', x = pDat$cruise)] <- pDat$Lat_N[!grepl(pattern = 'H05', x = pDat$cruise)] + .03
pDat$cruise <- as.factor(pDat$cruise)

myColors <- c('lightgreen', 'green', 'yellowgreen', 'lightblue', 'orange')
names(myColors) <- levels(pDat$cruise)

p <- ggplot2::ggplot()+
  ggplot2::geom_point(ggplot2::aes(x = Lon_E, y = Lat_N,colour=cruise),pDat)+
  ggplot2::geom_point(ggplot2::aes(y = WEA.PositionsLAT, x = WEA.PositionsLON),WEA.Positions,
                      colour = 'red')+
  ggplot2::geom_point(ggplot2::aes(x = lon, y = lat),test)+
  ggplot2::labs(x = 'Longitude [°E]', y = 'Latitude [°N]')+
  ggplot2::scale_color_discrete(name = 'Transect', palette = myColors)
p
save(p, file = 'figs/transectsHE466.rda')


#HE490
files <- list.files('data/output_AE/', pattern = 'HE490', full.names = T)
dat <- purrr::map_dfr(files, load_rda)
head(dat)

test <- dat[round(dat$Section_Distance_grid_km) == 52 | round(dat$Section_Distance_grid_km) == 61,]
test <- dplyr::group_by(test, cruise, round(Section_Distance_grid_km)) %>%
  dplyr::summarise(lat = mean(Lat_N), lon = mean(Lon_E))
# test$lat[!grepl(pattern = 'H05', x = test$cruise)] <- test$lat[!grepl(pattern = 'H05', x = test$cruise)] + .03
pDat <- dplyr::distinct(dat[dat$Section_Distance_grid_km>40,], cruise, Lat_N, Lon_E)
# pDat$Lat_N[!grepl(pattern = 'H05', x = pDat$cruise)] <- pDat$Lat_N[!grepl(pattern = 'H05', x = pDat$cruise)] + .03
p <- ggplot2::ggplot()+
  ggplot2::geom_point(ggplot2::aes(x = Lon_E, y = Lat_N),pDat, colour = 'darkgray')+
  ggplot2::geom_point(ggplot2::aes(y = WEA.PositionsLAT, x = WEA.PositionsLON),WEA.Positions,
                      colour = 'red')+
  ggplot2::geom_point(ggplot2::aes(x = lon, y = lat),test)+
  ggplot2::labs(x = 'Longitude [°E]', y = 'Latitude [°N]')+
  ggplot2::scale_color_discrete(name = 'Transect')
p
save(p, file = 'figs/transectsHE490.rda')




#HE496
files <- list.files('data/output_AE/', pattern = 'HE496', full.names = T)
dat <- purrr::map_dfr(files, load_rda)
head(dat)

test <- dat[dat$cruise=='HE496_H03T4' & round(dat$Section_Distance_grid_km) == 153 | dat$cruise=='HE496_H03T4' & round(dat$Section_Distance_grid_km) == 165 |
              dat$cruise=='HE496_H03T5' & round(dat$Section_Distance_grid_km) == 191 | dat$cruise=='HE496_H03T5' & round(dat$Section_Distance_grid_km) == 203 |
              dat$cruise=='HE496_H03T6' & round(dat$Section_Distance_grid_km) == 237 | dat$cruise=='HE496_H03T6' & round(dat$Section_Distance_grid_km) == 250,]
test <- dplyr::group_by(test, cruise, round(Section_Distance_grid_km)) %>%
  dplyr::summarise(lat = mean(Lat_N), lon = mean(Lon_E))
# test$lat[!grepl(pattern = 'H05', x = test$cruise)] <- test$lat[!grepl(pattern = 'H05', x = test$cruise)] + .03
pDat <- dplyr::distinct(dat, cruise, Lat_N, Lon_E)
# pDat$Lat_N[!grepl(pattern = 'H05', x = pDat$cruise)] <- pDat$Lat_N[!grepl(pattern = 'H05', x = pDat$cruise)] + .03
pDat$cruise <- as.factor(pDat$cruise)
p <- ggplot2::ggplot()+
  ggplot2::geom_point(ggplot2::aes(x = Lon_E, y = Lat_N, colour = cruise),pDat)+
  ggplot2::geom_point(ggplot2::aes(y = WEA.PositionsLAT, x = WEA.PositionsLON),WEA.Positions,
                      colour = 'red')+
  ggplot2::geom_point(ggplot2::aes(x = lon, y = lat),test)+
  ggplot2::labs(x = 'Longitude [°E]', y = 'Latitude [°N]')+
  ggplot2::scale_color_discrete(name = 'Transect')
p
save(p, file = 'figs/transectsHE496.rda')

