# memory.limit(8000) #=8000MB RAM
`%>%` <- magrittr::`%>%`

# load('data/wind_data.rda')
# names(wind_data)

# wind_data$pro_dp <- wind_data$uvmag >= 5 & wind_data$uvmag < 10 & wind_data$uvdir > 0 & wind_data$uvdir < 90
# head(wind_data)
# range(wind_data$Year)

# test <- dplyr::summarise(dplyr::group_by(wind_data, lon, lat, Month),
#                          dp = sum(diPOS, na.rm = TRUE) /
#                            (sum(diPOS, na.rm = TRUE)+sum(natMIX, na.rm = TRUE)+1e-7),
#                          dp_abs = sum(diPOS, na.rm = TRUE))
# save(test, file = 'data/windYear.rda')
load('data/windYear.rda')
head(test)

#find areas shallower 20m
# c = marmap::getNOAA.bathy(lon1 = 5, lon2 = 15, lat1 = 53, lat2 = 56,
#                           resolution = 3, keep = T, path = 'data/')
# cf = marmap::fortify.bathy(c)
# cf <- dplyr::mutate(dplyr::group_by(cf, z), water_depth = floor(z / 10) * 10)
# cf <- cf[cf$water_depth < 0 & cf$water_depth>=-20,]
# cf$x <- floor(cf$x*100/5)*5/100 #round to .05
# cf <- dplyr::group_by(cf, x) %>%
#   dplyr::summarise(y = max(y))
# 
# test <- dplyr::left_join(test, cf, by = c('lon' = 'x'))
# test$dp[test$lat < test$y] <- NA

p <- ggplot2::ggplot()+
  ggplot2::geom_raster(ggplot2::aes(x = lon,
                                    y = lat,
                                    fill = pmin(dp, .2)), test)+
  ggplot2::scale_fill_gradient2(midpoint = .1,
                               breaks = c(0, .05, .1, .15, .2),
                               labels = c('0', '5', '10', '15', '>20'),
                               limits = c(0, .2),
                               name = '% dipole-induced mixing')+
  ggplot2::annotation_map(ggplot2::map_data("world"))+
  ggplot2::coord_quickmap(xlim=c(5,7),ylim=c(53,55))+
  ggplot2::facet_wrap(~Month, ncol = 3)+
  ggplot2::theme(legend.position = 'bottom')+
  ggplot2::labs(x='Longitude [°E]', y = 'Latitude [°N]')+
  ggplot2::scale_x_continuous(labels = c('5', '', '6', '', '7'))
save(p, file = 'figs/wind.rda')


# test <- dplyr::summarise(dplyr::group_by(wind_data[wind_data$Month==9,], lon, lat, Year),
#                          dp = sum(diPOS, na.rm = TRUE) /
#                            (sum(diPOS, na.rm = TRUE)+sum(natMIX, na.rm = TRUE)+1e-7),
#                          dp_abs = sum(diPOS, na.rm = TRUE))
# save(test, file = 'data/windMonth.rda')
load('data/windMonth.rda')
head(test)

# test <- dplyr::left_join(test, cf, by = c('lon' = 'x'))
# test$dp[test$lat < test$y] <- NA

p <- ggplot2::ggplot()+
  ggplot2::geom_raster(ggplot2::aes(x = lon, 
                                    y = lat, 
                                    fill = pmin(dp, .2)), test)+
  ggplot2::scale_fill_gradient2(midpoint = .1,
                                breaks = c(0, .05, .1, .15, .2),
                                labels = c('0', '5', '10', '15', '>20'),
                                limits = c(0, .2),
                                name = '% dipole-induced mixing')+
  ggplot2::annotation_map(ggplot2::map_data("world"))+
  ggplot2::coord_quickmap(xlim=c(5,7),ylim=c(53,55))+
  ggplot2::facet_wrap(~Year, ncol = 10)+
  ggplot2::theme(legend.position = 'bottom')+
  ggplot2::labs(x='Longitude [°E]', y = 'Latitude [°N]')+
  ggplot2::scale_x_continuous(labels = c('5', '', '6', '', '7'))
save(p, file = 'figs/wind_july.rda')


# check with NOA - nothing
# test <- dplyr::summarise(dplyr::group_by(wind_data, lon, lat, Year, Month),
#                          dp = sum(diPOS, na.rm = TRUE) /
#                            (sum(diPOS, na.rm = TRUE)+sum(natMIX, na.rm = TRUE)+1e-7))
# test <- test[test$Year>2000 & test$Year<2016,]
# head(test)
# 
# noa <- read.csv(file = 'data/NOA_2001_2015_monthly.csv')
# names(noa) <- c('year', 1:12)
# noa <- tidyr::gather(noa, key = month, value = noa, '1':'12')
# noa$month <- as.numeric(noa$month)
# test <- dplyr::left_join(test, noa, by = c('Year' = 'year', 'Month'='month'))
# 
# plot(test$dp, test$noa)


dummy <- wind_data[wind_data$Year == 2002 &
                      wind_data$Month == 05 &
                      wind_data$Day == 10 &
                      wind_data$Time == '02:00:00' &
                      wind_data$pro_dp,
                    ]
range(dummy$uvdir)
title <- paste0(dummy$Year[1], '-', dummy$Month[1], '-', dummy$Day[1], ', ', dummy$Time)
ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = lon , 
                                    y = lat, 
                                    fill = uvmag), dummy) +
  ggplot2::geom_spoke(ggplot2::aes(x = lon , 
                                   y = lat, 
                                   angle = uvdir/180*pi),#in radians not degree!
                      arrow = grid::arrow(length = grid::unit(.05, 'inches')),
                      radius = scales::rescale(dummy$uvmag, c(.1, .1)),
                      dummy) + 
  ggplot2::scale_fill_distiller(palette = "RdYlGn") + 
  ggplot2::coord_equal(expand = 0) + 
  ggplot2::theme(legend.position = 'bottom',
                 legend.direction = 'horizontal')+
  ggplot2::ggtitle(title)


# #merge year month day time
# wind_data$date <- paste(wind_data$Year, wind_data$Month, wind_data$Day, wind_data$Time, sep = '-')
# 
# #dimnames for array
# xn <- unique(wind_data$lon)
# yn <- unique(wind_data$lat)
# zn <- unique(wind_data$date)
# #array dims
# ad <- c(length(xn), length(yn), length(zn))
# windarray <- array(data = wind_data$pro_dp, dim = ad, dimnames = list(xn, yn, zn))
# 
# # windarray[,,1]+windarray[,,2]
# # sum(windarray[,,1:2])
# # apply(windarray[,,1:5], MARGIN = c(1,2), FUN = function(x){sum(x) / 5})
# 
# windarray_new <- array(dim = ad, dimnames = list(lon = xn, lat = yn, date = zn))
# windarray_new[,,1]
# 
# for (z in 10:length(zn)) {
#   windarray_new[,,z] <- apply(windarray[,,(z-9):z], MARGIN = c(1,2), FUN = function(x){sum(x, na.rm = T) / 10})
# }
# 
# windarray_new[,,11]
# 
# 
# test <- reshape2::melt(windarray_new, id = 'lon', value.name = 'pro_dp')
# head(test)
# test$pro_dp[10:20]
# 
# View(dplyr::summarise(dplyr::group_by(test, date), x = sum(pro_dp, na.rm = T)))
# 
# dp_pro <- tidyr::separate(test, col = date, into = c('Year', 'Month', 'Day', 'Time'), sep = '-')
# wind_data$dp_pos <- dp_pro$pro_dp #% wieviele der letzen 10 stunden erfüllen die bedingungen für einen dipol
# save(wind_data, file = 'wind_data.rda')

dummy <- wind_data[wind_data$Year == 2019 &
                      wind_data$Month == 05 &
                      wind_data$Day == 16 &
                      wind_data$Time == '01:00:00',
]

load('OWF_positions.rda')

title <- paste0(dummy$Year[1], '-', dummy$Month[1], '-', dummy$Day[1], ', ', dummy$Time)
ggplot2::ggplot() +
  ggplot2::geom_raster(ggplot2::aes(x = lon , 
                                    y = lat, 
                                    fill = dp_pos), dummy)+
  ggplot2::geom_point(ggplot2::aes(x = WEA.PositionsLON, y = WEA.PositionsLAT), WEA.Positions, colour = 'white', size = 1) +
  ggplot2::geom_spoke(ggplot2::aes(x = lon ,
                                   y = lat,
                                   angle = uvdir/180*pi),#in radians not degree!
                      arrow = grid::arrow(length = grid::unit(.05, 'inches')),
                      radius = scales::rescale(dummy$uvmag, c(.1, .1)),
                      dummy) +
  ggplot2::scale_fill_distiller(palette = "RdYlGn") + 
  ggplot2::coord_equal(expand = 0) + 
  ggplot2::theme(legend.position = 'bottom',
                 legend.direction = 'horizontal')+
  ggplot2::ggtitle(title)


#days with potential dipols:
dummy <- wind_data[wind_data$Year == 2019 &
                      wind_data$Month > 04 &
                      wind_data$Month < 6 &
                      wind_data$Time == '09:00:00' &
                      wind_data$dp_pos > .5,
]

View(dplyr::distinct(dummy, Year, Month, Day),title = 'dipol_days')

