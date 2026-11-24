plotDipole <- function(section, temp, minT, maxT) {
  return(ggplot2::ggplot()+
           # ggplot2::geom_tile(ggplot2::aes(x = Section_Distance_grid_km, y = -Depth_grid_m, fill = chlorophyll_a_rfu),dataset)+
           ggplot2::geom_tile(ggplot2::aes(x = Dist, y = -Depth, fill = pmax(pmin(Temp,maxT),minT)),section[section$cat>=1,])+
           ggplot2::geom_tile(ggplot2::aes(x = Dist, y = -Depth, fill = pmax(pmin(Temp,maxT),minT)),section[section$cat<1,],alpha=.6)+
           # ggplot2::geom_line(ggplot2::aes(x = Section_Distance_grid_km[1]+x, y = -10,
           #                                 colour = pmax(pmin(var,4),1)), dipo,linewidth = 5)+
           ggplot2::geom_line(ggplot2::aes(x = Dist, y = -tc, colour = cat),temp, linewidth = 1, show.legend = T)+
           ggplot2::scale_fill_gradientn(colours = c("blue", "orange"),
                                         values = scales::rescale(c(minT, maxT)),
                                         guide = "colorbar", limits=c(minT,maxT),
                                         name = 'Temperature [°C]')+
           ggplot2::scale_colour_gradient2(midpoint = .99,
                                           low = "white",
                                           mid = "grey",
                                           high = "black",
                                         # breaks = c(0, .95, 1, 1.05, 2),
                                         # labels = c('0', '5', '10', '15', '>20'),
                                         # limits = c(0., 2.),
                                         name = expression(I[EE]))+
           # ggplot2::scale_color_continuous(name = 'Var',
           #                                 guide = "colorbar", limits=c(1,4),
           #                                 labels = c('<1', '2', '3', '4<'))+
           # ggplot2::geom_vline(xintercept = xInter, linetype = 2)+
           ggplot2::labs(x = 'Distance along the transect [km]', y = 'Depth [m]')+
           ggplot2::theme(legend.position = "bottom"))
}


files <- list.files('/data/gridSFrolf', full.names = T)[-c(4,7,10)]

load(files[5])

ggplot2::ggplot()+
  ggplot2::geom_point(ggplot2::aes(x = Lon, y = Lat, colour = Dist),dat[dat$Dist>850&dat$Dist<900,])


dataset <- dplyr::mutate(dplyr::group_by(dat, Dist),
                         dT = max(Temp, na.rm = T) - min(Temp, na.rm = T))  #maximaler temperaturunterschied
temp <- dataset[dataset$dT > .5, c(4,7,10,14)]
temp <- temp[order(temp$Dist, temp$Depth),]
temp <- temp[temp$Depth >= 5 & temp$Depth <= 35,]
temp <- dplyr::summarise(dplyr::group_by(temp, Dist), tc = which.max(diff(Density))+5)

#empty vector to store results
l <- nrow(temp)
dummy <- rep(NA, times = l)
#indicees for x > range
ix <- which.max(temp$Dist > temp$Dist[1]+10):l
#standard deviation of 'tc' within  a window of 'range' km
var_ <- purrr::map_dbl(ix, ~sd(temp$tc[temp$Dist >= temp$Dist[.] - 10 & temp$Dist <= temp$Dist[.]], na.rm = TRUE))

dummy[ix] <- var_

temp$var <- dummy
temp$q80 <- quantile(temp$var, .8, na.rm = TRUE)
temp$cat <- temp$var / temp$q80
# temp$cat[temp$cat > 1] <- 2
# temp$cat[temp$cat < 1 ] <- 0

# x <- 930 #gut
x <- 850 #besser
# x <- 1001
section <- dat[dat$Dist>x&dat$Dist<(x+50),]
section <- dplyr::left_join(section, temp[,c(1,5)])

p <- plotDipole(section, temp[temp$Dist>x&temp$Dist<(x+50),], 7,17)
p


# save(p, file = "figs/explainT20.rda")
