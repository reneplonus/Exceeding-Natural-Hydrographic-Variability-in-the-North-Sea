#Corridor of natural variability für verschiedene ranges bestimmen (anhand von scanfish)
#plot ähnlich der wavelet: signifikant = variabilität > 80% quantil
#plotten: 80% quantil VS range (model?) --> corridor of nat. variability
#variabilität von dipolen bestimmen (alle gemeinsam? jeder einzeln? txt files mit liste von transekten)
#--> für welche ranges ist variabilitöt_dipol > corr. of nat. var.? oder überhaupt?
corrNATvar <- function(dat,   #data
                       range) #window to calculate variability
{
  temp <- dplyr::distinct(dat, Dist, tc)
  x <- temp$Dist     #distances in km
  tc <- temp$tc      #depth of the thermocline
  id <- dat$cruise[1]
  #empty vector to store results
  l <- length(x)
  dummy <- rep(NA, times = l)
  #indicees for x > range
  ix <- which.max(x > x[1]+range):l
  #standard deviation of 'tc' within  a window of 'range' km
  var_ <- purrr::map_dbl(ix, ~sd(tc[x >= x[.] - range & x <= x[.]], na.rm = TRUE))
  
  #das ist blödsinn weil dipole dann zB 0 variability hätten - es würde longterm-trends rausrechnen aber macht dann eben erst bei einem range >X sinn und den müsste man irgendwie definieren...
  # var_ <- purrr::map_dbl(ix, ~sqrt(sum((tc[x >= x[.] - range & x <= x[.]] - predict(lm(tc[x >= x[.] - range & x <= x[.]] ~ x[x >= x[.] - range & x <= x[.]])))**2)) / length(x[x >= x[.] - range & x <= x[.]]))
  #kann NA ergeben, wenn das nächst kleinere X weiter weg ist als range --> dann bekommt sd() nur 1 wert
  n_ <- purrr::map_dbl(ix, ~sum(x >= x[.] - range & x <= x[.], na.rm = TRUE))
  
  #place each value in the middle of the window that it represents
  #start <- which.max(x >= (x[1] + range/2))
  #stop <- start + length(var_) - 1
  #dummy[start:stop] <- var_
  
  dummy[ix] <- var_
  #80% quantile = threshold of natural variability
  # q80 <- DescTools::Quantile(var_, weights = n_ / max(n_, na.rm = TRUE), na.rm = TRUE, probs = .8)
  #das gewichtete quantile scheint nicht auszureichen - werte die aus weniger als 50% der nötigen daten erzeugt werden haben
  #immernoch einen sehr großen einfluss auf das quantil was nicht gut scheint..
  q80 <- quantile(var_[(n_/max(n_, na.rm = TRUE)) > .5], probs = .8) #darum jetzt nur mit werten die auf min 50% der erforderlichen daten basieren (= range 10 mit min 5 werten)
  print(paste0('Current range: ', range, ' in ', id))
  out <- data.frame(id = id, x = x, range = range, var = dummy, n = NA, q80 = q80)
  #out$n[start:stop] <- n_
  out$n[ix] <- n_
  return(out)
}

load_rda <- function(x) {
  load(x)
  cru <- paste0('HE', substr(x, 60, 62))
  out <- tibble::add_column(dat, .before = 1, cruise = cru)
  
  temp <- dat[dat$Depth >= 5 & dat$Depth <= 35,]
  temp <- temp[!is.na(temp$Density),]
  filt <- dplyr::count(dplyr::group_by(temp, Dist), Dist)
  temp <- dplyr::left_join(temp, filt)
  temp <- temp[temp$n > 9,-ncol(temp)]
  temp <- dplyr::summarise(dplyr::group_by(temp, Dist), tc = which.max(diff(Density))+5,
                           dT = max(Temp) - min(Temp))
  out <- dplyr::left_join(out, temp)
  return(out[out$dT>.5 & !is.na(out$tc),]) #dT > .5 = stratified
  # return(out[out$dT>2 & !is.na(out$tc),]) #dT > 2 --> dipole transects had dT 2-3°C
}
files <- list.files('/media/rene/share/Paper/Dipole/data/gridSFrolf', full.names = T)[-c(4,7,10)]
#4: max temperature in HE319 is 4°C
#7: temperature range in HE336 is 14.6 - 16.4°C # hat nur 40 profile die alle TC = 11 haben --> gerade linie!
#10: max temperature range in HE365 is .7°C
dat <- purrr::map(files, load_rda)
head(dat)


test <- purrr::map_dfr(dat, function(x) purrr::map_dfr(1:100, function(y) corrNATvar(x, y)))
test <- dplyr::mutate(dplyr::group_by(test, range), n_ = n / max(n, na.rm = TRUE))
#max(n) --> max mögliche anzahl an werten in einem range von X km
#n / max(n) --> 'completeness' des windows!
head(test)
# save(file = 'data/range1_100.rda',test)
# p1 <- ggplot2::ggplot()+
#   ggplot2::geom_tile(ggplot2::aes(x = x, y = range, fill = var > q80), test[!is.na(test$var),])+
#   ggplot2::scale_fill_manual(values = c('grey', 'white'))+
#   ggnewscale::new_scale("fill") +
#   ggplot2::geom_tile(ggplot2::aes(x = x, y = range, fill = n_), test[!is.na(test$var),], alpha = .2)+
#   ggplot2::scale_fill_gradientn(colours = c("red", "red","yellow","yellow"), 
#                                   values = scales::rescale(c(0.,.25,.75,1.)),
#                                   guide = "colorbar", limits=c(0,.5))+
#   ggplot2::scale_y_reverse()
# 
# p2 <- ggplot2::ggplot()+
#   ggplot2::geom_point(ggplot2::aes(x = Section_Distance_km_grid, y = -tc),dat)
# 
# ggpubr::ggarrange(plotlist=list(p1,p2),nrow=2,common.legend = TRUE)
# 
# ggplot2::ggplot()+
#   ggplot2::geom_point(ggplot2::aes(x = range, y = q80),dplyr::distinct(test, range, q80))
