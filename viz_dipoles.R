load_rda <- function(x,pattern='dipole') {
  load(x)
  if(pattern == 'dipole'){
    return(dipo)
  } else {
   return(test) 
  }
}
`%>%` <- magrittr::`%>%`

#zu wenig speicher
# memory.limit(size = 8000) #=8000MB RAM


files <- list.files('data/', pattern = 'dipole', full.names = T)
dat <- purrr::map_dfr(files, load_rda)
head(dat)

ggplot2::ggplot()+
  ggplot2::geom_line(ggplot2::aes(x = scale, y = var, group = cruise, colour = factor(cruise)),dat[dat$scale>0,])


#HE466
# H07T3: weak dooming of tc
# H05T1: clear dipole
# H05T2: clear dipole
# H05T3: clear dipole


files <- list.files('data', pattern = 'range', full.names = T)
nats <- purrr::map_dfr(files, ~load_rda(., ''))
head(nats)

temp <- dplyr::distinct(nats, id, range, q80)
mon <- data.frame(id = unique(temp$id),
                  mon = c(5, 8, 9, 5, 7, 4, 6, 2, 4, 8, 5))
temp <- dplyr::left_join(temp, mon)
mtemp <- dplyr::summarise(dplyr::group_by(temp, range, mon), q80 = mean(q80))

p_corridor <- ggplot2::ggplot()+
  ggplot2::geom_line(ggplot2::aes(x = range, y = q80, group = id, colour = factor(mon)),temp)+
  ggplot2::geom_line(ggplot2::aes(x = range, y = q80, group = mon),mtemp, linetype = 2)
  #höhere var im frühling als im (spät)sommer?
#gewichtet oder nicht macht keinen unterschied
# save(p_corridor, file = 'figs/p_corridor.rda')


#dipole vs nat var alle monate
ggplot2::ggplot()+
  ggplot2::geom_line(ggplot2::aes(x = range, y = q80, group = id),temp[temp$range < 15,])+
  ggplot2::geom_line(ggplot2::aes(x = scale, y = var, group = cruise, colour = factor(cruise)),dat)


#merge mon
nats <- dplyr::left_join(nats, mon)


#anteil von q80 an der gesamten var pro monat
top20 <- dplyr::group_by(nats, mon, id, range) %>%
  dplyr::summarise(varInTop20 = 1 - (max(q80, na.rm=T) / max(var, na.rm=T)))
colPal <- dplyr::distinct(top20, id) %>%
  dplyr::ungroup(id) %>%
  dplyr::mutate(shape = match(id, unique(id)))
c <- as.character(colPal$mon)
names(c) <- colPal$id
top20 <- dplyr::left_join(top20, colPal)
#muss nach dem join von top20 und colPal passieren!
colPal$shape[6] <- 0 #weiß gott wieso die shapes nicht gleich nummeriert sind aber so wird aus 0 -> 15 = quadrat
p20 <- ggplot2::ggplot()+
  ggplot2::labs(x = 'Range [km]', y = 'Variability of MLD in Top20 [%]')+
  ggplot2::geom_point(ggplot2::aes(x = range, y = varInTop20 * 100, color = factor(id), shape = factor(shape)), size = 2,top20)+
  ggplot2::scale_color_manual(name='Cruise (Month)', values = c, limits = factor(colPal$id), labels = paste0(colPal$id, ' (', colPal$mon, ')'))+
  ggplot2::guides(color = ggplot2::guide_legend(override.aes=list(shape = colPal$shape+15)), shape = 'none')
# save(p20, file = 'figs/p20.rda')



#reverence only in area of OWF
#check lat/lon in scanfish cruise
#use all points with section distance between points with respective lat/lon in target area
#use local background - will dipoles exceed local background instead of north sea wide background?
# lon: 5.5 - 7.7
# lat: 54 - 54.8
get_points_in_poly <- function(x, data) {
  poly <- data.frame(x = c(4, 7.7, 7.7, 4),
                     y = c(56, 56, 54, 54))  
  # poly <- data.frame(x = c(5.5, 7.7, 7.7, 5.5),
  #                    y = c(54.8, 54.8, 54, 54))
  load(x)
  id <- paste0('HE', substr(x, 29,31))
  ix <- sp::point.in.polygon(point.x = dat$Lon, point.y = dat$Lat,
                             pol.x = poly$x, pol.y = poly$y)
  rec <- dat[as.logical(ix),]
  rec$start <- c(TRUE, diff(rec$Dist)>10)
  rec$stop <- c(diff(rec$Dist)>10, TRUE)
  # View(rec[rec$start == TRUE | rec$stop == TRUE,])
  
  rec <- purrr::map2_dfr(rec$Dist[rec$start],
                         rec$Dist[rec$stop],
                         ~data[data$x > .x & data$x < .y &
                                 data$id == id,])
  return(rec)
}
files <- list.files(path = 'data/gridSFrolf', full.names = TRUE) #all SF cruises
files <- purrr::map_chr(substr(mon$id[mon$mon>4],3,5), ~files[grep(., x=files)])
ref <- purrr::map_dfr(files, ~get_points_in_poly(., nats)) #[only June (HE359)!] without April
head(ref)
ref <- dplyr::group_by(ref, range, mon) %>%
  dplyr::mutate(q80_ref = quantile(var, probs = .8, na.rm = TRUE))
# save(ref, file = 'data/varReferenceArea.rda')
# ref <- dplyr::left_join(ref, mon)
june <- ref[ref$mon > 4 & !is.na(ref$var) & ref$range < 16,]
head(june)

pole_june <- dat[grepl('HE466',dat$cruise) | grepl('HE490',dat$cruise),]

poly <- dplyr::group_by(june[june$var < june$q80_ref & june$var > 0,], range, mon) %>%
  dplyr::summarise(low = min(var), high = max(var)) %>%
  tidyr::gather(key='shape', value = 'y', low:high)
temp <- poly[poly$shape=='low',]
temp <- temp[order(temp$range, decreasing = TRUE),]
poly[poly$shape=='low',] <- temp
p_germanbight <- ggplot2::ggplot()+
  ggplot2::geom_polygon(ggplot2::aes(x = range, y = y),poly)+
  ggplot2::geom_line(ggplot2::aes(x = scale, y = var, colour = factor(cruise)),
                     pole_june[pole_june$scale>0,])+
  ggplot2::facet_wrap(~mon, labeller = label_wrap)+
  ggplot2::scale_color_discrete(name = 'Cruise')+
  ggplot2::scale_x_continuous(breaks = seq(3,15,3))+
  ggplot2::labs(x = 'Range [km]', y = 'Variability in MLD')
# save(p_germanbight, file = 'figs/german_bight_ref.rda')


#load schanfish data
# load_rda <- function(x) {
#   load(x)
#   cru <- substr(x, 29, 31)
#   out <- tibble::add_column(dat, .before = 1, cruise = cru)
#   return(out)
# }
# files <- list.files('data/gridSFrolf', full.names = T)
# data <- purrr::map_dfr(files, load_rda)
# head(data)

#depth of TC by month
ggplot2::ggplot()+
  ggplot2::geom_boxplot(ggplot2::aes(x = mon, y = -tc, group = mon),dat)
#this plot with data from reference area only (dat in map_reference_area.R) shows:
#usual MLD ~10m in June --> not shallower as predicted in Christensen!


#wahrscheinlichkeit für extremereignisse
#ist natürlich 20% du trottel - die 20% der daten die über dem 80% quantil liegen...
# 1kn = 1.852km/h
# 7kn geschwindigkeit (scanfish towed with 6-8kn) --> 7*1.852 --> 12.964km/h
# sec_distance / 12.964 --> gruppierung pro stunde

# ref$hour <- floor(ref$x/12.962)
# bla <- dplyr::group_by(ref, id, hour, range, mon) %>%
#   dplyr::summarise(var = mean(var, na.rm = TRUE), q80 = mean(q80_ref, na.rm = TRUE)) %>%
#   dplyr::summarise(extreme = var > q80, mon = mean(mon))
# sum(is.na(bla$extreme))
# bla <- dplyr::group_by(bla, mon, range) %>%
#   dplyr::summarise(extreme = sum(extreme, na.rm = TRUE) / dplyr::n())
# 
# ggplot2::ggplot()+
#   ggplot2::geom_line(ggplot2::aes(x = range, y = extreme, group = mon, colour = factor(mon)),bla)



#dipoles and their variability at different scales with the chance for natural disturbances in the background
load('data/dipoCheck.rda')
load('data/myDipos.rda')
sum(test$someDipole,na.rm = T)/nrow(test) #21%
sum(diff(test$someDipole)==1,na.rm = T)/nrow(test) #5%
tidyr::spread(test, range, someDipole) #zeile zählen in der true steht

test2 <- test %>%
  dplyr::group_by(range, strength) %>%
  dplyr::summarise(dipoProb = sum(diff(someDipole)==1, na.rm = T) / sum(!is.na(someDipole), na.rm = TRUE))
dipos2 <- dipos[dipos$dipoPos&!grepl('HE496',dipos$cruise),]
dipos2$cappedStrength <- purrr::map_dbl(dipos2$maxVrange, ~min(10, .))
#add N to plot
nBox <- dplyr::count(dipos2, scale)
# dipos2 <- dipos[dipos$dipoPos&!grepl('HE496',dipos$cruise),] %>%
#   dplyr::group_by(scale) %>%
#   dplyr::summarise(mStr = quantile(strength, .5, na.rm=T), qLow = quantile(strength, .25, na.rm=T), qHigh = min(9.9, quantile(strength, .75, na.rm=T)))
vEx <- ggplot2::ggplot()+
  ggplot2::geom_raster(ggplot2::aes(x = range, y = strength, fill = dipoProb*100),test2, hjust = 0, vjust = 0)+
  ggplot2::geom_boxplot(ggplot2::aes(x = scale*2, y = cappedStrength, group = scale*2), dipos2)+
  ggplot2::geom_text(ggplot2::aes(x = scale*2, y = rep(c(5.25, 5.75), length.out=15), label = paste0('n = ', n)), nBox[1:15,]) +
  ggplot2::scale_x_continuous(expand = c(0,0), limits = c(9, 25))+
  ggplot2::scale_y_continuous(expand = c(0,0), limits = c(5, 10.1))+
  ggplot2::labs(x = 'Range [km]', y = 'Vertical excursion of MLD [m]')+
  ggplot2::theme(legend.position = 'bottom')+
  ggplot2::scale_fill_gradient2(low = 'red', mid = 'grey', high = 'white', midpoint = 5, name = 'Probability for a dipole-like pattern [%]')+
  ggplot2::theme(axis.text.y = ggplot2::element_text(vjust = -2.5, hjust=1), axis.ticks.y = ggplot2::element_blank())
save(vEx, file = 'figs/vEx.rda')


plotHist <- function(scale) {
  load('data/range1_100.rda')
  # load('data/myDipos.rda') #wird nicht mehr genutzt
  mon <- data.frame(id = unique(test$id),
                    mon = c(5, 8, 9, 5, 7, 4, 6, 2, 4, 8, 5))#von oben kopiert!
  test <- dplyr::left_join(test, mon)
  # hist of MLD variance at a specific scale
  temp <- test[test$range==scale & !is.na(test$var),]#test$id == 'HE331',]
  grp_bins <- function(data, mon) {
    temp <- data[data$mon == mon,] %>%
      dplyr::ungroup() %>%
      dplyr::mutate(varBinned = floor(var/.5)*.5) %>%
      dplyr::group_by(varBinned) %>%
      dplyr::summarise(countVar = dplyr::n()) %>%
      dplyr::mutate(normVar = countVar / max(countVar,na.rm=T)) %>%
      dplyr::mutate(relativeVar = countVar / sum(countVar,na.rm=T)) %>%
      dplyr::mutate(cummulativeVar = cumsum(relativeVar)) %>%
      dplyr::mutate(mon = mon)
    return(temp)
  }
  temp <- purrr::map_dfr(c(2,4:9), ~grp_bins(temp, .))
  
  #plot a histogram of vertical excusions for specific month and transect length
  # temp <- temp[temp$mon == 7,]
  # q80 <- quantile(rep(temp$varBinned, times = temp$countVar),na.rm=T,.8)
  # pHist <- ggplot2::ggplot()+
  #   # ggplot2::geom_boxplot(ggplot2::aes(x = maxVrange/2, y = .5),dipos[dipos$scale==(scale/2) & !grepl('HE496', dipos$cruise),][-5,], orientation = 'y')+
  #   ggplot2::geom_histogram(ggplot2::aes(x = varBinned, y = normVar), temp, stat = 'identity')+
  #   ggplot2::geom_line(ggplot2::aes(x = varBinned, y = 1-cummulativeVar), temp)+
  #   # ggplot2::geom_vline(xintercept = 3)+
  #   ggplot2::geom_vline(xintercept = q80,colour='red')+
  #   ggplot2::labs(x = 'Vertical excursion of MLD [m]', y = 'Normalized n')
  # return(pHist)
  
  #plot a line of probability for dipole-like patterns over the entire year (for a specific transect length)
  pLine <- ggplot2::ggplot()+
    ggplot2::geom_line(ggplot2::aes(x = mon, y = (1-cummulativeVar)*100, colour = factor(varBinned)),
                       temp[temp$varBinned >= 4 & temp$varBinned<=6 & temp$mon < 9,])+
    ggplot2::labs(x = 'Month', y = 'Probability [%]')+
    ggplot2::scale_color_discrete(name = 'Vertical excursion [m]')+
    ggplot2::theme(legend.position = 'bottom')
  #data for september not suffcient
  return(pLine)
}
pLine <- plotHist(10)
#distribution of vertical excursions at range 10km in july
# save(pHist, file = 'figs/pHist.rda')
#probability for vertical excursions >X at 10km in different month
# save(pLine, file = 'figs/pLine.rda')

# for (i in seq(10,20)) {
#   print(plotHist(i))
# }

x <- unique(dipos$scale)
purrr::map_dbl(x, ~nrow(dipos[dipos$scale==. & dipos$dipoPos==TRUE,]))


