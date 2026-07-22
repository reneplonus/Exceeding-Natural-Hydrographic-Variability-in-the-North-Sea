#the currently investigated tc values
checkForDipole <- function(win) {
  # #check if the difference between the shallowest and deepest values is at least 8m
  # if(diff(quantile(win, probs = c(.05, .95))) < maxVrange) {return(FALSE)}
  # #difference between value and mean of window
  dwin <- win > mean(win, na.rm = TRUE)
  #first half of the window
  left <- dwin[1:(length(win)/2)]
  #second half of the window
  right <- dwin[(length(left)+1):length(win)]
  #count duplicates (win is a vector of True/False and so are left/ right)
  dupLeft <- scrutiny::duplicate_count(left) #the first row in the returned tibble is the most frequent duplicate
  dupRight <- scrutiny::duplicate_count(right)
  # #check if 80% of all values in left/ right are true OR false
  # if(dupLeft$frequency[1] < length(left)*.8) {return(FALSE)} #stop here if condition is not met
  # if(dupRight$frequency[1] < length(right)*.8) {return(FALSE)}
  # #check if left and right differ in which value is more frequent (true or false) - if no - stop here
  # if(dupLeft$value[1] == dupRight$value[1]) {return(FALSE)}
  # #if all conditions are met return TRUE
  # return(TRUE)
  print(ggplot2::ggplot()+
    ggplot2::geom_line(ggplot2::aes(x = 1:length(win), y = win), data = NULL)+
    ggplot2::geom_hline(yintercept = quantile(win, probs = c(.05, .5, .95)))+
    ggplot2::geom_vline(xintercept = length(win)/2))
  
  out <- data.frame(maxVrange = quantile(win, probs = .95) - quantile(win, probs = .05),
                    leftFreq = dupLeft$frequency[1] / length(left),
                    rightFreq = dupRight$frequency[1] / length(right),
                    oppposing = dupLeft$value[1] != dupRight$value[1])
  return(out)
}
# calc dipol variability
# wind induced dipole structures:
# HE466 27.6. (Hol5) - T1, T2, T3 (2 dipole?)
# H05T1/T2/T3
# HE466 29.6. (Hol6) - T4 (bei mir T3!)
# H06T3
# HE466 30.6. (Hol7) - T1 (weak) (bei mir T3!)
# H07T3
# HE490 24.6. (Hol3) - T1 (vllt?)
# H03T1


# HE496 19.9. (Hol3) - T2 (weak?), T3 (weak?), T4 (weak?)
# T4,T5,T6 - T1 als referenz? - quasi keine stratifizierung!!!
# HE496 20.9. (Hol3) - T1, T2, T3
# hab ich nicht

# ggplot2::ggplot()+
#   ggplot2::geom_point(ggplot2::aes(x=Lon_E, y = Lat_N),grid)

files <- c('HE466_H05T1', 'HE466_H05T2', 'HE466_H05T3', 'HE466_H06T3', 'HE466_H07T3', 'HE490_H03T1', 'HE496_H03T4', 'HE496_H03T5', 'HE496_H03T6')
strat <- c(1., 1., 1., 1., 1., .5, .05, .04, .06)
z <- c(54, 103, 144, 144, 108, 58, 159, 197, 244)

calcDipoleStuff <- function(file, strat, z) {
  load(paste0('data/output_AE/', file, '.rda'))
  head(dataset)
  
  
  dataset <- dplyr::mutate(dplyr::group_by(dataset, Section_Distance_grid_km),
                           dT = max(temperature_degc) - min(temperature_degc))  #maximaler temperaturunterschied
  
  temp <- dplyr::summarise(dplyr::group_by(dataset[dataset$cluster != -1,], Section_Distance_grid_km), strat = !length(unique(cluster)) == 1)
  dataset <- dplyr::left_join(dataset, temp)
  dataset$strat[is.na(dataset$strat)] <- FALSE #NA falls eine ganze spalte nur -1 einhielt --> nicht stratifiziert
  print(ggplot2::ggplot()+
    ggplot2::geom_boxplot(ggplot2::aes(x = strat, y = dT, group = strat),dataset))
  
  scales <- seq(5,15,.5)
  # strat
  # HE466 T1: 1.
  # HE466 T2: 1.
  # HE466 T3: 1.
  # HE466 T3(H06): 1.
  # HE466 T3(H07): 1.
  # HE490 T1: .5
  # HE496 T4: .05
  # HE496 T5: .04
  # HE496 T6: .06
  
  print(ggplot2::ggplot()+
    ggplot2::geom_tile(ggplot2::aes(x = Section_Distance_grid_km, y = -Depth_grid_m,
                                    fill = temperature_degc),dataset[dataset$dT > strat,])+
    ggplot2::geom_tile(ggplot2::aes(x = Section_Distance_grid_km, y = -Depth_grid_m,
                                    fill = temperature_degc),dataset[dataset$dT <= strat,],alpha=.3)+
    ggplot2::scale_fill_gradientn(colours = c("blue", "orange"),
                                  values = scales::rescale(range(dataset$temperature_degc,na.rm = T)),
                                  guide = "colorbar", limits=range(dataset$temperature_degc,na.rm = T)))
  
  # z
  # HE466 T1: 54
  # HE466 T2: 102
  # HE466 T3: 145
  # HE466 T3(H06): 144
  # HE466 T3(H07): 103
  # HE490 T1: 58
  # HE496 T4: 159
  # HE496 T5: 197
  # HE496 T6: 244
  
  temp <- dataset[dataset$dT > strat, c(1,2,7)]
  temp <- temp[order(temp$Section_Distance_grid_km, temp$Depth_grid_m),]
  temp <- temp[temp$Depth_grid_m >= 5 & temp$Depth_grid_m <= 35,]
  temp <- dplyr::summarise(dplyr::group_by(temp, Section_Distance_grid_km), tc = which.max(diff(density_kg_m))+5)
  #maximal sprünge von 3m zulassen um var nicht künstlich groß zu machen:
  # for (row in 2:nrow(temp)) {
  #   ix <- abs(diff(temp$tc)[row-1])>1
  #   if(ix){
  #     v <- c(temp$tc[row-1]-1, temp$tc[row-1]+1)
  #     temp$tc[row] <- v[which.min(abs(v-temp$tc[row]))]
  #   }
  # }
  
  ix <- which.min(abs(temp$Section_Distance_grid_km - z))
  temp$Section_Distance_grid_km[ix]
  
  corr_nat <- function(x, tc, ix, range) {sd(tc[x >= x[ix] - range & x <= x[ix] + range], na.rm = TRUE)} #sd
  
  vars <- purrr::map_dbl(scales, ~corr_nat(temp$Section_Distance_grid_km, temp$tc, ix, .))
  dipoQuan <- purrr::map_dfr(scales, ~checkForDipole(temp$tc[temp$Section_Distance_grid_km >= temp$Section_Distance_grid_km[ix] - . & temp$Section_Distance_grid_km <= temp$Section_Distance_grid_km[ix] + .]))
  dipo <- data.frame(cruise = file,
                     Section_Distance_grid_km = temp$Section_Distance_grid_km[ix],
                     scale = scales,
                     var = vars)
  dipo <- as.data.frame(cbind(dipo, dipoQuan))
  head(dipo)
  return(dipo)
  # save(dipo, file = paste0('data/dipole_', file, '.rda'))
  # load(file = paste0('data/dipole_', file, '.rda'))
}

dipos <- purrr::pmap_dfr(list(files, strat, z), calcDipoleStuff)
dipos$dipoPos <- dipos$maxVrange >=5 & dipos$leftFreq > .6 & dipos$rightFreq > .6 & dipos$oppposing
# save(dipos, file = 'data/myDipos.rda')
ggplot2::ggplot()+
  ggplot2::geom_boxplot(ggplot2::aes(group = scale, x = scale, y = maxVrange),dipos[!grepl('HE496',dipos$cruise),])
ggplot2::ggplot()+
  ggplot2::geom_boxplot(ggplot2::aes(group = scale, x = scale, y = rightFreq),dipos[!grepl('HE496',dipos$cruise),])



plotDipole <- function(dipoData, dataFile, minT, maxT, maxScale, strat, xInter) {
  load(paste0('data/output_AE/', dataFile, '.rda'))
  dipo <- dipoData[dipoData$cruise == dataFile,]
  diVar <- data.frame(x = seq(-maxScale, maxScale, .5))
  diVar$scale <- abs(diVar$x)
  dipo <- dplyr::left_join(diVar, dipo)
  dataset <- dplyr::mutate(dplyr::group_by(dataset, Section_Distance_grid_km),
                           dT = max(temperature_degc) - min(temperature_degc))  #maximaler temperaturunterschied
  temp <- dataset[dataset$dT > strat, c(1,2,7)]
  temp <- temp[order(temp$Section_Distance_grid_km, temp$Depth_grid_m),]
  temp <- temp[temp$Depth_grid_m >= 5 & temp$Depth_grid_m <= 35,]
  temp <- dplyr::summarise(dplyr::group_by(temp, Section_Distance_grid_km), tc = which.max(diff(density_kg_m))+5)
  
  return(ggplot2::ggplot()+
           ggplot2::geom_tile(ggplot2::aes(x = Section_Distance_grid_km, y = -Depth_grid_m, fill = pmax(pmin(temperature_degc,maxT),minT)),dataset[dataset$dT > strat,])+
           ggplot2::geom_tile(ggplot2::aes(x = Section_Distance_grid_km, y = -Depth_grid_m, fill = pmax(pmin(temperature_degc,maxT),minT)),dataset[dataset$dT <= strat,],alpha=.3)+
           # ggplot2::geom_line(ggplot2::aes(x = Section_Distance_grid_km[1]+x, y = -10,
           #                                 colour = pmax(pmin(var,4),1)), dipo,linewidth = 5)+
           ggplot2::geom_line(ggplot2::aes(x = Section_Distance_grid_km, y = -tc),temp, linewidth = 1)+
           ggplot2::scale_fill_gradientn(colours = c("blue", "orange"),
                                         values = scales::rescale(c(minT, maxT)),
                                         guide = "colorbar", limits=c(minT,maxT),
                                         name = 'Temperature [°C]')+
           # ggplot2::scale_color_continuous(name = 'Var',
           #                                 guide = "colorbar", limits=c(1,4),
           #                                 labels = c('<1', '2', '3', '4<'))+
           ggplot2::geom_vline(xintercept = xInter, linetype = 2)+
           ggplot2::labs(x = 'Section distance [km]', y = 'Depth [m]'))
}


HE466_1 <- plotDipole(dipoData = dipos, dataFile = 'HE466_H05T1', minT = 11, maxT = 15, maxScale = 8, strat = 1., xInter = c(47, 62))
HE466_1
# save(HE466_1, file = 'figs/diHE466_1.rda')

HE466_2 <- plotDipole(dipoData = dipos, dataFile = 'HE466_H05T2', minT = 11, maxT = 15, maxScale = 8, strat = 1., xInter = c(95, 110))
HE466_2 <- HE466_2+ggplot2::scale_x_reverse()
HE466_2
# save(HE466_2, file = 'figs/diHE466_2.rda')

HE466_3 <- plotDipole(dipoData = dipos, dataFile = 'HE466_H05T3', minT = 11, maxT = 15, maxScale = 8, strat = 1., xInter = c(136, 152))
HE466_3
# save(HE466_3, file = 'figs/diHE466_3.rda')

HE466_4 <- plotDipole(dipoData = dipos, dataFile = 'HE466_H06T3', minT = 11, maxT = 15, maxScale = 8, strat = 1., xInter = c(137, 150))
HE466_4 <- HE466_4+ggplot2::scale_x_reverse()
HE466_4
# save(HE466_4, file = 'figs/diHE466_4.rda')

HE466_5 <- plotDipole(dipoData = dipos, dataFile = 'HE466_H07T3', minT = 11, maxT = 15, maxScale = 8, strat = 1., xInter = c(100, 115))
HE466_5 <- HE466_5+ggplot2::scale_x_reverse()
HE466_5
# save(HE466_5, file = 'figs/diHE466_5.rda')

HE490 <- plotDipole(dipoData = dipos, dataFile = 'HE490_H03T1', minT = 14, maxT = 16, maxScale = 8, strat = .5, xInter = c(52, 61))
HE490
# save(HE490, file = 'figs/diHE490.rda')

HE496_1 <- plotDipole(dipoData = dipos, dataFile = 'HE496_H03T4', minT = 17, maxT = 17.5, maxScale = 8, strat = .05, xInter = c(153, 165))
HE496_1
# save(HE496_1, file = 'figs/diHE496_1.rda')

HE496_2 <- plotDipole(dipoData = dipos, dataFile = 'HE496_H03T5', minT = 17, maxT = 17.5, maxScale = 8, strat = .04, xInter = c(191, 203))
HE496_2
# save(HE496_2, file = 'figs/diHE496_2.rda')

HE496_3 <- plotDipole(dipoData = dipos, dataFile = 'HE496_H03T6', minT = 17, maxT = 17.5, maxScale = 8, strat = .06, xInter = c(237, 250))
HE496_3
# save(HE496_3, file = 'figs/diHE496_3.rda')


# files <- list.files('figs/',pattern = 'diHE466',full.names = TRUE)
# for (file in files) {
#   load(file)
# }
