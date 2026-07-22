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
  return(out[out$dT>.0005 & !is.na(out$tc),]) #dT > .5 = stratified
}
corrNATvar <- function(dat,       #data
                       range,     #window to calculate variability
                       strength)  #strength of potential dipole structures
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
  # var_SD <- purrr::map_dbl(ix, ~sd(tc[x >= x[.] - range & x <= x[.]], na.rm = TRUE))
  # var_LM <- purrr::map_dbl(ix, ~sqrt(sum((tc[x >= x[.] - range & x <= x[.]] - predict(lm(tc[x >= x[.] - range & x <= x[.]] ~ x[x >= x[.] - range & x <= x[.]])))**2)) / length(x[x >= x[.] - range & x <= x[.]]))

  #the currently investigated tc values
  checkForDipole <- function(win, strength = 5, n) {
    #check if win has enought data
    if(length(win) < n*.8) {return(FALSE)}
    #check if the difference between the shallowest and deepest values is at least 8m
    if(diff(quantile(win, probs = c(.05, .95))) < strength) {return(FALSE)}
    #difference between value and mean of window
    win <- win > mean(win, na.rm = TRUE)
    #first half of the window
    left <- win[1:(length(win)/2)]
    #second half of the window
    right <- win[(length(left)+1):length(win)]
    #count duplicates (win is a vector of True/False and so are left/ right)
    dupLeft <- scrutiny::duplicate_count(left) #the first row in the returned tibble is the most frequent duplicate
    dupRight <- scrutiny::duplicate_count(right)
    #check if left and right differ in which value is more frequent (true or false) - if no - stop here
    if(dupLeft$value[1] == dupRight$value[1]) {return(FALSE)}
    #check if 60% of all values in left/ right are true OR false
    if(dupLeft$frequency[1] < length(left)*.6) {return(FALSE)} #stop here if condition is not met
    if(dupRight$frequency[1] < length(right)*.6) {return(FALSE)}
    #if all conditions are met return TRUE
    return(TRUE)
  }
  foundDipole <- purrr::map_lgl(ix, ~checkForDipole(tc[x >= x[.] - range & x <= x[.]], strength, range))
  n <- purrr::map_dbl(ix, ~length(tc[x >= x[.] - range & x <= x[.]]))
  
  # dummy[ix] <- var_SD / var_LM
  dummy[ix] <- foundDipole
  
  print(paste0('Current range: ', range, ' in ', id))
  out <- data.frame(id = id, x = x, range = range, strength = strength, someDipole = dummy)
  #out$n[start:stop] <- n_
  return(out)
}
files <- list.files('data/gridSFrolf', full.names = T)


dat <- purrr::map(files, load_rda)
#4: max temperature in HE319 is 4°C
#7: temperature range in HE336 is 14.6 - 16.4°C
#10: max temperature range in HE365 is .7°C
##nochmal machen und diese reisen rausnehmen weil die daten im corNatVar auch nicht drin sind?!
##so wird nämlich die wahrscheinlichkeit dipole zu finden künstlich (minimal?) reduziert?

save_output <- function(x) {
  range <- seq(10, 25, .5)
  strength <- seq(5, 10, .5)
  input <- expand.grid(range, strength)
  dipoSF <- purrr::map2_dfr(input$Var1, input$Var2, function(y, z) corrNATvar(x, y, z))
  # save(dipoSF, file = paste0(dipoSF$id[1],'_checkDipo.rda'))
  return(dipoSF)
}

test <- purrr::map_dfr(dat, save_output)

# save(test, file = 'dipoCheck.rda')
# ggplot2::ggplot()+
#   ggplot2::geom_line(ggplot2::aes(x, y = (var)),test[test$id=='HE303' & test$range==15,], linewidth = 5)


# View(test[test$id=='HE303'&log(test$var)>20&!is.na(test$var),])

dat <- load_rda(files[14])

plotProfile <- function(x){print(ggplot2::ggplot()+ggplot2::geom_point(ggplot2::aes(x=c(0,diff(Density)),y=-Depth),dat[dat$Dist==x,]))}
plotProfile(561.5)
