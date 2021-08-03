#' contraction
#'
#' ChocoAnalyzer contraction elaboration 
#'
#' @param path      file path
#' @param nome      file name
#' @param poly.gr   polynomial degree
#' @param d1_peak   first derivative peak number
#' @param d2_peak   second derivative peak numbers
#'
#' @return list results 'dati', 'header'
#'
#' @examples
#' #
#'
#' @export

contraction <- function(path="~/SFTP_Area/ITA18_Project_Area/PTPM_Praline",
                        nome="",
                        poly.gr=6,
                        d1_peak=2,
                        d2_peak=c(3,4),
                        original=F){
  
  header <- read.table(paste(path, '/', nome, sep=''), dec='.', sep=';', header=F, skip=0, nrows=22)
  header <- header[,1:2]
  names(header) <- c('name', 'value')
  # Caricamento dati
  dati <- read.table(paste(path, '/', nome, sep=''), dec='.', sep=';', header=F, skip=24)
  dati <- dati[,1:4]
  names(dati) <- c('time', 'temp', 'press', 'contr')
  
  # Regressione Polinomiale
  x <- dati$time
  y <- dati$contr
  model <- lm(y ~ poly(x, poly.gr))
  g <- function(x) {predict(model, data.frame(x=x))}
  contr1 <- g(dati$time)
  dati <- cbind(dati, contr1)
  
  # Calcolo derivate
  d1 <- derive(x=dati$time, y=dati$contr1, step=3)
  d2 <- derive(x=dati$time, y=d1, step=3)
  dati <- cbind(dati, d1, d2)
  
  # calcolo inversioni derivate
  # d1_ <- cumsum(rle(sign(diff(d1)))[[1]])
  d1_ <- which(diff(sign(diff(d1)))!=0)
  # d2_ <- cumsum(rle(sign(diff(d2)))[[1]])
  d2_ <- which(diff(sign(diff(d2)))!=0)
  
  # calcolo punti sensibili
  t0 <- dati[d1_[d1_peak],]['time']
  c0 <- dati[d1_[d1_peak],]['contr']
  T0 <- dati[d1_[d1_peak],]['temp']
  t1 <- dati[d2_[d2_peak[1]],]['time']
  c1 <- dati[d2_[d2_peak[1]],]['contr']
  T1 <- dati[d2_[d2_peak[1]],]['temp']
  t2 <- dati[d2_[d2_peak[2]],]['time']
  c2 <- dati[d2_[d2_peak[2]],]['contr']
  T2 <- dati[d2_[d2_peak[2]],]['temp']
  
  # Grafico
  # standard par('mar') <- c(5.1 4.1 4.1 2.1)
  par(mar = c(4.2,4.2,4,4.2))
  plot(dati$temp ~ dati$time, type='l', col='green', lwd=2, xlab='time in sec', ylab='', axes=F)
  axis(side=4)
  mtext(side = 4, line = 3, 'Temperature °C', col='green')
  par(new=T)
  plot(dati$d1 ~ dati$time, type='l', col='gray', lwd=1, lty='dotted', xlab='time in sec', ylab='', axes=F)
  par(new=T)
  plot(dati$d2 ~ dati$time, type='l', col='gray', lwd=1, lty='dotted', xlab='time in sec', ylab='', axes=F)
  # se richiesto stampa punti dati originali
  if (original){
    par(new=T)
    plot(dati$contr1 ~ dati$time, type='l', col='blue', lwd=0.5, lty='dotted', xlab='time in sec', ylab='', axes=F)
  }
  par(new=T)
  plot(dati$contr ~ dati$time, type='l', col='red', lwd=2, xlab='time in sec', ylab='', main=paste('Contraction: ', header[2,2], '\n', header[3,2]))
  mtext(side = 2, line = 3, 'Contraction %', col='red')
  abline(v=dati$time[d1_[d1_peak]], col='gray', lwd=1, lty='dotted')
  abline(v=dati$time[d2_[d2_peak[1]]], col='gray', lwd=1, lty='dotted')
  abline(v=dati$time[d2_[d2_peak[2]]], col='gray', lwd=1, lty='dotted')
  
  # punti sul grafico
  points(t0, c0, pch=16, col='blue')
  points(t1, c1, pch=16, col='blue')
  points(t2, c2, pch=16, col='blue')
  # legenda sui punti
  text(t0,c0, label=paste('time = ', t0, '\ncontr. = ', round(c0, 2), '\ntemp. = ', round(T0,2)), cex=, col='black', pos=2)
  text(t1,c1, label=paste('time = ', t1, '\ncontr. = ', round(c1, 2), '\ntemp. = ', round(T1,2)), cex=, col='black', pos=2)
  text(t2,c2, label=paste('time = ', t2, '\ncontr. = ', round(c2, 2), '\ntemp. = ', round(T2,2)), cex=, col='black', pos=2)
  
  # stampa dati
  if (c1 < 0){
    speed <- ((c2-0)/(t2-t1))*60
  }else{
    speed <- ((c2-c1)/(t2-t1))*60}
  text(1000,0.5, label=paste('contraction speed = ', round(speed, 4), ' %/min'), cex=, col='black', pos=4)
  
  
  invisible(list(dati, header))
}

#' derive
#'
#' Calculate gradient y dx 
#'
#' @param x      x vector
#' @param y      y vector
#' @param step   value to be interpoleted for calculation (3 or 5)
#'
#' @return gradient vector
#'
#' @examples
#' #data(choc_ctest)
#' #ctest(choc_ctest)
#'
#' @export

derive <- function(x, y, step=3){
  if (length(x) != length(y)){
    return
  }
  nulli <- floor(step/2)
  d1 <- rep(NA, length(x))
  for (i in (nulli+1):(length(x)-nulli)){
    a <- lm(y[(i-1):(i+1)]~x[(i-1):(i+1)])
    if (is.na(a$coefficients[2]))
      d1[i] <- d1[i-1]
    else
      d1[i] <- a$coefficients[2]}
  d1[1:nulli] <- d1[nulli+1]
  d1[(length(d1)-nulli+1):length(d1)] <- d1[length(d1)-nulli]
  
  return(d1)
}