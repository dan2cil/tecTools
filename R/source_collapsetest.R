#' collapse test
#'
#' Calculate the melting point of a chocolate by collapse test analysis
#'
#' @param analisi data frame of data [Test, Temperature, Gap]
#' @param xlim    x axis limits - default c(26, 35)
#' @param ylim    y axis limits - default c(0, 5)
#' @param main    graph title - default 'Collapse Test
#' @param point   boolean T = plot points of d' and d'' - default F
#' @param values  boolean T = plot values of d' and d'' - default F
#' @param dt      boolean T = plot derivatives - default F
#' @param mm      integer media mobile se >1
#'
#' @return analysis list (calculated data and results)
#'
#' @examples
#' #data(choc_ctest)
#' #ctest(choc_ctest)
#'
#' @export

ctest <- function(analisi, xlim, ylim, main='Collapse test', point=F, values=F, dt=F, mm=1){

    # Variabili di base
    call <- match.call()
    #type <- match.arg(type)
    if (mm > 1 & mm%%2 == 0) mm = mm + 1
    if (!"xlim" %in% names(call)) {xlim <- c(26, 35)}
    if (!"ylim" %in% names(call)) {ylim <- c(0, 5)}

    results <- list()
	ntest <- nlevels(analisi$Test)
	test <- levels(analisi$Test)
	min <- xlim[1]
	max <- xlim[2]

	# Ciclo fra le diverse analisi
    for (i in 1:ntest){
	    # Seleziono il test corrente e cancello i livelli non presenti
	    df <- subset(analisi, Test==test[i])
	    # Eventuale media mobile dei valori di Gap
	    if (mm>1){
	        n <- mm
	        a <- df$Gap
	        mediamobile <- (cumsum(a)[(n):length(a)] - c(0, cumsum(a)[1:(length(a) - n)])) / n
	        mediamobile <- c(rep(NA, (mm-1)/2), mediamobile, rep(NA, (mm-1)/2))
	        df$Gap <- mediamobile
	    }
	    df$Test <- df$Test[drop=T]
	    dati <- length(df$Gap)

    	# Calcolo derivata prima -----------------------------------------------
    	d1 <- c(NA)
    	for (i in 2:(dati-1)){
    		a <- lm(df$Gap[(i-1):(i+1)]~df$Temperature[(i-1):(i+1)])
    		if (is.na(a$coefficients[2]))
                d1 <- c(d1, d1[length(d1)])
            else
                d1 <- c(d1,as.vector(a$coefficients[2]))}
    	d1 <- c(d1,NA)

        # Calcolo derivata seconda ---------------------------------------------
    	d2 <- c(NA,NA)
    	for (i in 3:(length(d1)-2)){
    		b <- lm(d1[(i-1):(i+1)]~df$Temperature[(i-1):(i+1)])
    		d2 <- c(d2,as.vector(b$coefficients[2]))}
    	d2 <- c(d2,c(NA,NA))

        # Eventuale Media Mobile sui valori delle derivate
        #if (mm > 1)  {
        #    for ( i in 3:(dati-mm-1)){
        #        d1[i] <- mean(d1[i:(i+mm-1)])
        #        d2[i] <- mean(d2[i:(i+mm-1)])}
        #    d1[1:mm] <- rep(NA, mm)
        #    d1[(length(d1)-mm+1):length(d1)] <- rep(NA, mm)
        #    d2[1:mm] <- rep(NA, mm)
        #    d2[(length(d2)-mm+1):length(d2)] <- rep(NA, mm)
        #}

    	# Variabili di lavoro
    	Temperature <- df$Temperature
    	Gap <- df$Gap

        # Calcolo punti di flesso
    	d1Min <- min(na.omit(d1)) ; d1MinPos <- which(d1 == d1Min)
    	Tempd1Min <- Temperature[d1MinPos] ; Gapd1Min <- Gap[d1MinPos]
    	d2Min <- min(na.omit(d2)) ; d2MinPos <- which(d2 == d2Min)
    	Tempd2Min <- Temperature[d2MinPos] ; Gapd2Min <- Gap[d2MinPos]
    	d2Max <- max(na.omit(d2)) ; d2MaxPos <- which(d2 == d2Max)
    	Tempd2Max <- Temperature[d2MaxPos] ; Gapd2Max <- Gap[d2MaxPos]

    	# Calcolo OnSet
    	# Retta tangente al flesso y = m0x + a0
    	lin0 <- lm(Gap[(d1MinPos-1):(d1MinPos+1)]~Temperature[(d1MinPos-1):(d1MinPos+1)])
    	m0 <- as.vector(lin0$coefficients[2])
    	a0 <- as.vector(lin0$coefficients[1])
    	# Retta tangente a punto iniziale y = m1x + a1
    	posizione <- which(Temperature>=min)[1]
    	lin1 <- lm(Gap[(posizione):(posizione+2)]~Temperature[(posizione):(posizione+2)])
    	m1 <- as.vector(lin1$coefficients[2])
    	a1 <- as.vector(lin1$coefficients[1])
    	OnSet.x <- round((a1-a0)/(m0-m1), 2)
    	OnSet.y <- round(a0 + OnSet.x * m0, 2)

    	# Print Results
    	#print(paste(df$Test[1], rep(" ", 5), 'OnSet.x = ', OnSet.x))
    	print(paste(df$Test[1], '  ', 'OnSet.x = ', OnSet.x, sep=''))
    	# Save results in results list
    	result <- list('Tempd2Min'=Tempd2Min, 'Gapd2Min'=Gapd2Min, 'Tempd1Min'=Tempd1Min, 'Gapd1Min'=Gapd1Min,
    	               'Tempd2Max'=Tempd2Max, 'Gapd2Max'=Gapd2Max, 'OnSet.x'=OnSet.x, 'OnSet.y'=OnSet.y)
        results[[levels(df$Test)]] <- list(dati=data.frame('Test'=df$Test, 'Temperature'=df$Temperature, 'Gap'=df$Gap, d1, d2), result=result)
    }

	# Grafici --------------------------------------------------------------------
	# Prepare the colors list
	col <- rainbow(ntest)
	#col <- topo.colors(casi)
	#col <- terrain.colors(casi)

	# Scelta dei margini del grafico
	if (dt){
	    par(mar=c(5,4,2,4)+0.1)
	}else{
	    par(mar=c(5,4,2,1)+0.1)}

	# Grafico standard
	plot(1, -1, xlim=xlim, ylim=ylim, type='b', lty='solid', lwd=2, xlab='Temperatura', ylab='Gap', main=main)
	#rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col='#10101020', border='black')
	grid(col='grey80')
	# Ciclo fra le diverse analisi
	for (i in 1:ntest){
	    par(new=T)
	    # Variabili di lavoro
	    Gap <- results[[test[i]]]$dati$Gap
	    Temperature <- results[[test[i]]]$dati$Temperature
	    d1 <- results[[test[i]]]$dati$d1
	    d2 <- results[[test[i]]]$dati$d2
	    Tempd2Min = results[[test[i]]]$result$Tempd2Min
	    Gapd2Min  = results[[test[i]]]$result$Gapd2Min
	    Tempd1Min = results[[test[i]]]$result$Tempd1Min
	    Gapd1Min  = results[[test[i]]]$result$Gapd1Min
	    Tempd2Max = results[[test[i]]]$result$Tempd2Max
	    Gapd2Max  = results[[test[i]]]$result$Gapd2Max
	    OnSet.x = results[[test[i]]]$result$OnSet.x
	    OnSet.y = results[[test[i]]]$result$OnSet.y
	    if (point) {
	        plot(Gap~Temperature, xlim=c(min, max), ylim=ylim, type='b', lty='solid', lwd=2, col=col[i], xlab='Temperatura', ylab='Gap', main=main)
	        #plot(lowess(Temperature, Gap, f=0.1), xlim=c(min, max), ylim=ylim, type='b', lty='solid', lwd=2, col=col[i], xlab='Temperatura', ylab='Gap', main=main)
	    } else {
	        plot(Gap~Temperature, xlim=c(min, max), ylim=ylim, type='l', lty='solid', lwd=2, col=col[i], xlab='Temperatura', ylab='Gap', main=main)
	        #plot(lowess(Temperature, Gap, f=0.1), xlim=c(min, max), ylim=ylim, type='l', lty='solid', lwd=2, col=col[i], xlab='Temperatura', ylab='Gap', main=main)
	    }
	    points(OnSet.x,OnSet.y, pch=16, col=col[i])

        # Stampa i punti dei flessi
	    if (dt | values){
	        points(Tempd2Min,Gapd2Min, pch=16, col=col[i])
	        points(Tempd1Min,Gapd1Min, pch=16, col=col[i])
	        points(Tempd2Max,Gapd2Max, pch=16, col=col[i])
	    }

	    # Stampa i valori dei flessi
	    text(OnSet.x+0.1,OnSet.y, label=paste('OnSet =', OnSet.x), cex=0.6, col='blue', pos=4)
	    if (values){
            text(Tempd2Min+0.1,Gapd2Min, label=paste('d\'\' min =', Tempd2Min), cex=0.5, col='black', pos=4)
            text(Tempd1Min+0.1,Gapd1Min, label=paste('d\' min =', Tempd1Min), cex=0.5, col='black', pos=4)
            text(Tempd2Max+0.1,Gapd2Max, label=paste('d\'\' Max =', Tempd2Max), cex=0.5, col='black', pos=4)
	    }
	    # stampa le linee di costruzione dell'onset
	    lines(c(Tempd1Min,OnSet.x), c(Gapd1Min,OnSet.y), lty='dotted', col='grey80')
	    lines(c(OnSet.x, Temperature[posizione]), c(OnSet.y,Gap[posizione]), lty='dotted', col='grey80')
	    if (dt){

	        par(new=T)
	        plot(d1~Temperature, xlim=xlim, ylim=c(-5,5), ann=F, axes=F, type='l', lty='dotted', lwd=1, col=col[i], xlab='Temperatura', ylab='Gap', main=main)
	        par(new=T)
	        plot(d2~Temperature, xlim=xlim, ylim=c(-5,5), ann=F, axes=F, type='l', lty='dashed', lwd=1, col=col[i], xlab='Temperatura', ylab='Gap', main=main)
	        mtext('Derivate', side=4, line=3)
	        axis(4)
	        #abline(v=Tempd1Min, lty='dotted', col=col[i])
	        #abline(v=Tempd2Min, lty='dashed', col=col[i])
	        #abline(v=Tempd2Max, lty='dashed', col=col[i])
	    }
	}

	# Scrive la legenda complessiva
	#legend(x=max-2, y=4, legend=c(legend, 'd\'', 'd\'\''), col= c('red', 'blue', 'green'), lty=c('solid', 'dotted', 'dotted'), lwd=c(2, 1, 1), bty='n')
	#legend(x=max-2, y=ylim[2], legend=test, col= col[1:ntest], lwd=2, bty='n')
	legend(x=max-(max-min)/5, y=ylim[2], legend=test, col= col[1:ntest], lwd=2, bty='n', cex=0.85, xjust=0, y.intersp=0.75)

	# return list of results
	invisible(results)
}

