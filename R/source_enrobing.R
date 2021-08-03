#' enrobing
#'
#' Simulate enrobing indirect system
#'
#' @param input      vector of flow of fresh chocolate from tempering in kg/h - defuault = 1000 kg/h
#' @param output     vector of chocolate consumption in kg/h - default = 300/kg/h
#' @param ricircolo  vector of flow of reclicling pump in kg/h - default = 2000 kg/h
#' @param vasca      vector of capacity of enrobing tub in kg - default = 120 kg
#' @param minuti     minute of simulation - default = 600
#' @param xlim       x axes extension - defaul = Null (automatic)
#' @param ylim       y axes extension - defaul = Null (automatic)
#' @param debug      if T debug is on - default = F
#' @param gg         if T use ggplot2 if F use standard graphics - default = F
#'
#' @return dataframe (step, composition, percentage)
#'
#' @examples
#' # enrobing()
#'
#' @export
#'
enrobing <- function(input=1000, output=300, ricircolo=2000, vasca=120, minuti=600, xlim=NULL, ylim=NULL, debug=F, gg=F){
    # input     = portata temperatrice [kg/h]
    # output    = consumo  copertura su prodotto [kg/h]
    # ricircolo = portata pompe ricircolo [kg/h]
    # vasca     = capacità vasche enrobatrice [kg]
    # minuti    = minuti di simulazione [min]
    # debug     = debug=T, standard<>T
    # gg        = T use ggplot2, F use standard graphics

    # check input values
    if (input < output){
        print('Input must be >= of output')
        invisible()
    }

    # make a vector with all combination of temperature, humidity and pression
    valori <- expand.grid(input, output, ricircolo, vasca)
    names(valori) <- c('input', 'output', 'ricircolo', 'vasca')
    casi <- length(valori$input)

    # verify parameter
    fissi <- NULL
    mobili <- list()
    indice <- 1
    lista <- c('input', 'output', 'ricircolo', 'vasca')
    for (i in lista){
        if (length(get(i)) == 1){
            if (is.null(fissi)){
                fissi <- paste(fissi, i, '=', get(i), sep='')
            } else {
                fissi <- paste(fissi, ', ', i, '=', get(i), sep='')
            }
        } else {
            mobili[[indice]] <- paste(i, '=', get(i), sep='')
            indice <- indice + 1
        }
    }
    # generate the legenda's labels
    mobili <- expand.grid(mobili, stringsAsFactors = F)
    mobili <- switch(length(mobili),
        paste(mobili$Var1),
        paste(mobili$Var1, mobili$Var2),
        paste(mobili$Var1, mobili$Var2, mobili$Var3),
        paste(mobili$Var1, mobili$Var2, mobili$Var3, mobili$Var4)
    )
    if (is.null(mobili)){
        mobili <- 'Dati'
    }
    # declare results variable
    results <- list()
    results2 <- data.frame(caso=NULL, cicli=NULL, percentuale=NULL)

    # cycle on different cases
    for (c in 1:casi){
        inputM <- valori$input[c]/60
        outputM <- valori$output[c]/60
        ricircoloM <- valori$ricircolo[c]/60

        ritorno <- inputM - outputM
        composizione <- NULL
        composizione1 <- NULL
        composizione[1] <- valori$vasca[c]
        percentuale <- composizione / sum(composizione)

        # ciclo di iterazioni
        for (i in 2:minuti){
            #
            for (ii in 1:i){
                if (ii==1){
                    aggiunti <- inputM
                    stesso.livello.mantenuto <- composizione[ii] - (inputM * percentuale[ii])
                    a.livello.successivo <- (ricircoloM - outputM) * percentuale[ii]
                    composizione1[ii] <- aggiunti + stesso.livello.mantenuto - a.livello.successivo}
                else if (ii==i){
                    da.livello.precedente <- (ricircoloM - outputM) * percentuale[ii-1]
                    composizione1[ii] <- da.livello.precedente}
                else {
                    stesso.livello.mantenuto <- composizione[ii] - (inputM * percentuale[ii])
                    da.livello.precedente <- (ricircoloM - outputM) * percentuale[ii-1]
                    a.livello.successivo <- (ricircoloM - outputM) * percentuale[ii]
                    composizione1[ii] <- stesso.livello.mantenuto + da.livello.precedente - a.livello.successivo}}
            composizione <- composizione1
            percentuale <- composizione / sum(composizione)
            #cicli <- c(0:(cicliM-1))
            rcicli <- 0:14
            if (debug){
                print(composizione)
                print(percentuale)}}
        results[[c]] <- data.frame(cicli=rcicli[1:15], composizione=composizione[1:15], percentuale=percentuale[1:15])
        parziali <- data.frame(caso=as.numeric(rep(c, 15)), cicli=as.numeric(rcicli[1:15]), composizione= composizione[1:15], percentuale=percentuale[1:15])
        #results2 <- cbind(results2, percentuale[1:15])
        results2 <- rbind(results2, parziali)
    }

    results2 <- data.frame(results2)
    col <- rainbow(casi)
    #col <- topo.colors(casi)
    #col <- terrain.colors(casi)

    # preparazione del grafico
    if (is.null(xlim)){
        xlim = c(0, 14)
    }
    if (is.null(ylim)){
        ylim = c(0, ceiling(max(results2$percentuale[!is.na(results2$percentuale)])*10)/10)
    }
    if (gg){
        p <- ggplot2::ggplot(results2) +
            ggplot2::geom_point(ggplot2::aes(x=results2$cicli, y=results2$percentuale, color=col[results2$caso])) +
            #ggplot2::stat_smooth(ggplot2::aes(x=results2$cicli, y=results2$percentuale, color=results2$caso, group=results2$caso), method='lm', formula= y ~ splines::ns(x,12), se=F) +
            ggplot2::scale_x_continuous(breaks=seq(0, max(results2[, 2]), 1)) +
            ggplot2::scale_y_continuous(limits=c(0, 1)) +
            ggplot2::labs(title='Cicli vs rapporto input/output\nricircolo=200, vasca=200', x='cicli', y='frequenza')

        print(p)
    } else {
        # par(bg='#05050550')
        x <- results[[1]]$cicli
        y <- results[[1]]$percentuale
        plot(y ~ x, pch=19, col=col[1], xlim=xlim, ylim=ylim, axes=F, xlab = "", ylab = "")
        par(new=T)
        plot(lowess(x, y, f=0.1), type='l', col=col[1], lwd=2, xlim=xlim, ylim=ylim, axes=F, xlab = "", ylab = "")
        if (length(results)>1){
            for (i in 2:length(results)){
                par(new=T)
                x <- results[[i]]$cicli
                y <- results[[i]]$percentuale
                plot(y ~ x, pch=19, col=col[i], xlim=xlim, ylim=ylim, axes=F, xlab = "", ylab = "")
                par(new=T)
                plot(lowess(x, y, f=0.1), type='l', col=col[i], lwd=2, xlim=xlim, ylim=ylim, axes=F, xlab = "", ylab = "")
            }
        }
        # y axis left
        axis(side=2, at=seq(0, 1, .1), labels=seq(0, 1, .1), col.ticks="#0000FF50", cex.axis=0.8)
        #axis(side=2, at=seq(y.lim[['start.sec']], y.lim[['stop.sec']], y.lim[['step.sec']]), labels=seq(y.lim[['start.sec']], y.lim[['stop.sec']], y.lim[['step.sec']]), lwd.ticks=2)

        # x axis down
        axis(side=1, at=seq(0, 14, 1), labels=seq(0, 14, 1), col.ticks="#0000FF50", cex.axis=0.8)
        #axis(side=1, at=seq(x.lim[['start']], x.lim[['stop']], x.lim[['step.sec']]), labels=FALSE, lwd.ticks=2)

        # print title
        title(main=paste('Chocolate cycles in indirect enrobing\n', fissi), xlab='chocolate cicle', ylab='frequency', cex.lab=0.8)
        grid(col='white')

        # print legend
        legend('topright', legend=mobili, col=col, lwd=3, bty='n', cex=0.9)

        # close the box as usual
        #box(bg='#10101050')
        rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col='#10101020', border='black')
        grid(col='white')
    }

    invisible(results2)
}
