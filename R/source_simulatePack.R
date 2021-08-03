#' simulatePack
#'
#' Simulate the weight of a mixed box containing different product
#' using the weight of all the single product
#'
#' @param source a data frame with the weight of the single praline
#' @param composition  a data frame with the composition of the pack
#' @param declared declared weight
#' @param plotce boolean, T if you want to plot T1 and T2, otherwise F
#' @param main   optional main title
#' @param step simulation steps, number of pack to be simulated
#'
#' @return data list
#'
#' @examples
#' # data(source, composition)
#' # simulatePack(source, composition, 136)
#'
#' @export

simulatePack <- function(source, composition, declared, plotce=F, main='Pesi medi', step=100000){
    # variabili di supporto
    tot_prod <- sum(composition)
    # define le composition list
    lista <- rep(names(composition), composition[1,])
    # contare i valori per ogni colonna della tabella 'source'

    # genera la matrice dei risultati
    output <- matrix(NA_real_, nrow =step, ncol=tot_prod)
    colnames(output) <- lista
    # loop per generare la matrice dei dati
    for (i in 1:tot_prod){
        campione <- sample(source[,lista[i]], step, replace=TRUE)
        output[,i] <- campione
    }

    # calcola i pesi medi e i vari indici da stampare
    pesi_medi <- margin.table(output, 1)
    media <- mean(pesi_medi)
    stddev <- sd(pesi_medi)
    sigma3 <- round(media-3*stddev, 1)
    sigma4 <- round(media-4*stddev, 1)
    if (declared==sigma3 | declared==sigma4)
        overlay = T
    else
        overlay = F
    T1 <- declared - err_ce(declared)
    T2 <- T1 - err_ce(declared)
    prealdeclared <- round(length(pesi_medi[pesi_medi<declared])/step*100, 4)
    prealsigma3 <- round(length(pesi_medi[pesi_medi<sigma3])/step*100, 4)
    prealsigma4 <- round(length(pesi_medi[pesi_medi<sigma4])/step*100, 4)
    pnormdeclared <- round(pnorm(declared, media, stddev)*100, 4)
    pnormsigma3 <- round(pnorm(sigma3, media, stddev)*100, 4)
    pnormsigma4 <- round(pnorm(sigma4, media, stddev)*100, 4)

    # plotta il grafico
    if (plotce){
        min_x <- min(c(pesi_medi, declared, T1, T2))
        max_x <- max(c(pesi_medi, declared, T1, T2))
    } else {
        min_x <- min(c(pesi_medi, declared))
        max_x <- max(c(pesi_medi, declared))
    }
    hist(pesi_medi, freq=F, xlim=c(min_x, max_x), xlab='weight', ylab='freq', main=main)
    # linee di riferimento
    if (overlay==F){
        lines(rep(declared, 2), c(0, 0.25), col='red')
    }else{
        lines(rep(declared, 2), c(0.23, 0.25), col='red')
    }
    lines(rep(sigma3, 2), c(0, 0.20), col='blue')
    lines(rep(sigma4, 2), c(0, 0.20), col='blue')
    if (plotce){
        lines(rep(T1, 2), c(0, 0.20), col='green')
        lines(rep(T2, 2), c(0, 0.20), col='green')
    }
    # scrive il testo nel grafico
    text(declared, .25, paste('declared\n', declared, 'g', sep=''), adj=c(0.5,0), cex=0.6)
    pippo <- paste('\n', sigma3, 'g', sep='')
    text(sigma3, .20, bquote(atop('3 ' * sigma, .(sigma3) * 'g')), adj=c(0.5,0), cex=0.6)
    text(sigma4, .20, bquote(atop('4 ' * sigma, .(sigma4) * 'g')), adj=c(0.5,0), cex=0.6)
    #text(declared, .25, paste('declared\np=', prealdeclared, '% calc\np=', pnormdeclared,'% norm', sep=''), adj=c(0.5,0), cex=0.6)
    #text(sigma3, .20, paste('3 sigma\np=', prealsigma3, '% calc\np=', pnormsigma3,'% norm', sep=''), adj=c(0.5,0), cex=0.6)
    #text(sigma4, .20, paste('4 sigma\np=', prealsigma4, '% calc\np=', pnormsigma4,'% norm', sep=''), adj=c(0.5,0), cex=0.6)
    if (plotce){
        text(T1, .20, paste('T1\n', T1, 'g', sep=''), adj=c(0.5,0), cex=0.6)
        text(T2, .20, paste('T2\n', T2, 'g', sep=''), adj=c(0.5,0), cex=0.6)
    }

    # plotta la normale
    #x <- hist(pesi_medi, plot=F)$mids
    x <- seq(par('xaxp')[1], par('xaxp')[2], length.out=200)
    y <- dnorm(x, media, stddev)
    par(new=T)
    lines(x, y, col='green')

    # stampa risultati
    print(paste('Are be generated ', step, ' sample with mean=', round(media, 4), ' and sd=', round(stddev,4), sep=''))
    print(paste('Probability at declared(', declared, 'g): ', prealdeclared, '% real ', pnormdeclared, ' % normal', sep=''))
    print(paste('Probability at  3 sigma(', sigma3, 'g): ', prealsigma3, '% real ', pnormsigma3, ' % normal', sep=''))
    print(paste('Probability at  4 sigma(', sigma4, 'g): ', prealsigma4, '% real ', pnormsigma4, ' % normal', sep=''))
    print(paste('T1 = ', T1, 'g', sep=''))
    print(paste('T2 = ', T2, 'g', sep=''))

    invisible(list(pesi_medi=pesi_medi, matrice=output))
}

#' simulateMH
#'
#' Simulate a multi-head weighting machine producing a box of pralines
#' the result of the simulation is a data.frame containing
#' the weight and numbers of praline for each box simulated
#'
#' @param source vector with the weight of the single praline
#' @param target  the target weight of the multi-head machine
#' @param declared declared weight of the box (unused)
#' @param min.pcs declared pieces of the box (unused)
#' @param step simulation steps, number of pack to be simulated
#'
#' @return data.frame width data simulation results
#'
#' @examples
#' # dati <- dati <- rnorm(100, 10.1, 0.5)
#' # b <- simulateMH(dati, 152, 150, 15)
#'
#' @export

simulateMH <- function(source, target, declared, min.pcs, step=1000){

    # verifica dei dati inseriti
    if (! is.vector(source)){
        print('Error! source will be a vector')
        return()
    }
    # variabili di supporto
    p_medio <- mean(source)
    # define il numero di pezzi da generare automaticamente
    pcs <- floor((target / p_medio)) - 2
    # contare i valori per ogni colonna della tabella 'source'

    # matrice dei singoli campioni
    conf <- matrix(NA_real_, nrow =step, ncol=pcs)

    # genera la matrice dei risultati
    output <- matrix(NA_real_, nrow =step, ncol=2)
    colnames(output) <- c('peso', 'pezzi')

    # loop per generare la matrice dei dati
    for (i in 1:pcs){
        conf[,i] <- sample(source, step, replace=TRUE)
    }

    output[,1] <- rowSums(conf)
    output[,2] <- rep(pcs, step)

    # loop per completare i pezzi nel prodotto
    for (i in 1: step){
        while(output[i, 1] < target){
            output[i, 1] <- output[i, 1] + sample(source, 1)
            output[i, 2] <- output[i, 2] +1
        }
    }

    # stampa dei risultati
    print(paste('Are be generated ', step, ' sample with mean=', round(mean(output[,1]), 4), ' and sd=', round(sd(output[,1]),4), sep=''))
    print(paste(length(output[,2][output[,2] < min.pcs]), ' box contains less than ', min.pcs, ' pieces', sep=''))

    dati <- data.frame(output)
    b <- tapply(dati$pezzi, dati$pezzi, length)
    n.pezzi <- as.numeric(names(b))
    frequenza <- as.numeric(b)
    frequenza.perc <- frequenza/sum(frequenza)
    media <- as.numeric(tapply(dati$peso, dati$pezzi, mean))
    dev.std <- as.numeric(tapply(dati$peso, dati$pezzi, sd))

    invisible(list(data=dati, results=data.frame(n.pezzi, frequenza, frequenza.perc, media, dev.std)))
}


#' err_ce
#'
#' Compute tollerance of the weight
#'
#' this is detail field bla bla bla
#'   bla bla
#'
#' @param decweight    the declared weight
#'
#' @return error
#'
#' @examples
#' #err_ce(200)
#'

err_ce <- function(decweight){
    if ((decweight > 5) & (decweight <= 50))
        error <- ceiling(decweight * 0.09 *10) / 10
    else if (decweight <= 100)
        error <- 4.5
    else if (decweight <= 200)
        error <- ceiling(decweight * 0.045 *10) / 10
    else if (decweight <= 300)
        error <- 9
    else if (decweight <= 500)
        error <- ceiling(decweight * 0.03 *10) / 10
    else if (decweight <= 1000)
        error <- 15
    else if (decweight <= 10000)
        error <- ceiling(decweight * 0.015 *10) / 10

    return(error)
}
