#' mosaicFB
#'
#' Simulate chocolate crystallization using Imogen Foubert model
#'   of crystallization kinetics of cocoa butter
#'
#' @param data       data frame of data
#' @param cluster    field to use to clusterize data
#' @param filter     data filter
#' @param depivot    boolean, if T depivot data, if F none
#' @param main       main title
#' @param sub        subtitle
#'
#' @return elaborated data frame
#'
#' @examples
#' #tempering(BC=0.42)
#'
#' @export

mosaicFB <- function(dati, cluster='plant', filter=NULL, depivot=T, main='Product fat-bloom', sub=''){
    ## ----------------------------------------------------------------------
    ## History
    ##   v. 1.0  - 10 aug 2012 initial version
    ## ToDo:
    ## ----------------------------------------------------------------------
    ## Author: Danilo Cillario, Date: 3 June 2017

    ## Filter example:
    ## filter="plant=='Alba'"
    ## filter="plant=='Alba' & linea %in% c('L1','L4')"

    # option
    options(encoding="utf-8")

    # costanti
    colore <- c(colours()[c(257, 254, 259)], rgb(1,.5,.5), rgb(1,.25,.25), rgb(1,0,0))

    # se necessario espande i dati
    if (depivot){
        dati <- depivot(dati)
    }

    # se necessario applica il filtro ai dati
    if (!is.null(filter)){
        dati <- subset(dati, eval(parse(text=filter)))
    }

    # se necessario crea un cluster composto
    if (grepl('+', cluster, fixed=T)){
        gsub(" ", "", cluster, fixed = TRUE)
        cluster2 <- unlist(strsplit(cluster, split='+', fixed=T))
        cluster3 <- interaction(dati[, cluster2], drop=T)
        dati <- cbind(dati, cluster3)
        cluster <- 'cluster3'
    }

    # elimina i livelli del cluster non utilizzati
    dati[, cluster] <- dati[,cluster][drop=T]

    # plotta il grafico
    if (length(dati[,cluster]) > 1){
        mosaicplot(table(dati[,c(cluster,'month', 'fat_bloom')]),
                   color = colore,
                   xlab = cluster,
                   ylab = 'Età prodotto',
                   main = main,
                   sub = sub)
    } else {
        print(paste('Non ci sono abbastanza dati del cluster:', length(dati[cluster])))
    }
    invisible(dati)
}

#' stackFB
#'
#' Simulate chocolate crystallization using Imogen Foubert model
#'   of crystallization kinetics of cocoa butter
#'
#' @param data        data frame of data
#' @param cluster     field used to clusterize data
#' @param filter      data filter
#' @param main        main title
#' @param new         field useed to change graph
#' @param tabOnGraph  boolean, if T print kruskal test result above graphics
#'
#' @return elaborated data frame
#'
#' @examples
#' #tempering(BC=0.42)
#'
#' @export

stackFB <- function(dati, cluster='plant', filter=NULL, main='Product fat-bloom', new='month', tabOnGraph=T){
    ## data: prodotto, mese, L1, L2, L3, L4, L5, L6
    ## ----------------------------------------------------------------------
    ## History
    ##   v. 1.0  - 10 aug 2012 initial version
    ## ToDo:
    ## 1. inserire anche paired t test oltre a kruskal
    ## ----------------------------------------------------------------------
    ## Author: Danilo Cillario, Date: 3 June 2017

    # controllo data frame
    #call <- match.call()
    #if (!"new" %in% names(call)) {new <- "mese"}

    # dati di base
    colori <- c(colours()[c(257, 254, 259)], rgb(1,.5,.5), rgb(1,.25,.25), rgb(1,0,0))

    # se necessario applica il filtro ai dati
    if (!is.null(filter)){
        dati <- subset(dati, eval(parse(text=filter)))
    }

    # se necessario aggrega i dati
    ncluster <- nlevels(dati[,cluster])
    nnew <- nlevels(dati[,new])
    if (length(dati[,1]) > ncluster * nnew){
        print('richiede aggregazione dei dati')
        dati <- aggregate(dati[,c('L1','L2','L3','L4','L5','L6')], by=list(dati[,cluster],dati[,new]), FUN=sum, na.rm=T)
        names(dati)[1:2] <- c(cluster, new)
        print(dati)
    } else {
        print('NON richiede aggregazione dei dati')
    }


    # rows and columns of graphics count
    nlivelli <- nlevels(as.factor(dati[,new]))     # numero di grafici
    #livelli <- sort(unique(dati[,new]))           # etichetta dei grafici
    livelli <- levels(as.factor(dati[,new]))
    n.prod <- nlevels(as.factor(dati[,cluster]))   # numero di barre nei grafici singoli

    righe <- trunc(sqrt(nlivelli))                 # numero delle righe
    colonne <- ceiling(nlivelli/righe)             # numero delle colonne
    quadri <- colonne * righe + 1                  # numero dei quadri

    # define layout for multiple plot
    if (Sys.info()[1]=='Linux') X11.options(width=7/righe*colonne, height=7) else windows.options(width=7/righe*colonne, height=7)
    par(mai=c(0, 0, 0, 0))
    if (tabOnGraph){
        se <- 2:((((righe*colonne)*2))+1)
        if (righe == 1){
            aa <- split(se, rep(c(1,2), times=colonne))
            bb <- c(aa[[1]], aa[[2]])}
        else if (righe == 2){
            aa <- split(se, c(rep(c(1,2), times=colonne), rep(c(3,4), times=colonne)))
            bb <- c(aa[[1]], aa[[2]], aa[[3]], aa[[4]])}
        else if (righe == 3){
            aa <- split(se, c(rep(c(1,2), times=colonne), rep(c(3,4), times=colonne), rep(c(5,6), times=colonne)))
            bb <- c(aa[[1]], aa[[2]], aa[[3]], aa[[4]], aa[[5]], aa[[6]])}
        else if (righe == 4){
            aa <- split(se, c(rep(c(1,2), times=colonne), rep(c(3,4), times=colonne), rep(c(5,6), times=colonne), rep(c(7,8), times=colonne)))
            bb <- c(aa[[1]], aa[[2]], aa[[3]], aa[[4]], aa[[5]], aa[[6]], aa[[7]], aa[[8]])}

        layout(matrix(c(rep(1,colonne), bb),byrow=T,nrow=(righe*2)+1), heights=c(1,rep(c(5,2), times=righe)))
        xpad <- c(.8,rep(1.25, times=n.prod))
    } else {
        layout(matrix(c(rep(1,colonne), 2:quadri),byrow=T,nrow=righe+1), heights=c(righe*0.75, rep(9, righe)))
    }
    # write main title on page
    plot(c(0,0), c(1,1), type="n", xlab="", ylab="", axes=FALSE, asp=1)
    mtext(main, side=3, line=-3, cex=1.5, font=4, col='red')

    if (tabOnGraph)
        par(mai=c(0.2, 0.3, 0.3, 0.1))
    else
        par(mai=c(0.4, 0.3, 0.3, 0.1))

    # Graphics plot
    for (graph in livelli){
        caso <- subset(dati, dati[,new]==graph, select=c('L1','L2','L3','L4','L5','L6'))
        nomi <- levels(subset(dati, dati[,new]==graph, select=cluster)[,cluster][, drop=T])

        print(caso)
        print(nomi)
        row.names(caso) <- NULL
        caso <- data.frame(t(caso))
        names(caso) <- nomi
        x <- as.matrix(caso)

        # mette i dati nell'ordine corretto
        #peso <- rep(0, ncol(x))
        #for (i in 1:ncol(x)){
        #    peso <- peso + x[,i]*i}
        #x1 <- x[order(peso),]
        y <- .pairkruskal(caso)

        #par( mai = c(0.6, 0.6, 0.8, 0.2))
        par( mai = c(0.4, 0.6, 0.4, 0.2))

        # Plot the graph
        barplot(x, col=colori, names.arg=nomi)

        # Scrivo i titoli
        sub <- paste(new, ': ', graph)
        mtext(sub, side=3, line=+.5, cex=.8, font=4, col='blue')


        # If required plot the p-value table on graph
        if (tabOnGraph){
            old_parmai <- par('mai')
            par( mai = c(0.1,0.0,0.1,0.1) )
            plot.new()
            .scrivi(0, 0, y, bty="o", lwd=0, display.rownames=TRUE, display.colnames=FALSE, hlines=FALSE, vlines=FALSE, xpad=xpad, ypad=1.2, title="Kruskal-test")
            par(mai = old_parmai)
        }
    }

    #return
}

## ---------------------------------------------------------------------
## Funzioni generiche (esterne alle classi)
## ---------------------------------------------------------------------

# pairkruskal pair comparison
.pairkruskal <- function(dati, print=F) {
    ## Purpose: confronta i prodotti a coppie applicando il kruskal.test
    ##          INTERNAL FUNCTION NOT EXPORTED
    ## -----------------------------------------------------------------
    ## Arguments:
    ## df  : Data frame
    ##       $ name1: int  2 2 3 4 3 1 2 4 2 4 ...
    ##       $ name2: int  4 4 1 3 4 2 1 1 1 3 ...
    ##       $ namen: int  3 3 1 3 4 2 1 3 4 3 ...
    ## -----------------------------------------------------------------
    ## Result:
    ##       binomial p table
    ## -----------------------------------------------------------------
    ## Author: Danilo Cillario,  Date  : 3 november 2012

    #df <- object@dat
    df <- dati
    #df1 <- df[,order(sapply(df,sum))]
    df1 <- df
    #nomi <- names(object@dat)
    nomi <- names(df1)
    #colonne <- object@prd
    colonne <- length(df)
    #ordine <- order()

    #preferenze <- matrix(nrow=colonne, ncol=colonne)
    risultato <- matrix(nrow=colonne, ncol=colonne)
    for (i in 1:(colonne-1)){
      for (ii in (i+1):colonne){
          prob1 <- kruskal.test(list(df1[,i],df1[,ii]))
          prob2 <- kruskal.test(list(df1[,ii],df1[,i]))
          #risultato[i,ii] <- round(prob1$p.value,4)
          #risultato[ii,i] <- round(prob2$p.value,4)}
          risultato[i,ii] <- sprintf("%.3f", prob1$p.value)
          risultato[ii,i] <- sprintf("%.3f", prob2$p.value)}}
    colnames(risultato) <- nomi
    rownames(risultato) <- nomi
    if(print){
      cat("\n*********************************\n  Kruskal pairs preferences table\n*********************************\n")
      print(risultato)
      cat("\n")}
    invisible(risultato)
}

#' depivot
#'
#' Sscompone gli elementi aggregati in singoli
#'
#' @param dati        data frame of data
#'
#' @return elaborated data frame
#'
#' @examples
#' #
#'
#' @export

depivot <- function(dati){
    dt <- na.omit(dati)
    provv <- NULL
    for (i in 1:length(dt[,1])){
        pezzi <- sum(dt[i, c('L1','L2','L3','L4','L5','L6')])
        dati_inizio <- which(names(dt)=='L1')
        range_dati <- dati_inizio:which(names(dt)=='L6')
        # da utilizzare solo se i dati sono in percentuale
        #provv <- rep(1:6, ro[i,range_dati]*pezzi/100)
        provv <- rep(1:6, dt[i, range_dati])
        for (ii in 1:pezzi) {
            riga <- c(dt[i, 1:(dati_inizio-1)], provv[ii])
            names(riga)[dati_inizio] <- 'fat_bloom'
            if (i==1 & ii==1)
                dt2 <- data.frame(riga, stringsAsFactors=F)
            else
                dt2 <- rbind(dt2, riga)
        }
    }
    return(dt2)
}

.depivot2 <- function(dati){
    dt <- na.omit(dati)
    inizio <- which(names(dati)=='L1')
    fine <- which(names(dati)=='L6')
    pezzi <- rowSums(dati[,inizio:fine])

    b[rep(seq_len(nrow(b)), c(2,3)),]

}

# Scrive tabella su immagine
.scrivi <- function (x, y = NULL, table, lwd = par("lwd"), bty = "n", bg = par("bg"),
                     cex = 1, xjust = 0, yjust = 1, xpad = 0.1, ypad = 0.5, box.col = par("fg"),
                     text.col = par("fg"), display.colnames = TRUE, display.rownames = FALSE,
                     hlines = FALSE, vlines = FALSE, title = NULL){
    ## Purpose: Write tabel into a graphics windows
    ##          INTERNAL FUNCTION NOT EXPORTED
    ## -----------------------------------------------------------------
    ## Arguments:
    ## x,y      : Either x and y coordinates to locate the table or an 'xy.coords' object
    ## table    : A data frame, matrix or similar object that will be displayed
    ## lwd      : The line width for the box and horizontal dividers
    ## bty      : Whether to draw a box around the table ("o") or not ("n")
    ## bg       : The background color for the table
    ## cex      : Character expansion for the table
    ## xjust,yjust : Positioning for the table relative to 'x,y'
    ## xpad,ypad   : The amount of padding around text in the cells as a proportion of the maximum
    ##                width and height of the strings in each column
    ## box.col     : The color for the box and lines
    ## text.col    : The color for the text
    ## display.colnames : Whether to display the column names in the table
    ## display.rownames : Whether to display the row names in the table
    ## hlines   :Whether to draw horizontal lines between each row of the table
    ## vlines   : Whether to draw vertical lines between each column of the table
    ## title    : Optional title placed over the table
    ## -----------------------------------------------------------------
    ## Variables:
    ##
    ## -----------------------------------------------------------------
    ## Author: Modified from Plotrix package

    if (dev.cur() == 1)
        stop("Cannot add table unless a graphics device is open")
    if (is.null(y)) {
        if (is.null(x$y))
            stop("both x and y coordinates must be given")
        y <- x$y
        x <- x$x
    }
    tabdim <- dim(table)
    # Aggiunta
    if (length(xpad)==1)
        xpad <- rep(xpad,(tabdim[2]+1))
    # fine
    if (is.null(dim(bg)))
        bg <- matrix(bg, nrow = tabdim[1], ncol = tabdim[2])
    column.names <- colnames(table)
    if (is.null(column.names) && display.colnames)
        column.names <- 1:tabdim[2]
    row.names <- rownames(table)
    if (is.null(row.names) && display.rownames)
        row.names <- 1:tabdim[1]
    if (par("xlog"))
        x <- log10(x)
    cellwidth <- rep(0, tabdim[2])
    ##
    if (display.colnames) {
        for (column in 1:tabdim[2]) cellwidth[column] <- max(strwidth(c(column.names[column],
                                                                        format(table[, column])), cex = cex)) * (1 + xpad[column+1])
        nvcells <- tabdim[2] + 1 }
    else {
        nvcells <- tabdim[2]
        for (column in 1:tabdim[2]) cellwidth[column] <- max(strwidth(format(table[,
                                                                                   column]), cex = cex)) * (1 + xpad[column+1]) }
    ##
    if (display.rownames) {
        nhcells <- tabdim[2] + 1
        rowname.width <- max(strwidth(row.names, cex = cex)) *
            (1 + xpad[1]) }
    else {
        nhcells <- tabdim[2]
        rowname.width <- 0 }

    if (par("ylog"))
        y <- log10(y)
    cellheight <- max(strheight(c(column.names, row.names, as.vector(unlist(table))),
                                cex = cex)) * (1 + ypad)
    ytop <- y + yjust * nvcells * cellheight
    oldpar <- par(xlog = FALSE, ylog = FALSE, xpd = TRUE)

    ## Scrive il contenuto delle celle
    for (row in 1:tabdim[1]) {
        xleft <- x - xjust * (sum(cellwidth) + rowname.width)
        if (row <= nvcells - 1 && hlines)
            segments(xleft + rowname.width, ytop - row * cellheight,
                     xleft + sum(cellwidth) + rowname.width, ytop -
                         row * cellheight, lwd = lwd, col = box.col)
        if (display.rownames) {
            text(xleft + 0.5 * rowname.width, ytop - (row + display.colnames -
                                                          0.5) * cellheight, row.names[row], cex = cex,
                 col = text.col)
            xleft <- xleft + rowname.width
        }
        for (column in 1:tabdim[2]) {
            rect(xleft, ytop - (row + display.colnames - 1) *
                     cellheight, xleft + cellwidth[column], ytop -
                     (row + display.colnames) * cellheight, col = bg[row,
                                                                     column])
            text(xleft + 0.5 * cellwidth[column], ytop - (row +
                                                              display.colnames - 0.5) * cellheight, table[row,
                                                                                                          column], cex = cex, col = ifelse(table[row,column]<=0.05 | table[row,column]>=0.95,'red',text.col))
            if (vlines)
                segments(xleft, ytop - (row + display.colnames) *
                             cellheight, xleft + cellwidth[column], ytop -
                             (row + display.colnames) * cellheight, col = box.col)
            xleft <- xleft + cellwidth[column]
        }
    }
    ## Stampa del nome delle colonne
    if (display.colnames) {
        xleft <- x - xjust * (sum(cellwidth) + rowname.width)
        for (column in 1:tabdim[2]) {
            text(xleft + display.rownames * rowname.width + cellwidth[column] *
                     0.5, ytop - 0.5 * cellheight, column.names[column],
                 cex = cex, col = text.col)
            if (!hlines)
                segments(xleft + rowname.width, ytop - cellheight,
                         xleft + cellwidth[column], ytop - cellheight,
                         lwd = lwd, col = box.col)
            xleft <- xleft + cellwidth[column]}}

    ## Stampa del titolo
    if (!is.null(title)) {
        xleft <- x - xjust * (sum(cellwidth) + rowname.width)
        text(xleft + rowname.width + (sum(cellwidth))/2, ytop +
                 cellheight/2, title, cex = cex, col = text.col)
        if (bty == "n")
            segments(xleft, ytop, xleft + sum(cellwidth) + rowname.width,
                     ytop, lwd = lwd, col = box.col)
    }
    par(oldpar)
}
