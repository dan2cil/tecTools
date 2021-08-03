#' simHollow
#'
#' Simulate molds movements during hollow figure production
#'
#' @param g        define gravity vector
#' @param rpm1     asse x - 0= no rotazione >0 rotazione oraria <0 rotazione antioraria
#' @param rpm2     asse z - 0= no rotazione >0 rotazione oraria <0 rotazione antioraria
#' @param rpm3     asse y - 0= no rotazione >0 rotazione oraria <0 rotazione antioraria
#' @param sim      totale tempo simulato in secondi
#' @param step     passo di simulazione in minuti
#' @param seq      if True plot 'sequential' graph if False plot 'stellar' graph
#' @param d3       if True plot 3d graph if False plot 2d graph
#'
#'
#' @return coordinate
#'
#' @examples
#' #simHollow()
#'
#' @export

simHollow <- function(g=matrix(c(0,-1,0), ncol=1), rpm1=1, rpm2=6, rpm3=0, sim=120, step=2, seq = F, d3=T){
    ## ----------------------------------------------------------------------
    ## History
    ##   v. 1.0  - 13 aug 2017 initial version
    ## ToDo:
    ## ----------------------------------------------------------------------
    ## Author: Danilo Cillario, Date: 13 aug 2017

    # librerie
    library(ggplot2)
    # stampa 3d
    #d3=T
    # vettore iniziale
    #a <- matrix(c(0,0,-1), ncol=1)
    #a <- matrix(c(1,0,0), ncol=1)
    # rotazioni al minuto intorno all'asse x (dei bracci)
    #rpm1 <- 1
    # rotazione al minuto intorno all'asse z (spin)
    #rpm2 <- 6
    # rotazione al minuto intorno all'asse y
    #rpm3 <- 0
    # tempo totalee simulato (secondi)
    #sim <- 120
    # passo di simulazione (secondi)
    #step <- 1

    # preparazione variabili --------------------------------------------------
    # passi simulati
    tempo <- 1 + (sim / step)
    # alfa - rotazione intorno all'asse x (braccia)
    ang_deg <- (rpm1 * 360 * sim / 60) / (sim / step)      # gradi
    ang_rad <- ang_deg * pi / 180                          # radianti
    alfa_sec <- seq(0, sim, step)                          # secondi
    alfa_deg <- seq(0, (360*rpm1*sim/60), ang_deg) %% 360  # gradi
    alfa_rad <- alfa_deg * pi / 180                        # radianti
    # beta - rotazione intorno all'asse z (spin)
    bang_deg <- (rpm2 * 360 * sim / 60) / (sim / step)     # gradi
    bang_rad <- bang_deg * pi / 180                        # radianti
    beta_sec <- seq(0, sim, step)                          # secondi
    beta_deg <- seq(0, (360*rpm2*sim/60), bang_deg) %% 360 # gradi
    beta_rad <- beta_deg * pi / 180                        # radianti
    # gamma - rotazione intorno all'asse y
    gang_deg <- (rpm3 * 360 * sim / 60) / (sim / step)     # gradi
    gang_rad <- gang_deg * pi / 180                        # radianti
    gamma_sec <- seq(0, sim, step)                          # secondi
    gamma_deg <- seq(0, (360*rpm3*sim/60), gang_deg) %% 360 # gradi
    gamma_rad <- gamma_deg * pi / 180                        # radianti

    # matrici risulato della simulazione
    Z <- matrix(nrow=3, ncol=tempo, dimnames=list(c('x','y','z'), alfa_sec))
    Zseq <- Z
    Zseq2 <- Z

    # sequenza di simulazione -------------------------------------------------
    Z[,1] <- g
    Zseq[,1] <- g
    Zseq2[,1] <- matrix(c(0,0,0), ncol=1)
    # ciclo di lavoro
    for (i in 2:tempo){
        # matrice di rotazione sull'asse x
        if (rpm1==0){
            rX <- matrix(c(1,0,0,
                          0,1,0,
                          0,0,1), ncol=3)
        }else if (rpm1>0) {
            rX <- matrix(c(1,0,0,
                          0,cos(alfa_rad[i]),sin(alfa_rad[i]),
                          0,-sin(alfa_rad[i]),cos(alfa_rad[i])), ncol=3)
        }else if (rpm1<0){
            rX <- matrix(c(1,0,0,
                          0,cos(alfa_rad[i]),-sin(alfa_rad[i]),
                          0,sin(alfa_rad[i]),cos(alfa_rad[i])), ncol=3)}
        # matrice di rotazione sull'asse z (spin)
        if (rpm2==0){
            rZ <- matrix(c(1,0,0,
                          0,1,0,
                          0,0,1), ncol=3)
        }else if (rpm2>0){
            rZ <- matrix(c(cos(beta_rad[i]),sin(beta_rad[i]), 0,
                          -sin(beta_rad[i]), cos(beta_rad[i]), 0,
                          0, 0, 1), ncol=3)
        }else if (rpm2<0){
            rZ <- matrix(c(cos(beta_rad[i]), -sin(beta_rad[i]),0,
                          sin(beta_rad[i]), cos(beta_rad[i]), 0,
                          0, 0, 1), ncol=3)}
        # matrice di rotazione sull'asse y
        if (rpm3==0){
            rY <- matrix(c(1,0,0,
                          0,1,0,
                          0,0,1), ncol=3)
        }else if (rpm3>0){
            rY <- matrix(c(cos(gamma_rad[i]),0,-sin(gamma_rad[i]),
                           0,1,0,
                           sin(gamma_rad[i]), 0,cos(gamma_rad[i])), ncol=3)
        }else if (rpm3<0){
            rY <- matrix(c(cos(gamma_rad[i]),0,sin(gamma_rad[i]),
                           0,1,0,
                           -sin(gamma_rad[i]), 0,cos(gamma_rad[i])), ncol=3)}
        # matrice dello spostamento complessivo
        Cxyz <- rZ %*% rY %*% rX

        #tmp <- A %*% Z[,i-1]
        #Z[,i] <- B %*% tmp
        Z[,i] <- Cxyz %*% Z[,1]
        Zseq[,i] <- Z[,i] + Z[,i-1]
        Zseq2[,i] <- Zseq[,i-1]
    }

    # trasforma la matrice dei dati in un data frame
    Z1 <- as.data.frame(t(Z))
    Z2 <- as.data.frame(t(Zseq))
    Z3 <- as.data.frame(t(Zseq2))

    # grafico bidimensione ggplot2 --------------------------------------------
    if (!d3 & !seq){
        # stellar plot
        plot_base <- ggplot(data=Z1)
        xy <- plot_base + geom_segment(mapping=aes(x=0, y=0, xend=x, yend=y),
                                       arrow = arrow(length = unit(0.5, "cm")),
                                       colour="grey") +
                          geom_text(mapping=aes(x=x, y=y, label=alfa_sec),size=3) +
                          labs(title='Piano xy', x='x', y='y')
        yz <- plot_base + geom_segment(mapping=aes(x=0, y=0, xend=y, yend=z),
                                       arrow = arrow(length = unit(0.5, "cm")),
                                       colour="grey") +
                          geom_text(mapping=aes(x=y, y=z, label=alfa_sec),size=3) +
                          labs(title='Piano yz', x='y', y='z')
        xz <- plot_base + geom_segment(mapping=aes(x=0, y=0, xend=x, yend=z),
                                     arrow = arrow(length = unit(0.5, "cm")),
                                     colour="grey") +
                          geom_text(mapping=aes(x=x, y=z, label=alfa_sec),size=3) +
                          labs(title='Piano xz', x='x', y='z')
        p <- gridExtra::grid.arrange(xy, yz, xz, ncol=3)
        p
        #multiplot(xy, yz, xz, cols=1)
    }
    if (!d3 & seq){
        # sequenzal plot
        plot_base <- ggplot(data=Z2)
        xy <- plot_base + geom_segment(mapping=aes(x=Z3$x, y=Z3$y, xend=x, yend=y),
                                       arrow = arrow(length = unit(0.5, "cm")),
                                       colour="grey") +
          geom_text(mapping=aes(x=x, y=y, label=alfa_sec),size=3) +
          labs(title='Piano xy', x='x', y='y')
        yz <- plot_base + geom_segment(mapping=aes(x=Z3$y, y=Z3$z, xend=y, yend=z),
                                       arrow = arrow(length = unit(0.5, "cm")),
                                       colour="grey") +
          geom_text(mapping=aes(x=y, y=z, label=alfa_sec),size=3) +
          labs(title='Piano yz', x='y', y='z')
        xz <- plot_base + geom_segment(mapping=aes(x=Z3$x, y=Z3$z, xend=x, yend=z),
                                       arrow = arrow(length = unit(0.5, "cm")),
                                       colour="grey") +
          geom_text(mapping=aes(x=x, y=z, label=alfa_sec),size=3) +
          labs(title='Piano xz', x='x', y='z')
        q <- gridExtra::grid.arrange(xy, yz, xz, ncol=3)
        q
    }

    # if request plot the 3D graphics -----------------------------------------
    if (d3 & !seq){
      # sequential plot
      x0 <- rep(0, tempo)
      y0 <- rep(0, tempo)
      z0 <- rep(0, tempo)

      plot3D::arrows3D(x0, y0, z0, Z1$x, Z1$y, Z1$z,
                       lwd = 2, d = 3, clab = c("Quality", "score"),
                       xlim=c(-2,2), ylim=c(-2,2), zlim=c(-2,2),
                       main = "Gravity vector", bty ="g", ticktype = "detailed")

      # Add starting point of arrow
      #points3D(x0, y0, z0, add = TRUE, col="darkred",
      #         colkey = FALSE, pch = 19, cex = 1)

      library(plot3Drgl)
      plotrgl()
    }


    if (d3 & seq){
        # sequential plot
        #x0 <- rep(0, tempo)
        #y0 <- rep(0, tempo)
        #z0 <- rep(0, tempo)
        x0 <- Z3$x
        y0 <- Z3$y
        z0 <- Z3$z

        plot3D::arrows3D(x0, y0, z0, Z2$x, Z2$y, Z2$z,
                         lwd = 2, d = 3, clab = c("Quality", "score"),
                         xlim=c(-2,2), ylim=c(-2,2), zlim=c(-2,2),
                         main = "Gravity vector", bty ="g", ticktype = "detailed")

        # Add starting point of arrow
        #points3D(x0, y0, z0, add = TRUE, col="darkred",
        #         colkey = FALSE, pch = 19, cex = 1)

        library(plot3Drgl)
        plotrgl()
    }

    invisible(Z)
}


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)

    numPlots = length(plots)

    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }

    if (numPlots==1) {
        print(plots[[1]])

    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}
