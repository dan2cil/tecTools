#' tempering
#'
#' Simulate chocolate crystallization using Imogen Foubert model
#'   of crystallization kinetics of cocoa butter
#'
#' @param BC cocoa butter contents (0-1)
#' @param t0  initial temperature of simulation (cooling) - 35 C
#' @param f0 initially present amount of crystals [J/g or \% solid fat] - 1\%
#' @param a value for f as t approaches infinity [J/g or \% solid fat] - 100\%
#' @param n order of reverse reaction, linked to asymmetry of curve [-] - 3
#' @param k rate constant [h-1] - 5
#'
#' @return temperature list
#'
#' @examples
#' #tempering(BC=0.42)
#'
#' @export

tempering <- function(BC=0.42, t0=35, f0=1, a=100, n=3, k=5, H=11){
    ## Purpose: Simulate chocolate crystallization
    ##          FUNCTION EXPORTED
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## BC   : cocoa butter contents (0-1)
    ## t0   : initial temperature of simulation (raffreddamento)         35°C
    ## f0   : initially present amount of crystals [J/g or % solid fat]    1%
    ## a    : value for f as t approaches infinity [J/g or % solid fat]  100%
    ## n    : order of reverse reaction, linked to asymmetry of curve [-]   3
    ## k    : rate constant [h-1]                                           5
    ## H    : calore assorbito dalla cella (valore sperimentale) cal       11
    ## Output:
    ##
    ## ----------------------------------------------------------------------
    ## History
    ##   v. 1.0  - 10 Aug 2012 initial version
    ##   v. 2.0  - 02 May 2016 better comment of program
    ##   v. 2.1  - 10 Mar 2019 H as input variable
    ##   v. 3.0  - 18 Apr 2019 add. graphics, expand parameter
    ## ToDo:
    ## ----------------------------------------------------------------------
    ## Author: Danilo Cillario, Date: 18 Apr 2019
  
    # prepara le variabili da espandere
    call <- match.call()
    
    # BC validity check
    if (any((BC < 0) | (BC > 1)))
        return(paste("ERROR: cocoa butter content is ", BC, ".  Should be between 0 and 1 (100%)", sep = ""))

    # costanti
    lambda <- 32.74379          # calore latente di cristallizzazione cal/g (pari a 137 J/g)
                                # Industrial Chocolate Manufacture and Use p.324 beta V
    cl <- 0.39                  # calore specifico cioccolato liquido cal/g °C
                                # 1674 W s -> 0.39 cal Confectionery and Chocolate Engineering p.584
    cs <- 0.52                  # calore specifico cioccolato solido cal/g °C
    ps <- 1.325                 # peso specifico cioccolato kg/dm3
    V  <- 15 * ps               # quantita di cioccolato nella coppetta (15 cc)
    #H  <- 11                    # calore assorbito dalla cella nell'unità di tempo
    #tempo <- 7                  # passo simulazione in sec (totale circa 7 * 40 = c.a. 5 minuti)
    #s <- 0.001                  # spessore cella porta campione m
    #lambdaAl <- 210             # coeff. di conducibilita' termica Alluminio W / m°C
    #lambdaCi <- 0.24            # coeff. di conducibilita' termica Cioccolato W / m°C
                                # Confectionery and Chocolate Engineering p.583
    #A <- 0.002                  # superficie coppetta porta campione m^2
    #T.cella <- 8                # temperatura della cella porta campione °C
    #fattore <- 4.1867981879537  # 1 cal = 4.1867981879537 W s

    # make a vector with all combination of arguments
    valori <- expand.grid(BC, t0, f0, a, n, k, H)
    names(valori) <- c('BC', 't0', 'f0', 'a', 'n', 'k', 'H')
    print(valori)
    
    # parametri della formula di Foubert
    # a <- 100                 # valore di f0 as t approaches infinity
    ritardo <- 0

    # inizio simulazione
    t <- seq(0, 1, 0.025)

    # calcolo percentuale di cristallizzazione del campione nel tempo
    cristallizzazione <- numeric(length(t))
    for (i in 1:length(t)){
        cristallizzazione[i] <- a*(1-((1+((1-f0/a)^(1-n)-1) * exp(-(1-n)*k*(t[i]-ritardo)))^(1/(1-n))))
    }
    # calcolo calore rilasciato dalla cella (calore latente di cristallizzazione) cal
    calore_rilasciato <- numeric(length(t))
    for (i in 2:length(t)){
        calore_rilasciato[i] <- (cristallizzazione[i]-cristallizzazione[i-1]) / 100 * V * BC * lambda
    }
    # calcolo vettore calore assorbito (funzione del bagno termostatico) cal
    calore_assorbito <- rep(H,length(t))

    # calcolo temperatura
    temperatura <- numeric(length(t))
    calore_assorbito2 <- numeric(length(t))
    temperatura[1] <- t0
    for (i in 2:length(t)){
        temperatura[i] <- temperatura [i-1] + (calore_rilasciato[i]/(cl*V)) - (calore_assorbito[i]/(cl*V))
        #calore_assorbito2[i] <- 7 * (lambdaAl * A * (temperatura[i] - 8) / s) / fattore
        #calore_assorbito2[i] <- (A * ((temperatura[i] - 8) / R)) / fattore
        #temperatura[i] <- temperatura [i-1] + (calore_rilasciato[i]/(cl*V)) - (calore_assorbito2[i-1]/(cl*V))
    }

    #risultato <- list (tempo=t, temperatura = temperatura, cristallizzazione = cristallizzazione,
    #                   cal.rilasciato = calore_rilasciato, cal.assorbito = calore_assorbito,
    #                   cal.assorbito2 = calore_assorbito2)
    risultato <- data.frame (tempo=t, 
                             temperatura = temperatura,
                             cristallizzazione = c(1,cristallizzazione[2:length(cristallizzazione)]-cristallizzazione[1:(length(cristallizzazione)-1)]),
                             cristallizzazione.cum = cristallizzazione,
                             cal.rilasciato = calore_rilasciato, cal.assorbito = calore_assorbito,
                             cal.assorbito2 = calore_assorbito2)
    
    # plot del risultato
    x <- t
    y <- temperatura
    p <- ggplot(data.frame(risultato)) +
        geom_line(aes(x=tempo,y=temperatura)) +
        geom_line(aes(x=tempo, y=cristallizzazione))
        labs(title='Tempering Simulator', x='Time', y='Temperature', subtitle=call)

    print(p)
    
    invisible (risultato)
}
