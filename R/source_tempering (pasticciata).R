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

tempering <- function(BC=0.42, t0=35, f0=1, a=100, n=3, k=5){
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
    ##
    ## Output:
    ##
    ## ----------------------------------------------------------------------
    ## History
    ##   v. 1.0  - 10 aug 2012 initial version
    ##   v. 2.0  - 02 may 2016 better comment of program
    ## ToDo:
    ## ----------------------------------------------------------------------
    ## Author: Danilo Cillario, Date: 11 May 2016

    # costanti
    lambda <- 32.74379          # calore latente di cristallizzazione BC cal/g (pari a 137 J/g)
                                # Industrial Chocolate Manufacture and Use p.278 beta V
    cl <- 0.39                  # calore specifico cioccolato liquido cal/g °C
                                # 1674 W s -> 0.39 cal Confectionery and Chocolate Engineering p.584
    cs <- 0.52                  # calore specifico cioccolato solido cal/g °C
    ps <- 1.325                 # peso specifico cioccolato kg/dm3
    V  <- 15 * ps               # quantita di cioccolato nella coppetta (15 cc)
    H  <- 11                    # calore assorbito dalla cella nell'unità di tempo
    #tempo <- 7                 # passo simulazione in sec (totale circa 7 * 40 = c.a. 5 minuti)
    s <- 0.001                  # spessore cella porta campione m
                                # Confectionery and Chocolate Engineering p.583

    # calcoli per ipotesi alternative
    # R <- s/lambdaAl + 0.0015/lambdaCi
    #tempo <- 7                 # passo simulazione in sec (totale circa 7 * 40 = c.a. 5 minuti)
    #lambdaAl <- 210             # coeff. di conducibilita' termica Alluminio W / m°C
    #lambdaCi <- 0.24            # coeff. di conducibilita' termica Cioccolato W / m°C
    #A <- 0.002                  # superficie coppetta porta campione m^2
    #T.cella <- 8                # temperatura della cella porta campione °C
    #fattore <- 4.1867981879537  # 1 cal = 4.1867981879537 W s

    # parametri della formula di Foubert
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
    calore_assorbito <- rep(H, length(t))

    # calcolo temperatura
    temperatura <- numeric(length(t))
    #temperatura2 <- numeric(length(t))
    #calore_assorbito2 <- numeric(length(t))
    #calore_assorbito3 <- numeric(length(t))
    temperatura[1] <- t0
    #temperatura2[1] <- t0
    #calore_assorbito3[1] <- 7 * (A * ((temperatura2[1] - 8) / R)) / fattore
    for (i in 2:length(t)){
        temperatura[i] <- temperatura [i-1] + (calore_rilasciato[i]/(cl*V)) - (calore_assorbito[i]/(cl*V))
        # ipotesi di calcolo con formula Q = lambda ((T1 - T2) / s)
        #calore_assorbito2[i] <- 7 * (lambdaAl * A * (temperatura[i] - 8) / s) / fattore
        # ipotesi di calcolo con formula Q = A ((T1 - T2) / R)
        # con R = s/lambdaAl + l/lambdaCi
        #calore_assorbito3[i] <- 7 * (A * ((temperatura2[i-1] - 8) / R)) / fattore
        #temperatura2[i] <- temperatura2[i-1] + (calore_rilasciato[i]/(cl*V)) - (calore_assorbito3[i-1]/(cl*V))
    }

    risultato <- list (temperatura = temperatura, cristallizzazione = cristallizzazione,
                       cal.rilasciato = calore_rilasciato, cal.assorbito = calore_assorbito
                       #cal.assorbito2 = calore_assorbito2,
                       #cal.assorbito3 = calore_assorbito3,
                       #temperatura2 = temperatura2
                       )

    return (risultato)
}
