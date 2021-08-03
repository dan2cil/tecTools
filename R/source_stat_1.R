# http://www.listendata.com/2015/09/linear-regression-with-r.html
# http://jamesmarquezportfolio.com/correlation_matrices_in_r.html

lm.simple <- function (x, y, show.res = FALSE, show.ci = FALSE, conf.level = 0.95, pred = FALSE)
{
    op <- par()
    ord <- order(x)
    x <- x[ord]
    y <- y[ord]
    tmp.lm <- lm(y ~ x)
    if (show.res) {
        par(mfrow = c(2, 2))
    }
    plot(x, y, col='red')
    abline(tmp.lm,col='blue')
    if (show.ci) {
        xvals <- seq(min(x), max(y), length = 30)
        curve(predict(tmp.lm, data.frame(x = x), level = conf.level,
                      interval = "confidence")[, 3], col='grey', add = TRUE)
        curve(predict(tmp.lm, data.frame(x = x), level = conf.level,
                      interval = "confidence")[, 2], col='grey', add = TRUE)
        curve(predict(tmp.lm, data.frame(x = x), level = conf.level,
                      interval = "prediction")[, 3], col='grey', lty = 3, add = TRUE)
        curve(predict(tmp.lm, data.frame(x = x), level = conf.level,
                      interval = "prediction")[, 2], col='grey', lty = 3, add = TRUE)
    }
    coeffs <- floor(tmp.lm$coeff * 100 + 0.5)/100
    plusorminus <- c("+")
    if (coeffs[1] < 0)
        plusorminus <- c("")
    title(paste("y = ", coeffs[2], "x ", plusorminus, coeffs[1]))
    if (show.res) {
        Fitted <- fitted.values(tmp.lm)
        Residuals <- residuals(tmp.lm)
        plot(Fitted, Residuals)
        abline(h = 0)
        title("Residuals vs. fitted")
        hist(Residuals, main = "hist of residuals")
        qqnorm(Residuals, main = "normal plot of residuals")
        qqline(Residuals)
    }
    if (pred) {
        print(predict(tmp.lm, data.frame(x = pred)))
    }
    tmp.lm
}

pa <- function(c,n,difpop,lotto=1.0e10,tipo="automatico")
{	# Probabilita' di accettazione di un piano di campionamento
    # c = numero di difetti
    # n = numero di pezzi campionati
    # difpop = difettosit� della popolazione
    # lotto = numero di pezzi componenti il lotto (per ipergeometrica)
    # tipo = algoritmo di calcolo
    if (tipo=="automatico")
    {	if (n/lotto >= 0.1)
        tipo = "ipergeometrica"
    else
        tipo = ifelse(difpop <= 0.1, "binomiale", "poisson")}
    switch(tipo,
           binomiale  = pbinom(c,n,difpop),
           poisson = ppois(c,n*difpop),
           ipergeometrica = phyper(c,difpop*lotto,(1-difpop)*lotto,n))
}

pr <- function(c,n,difpop,lotto=1.0e10,tipo="automatico")
{	# Probabilita' di rifiuto di un piano di campionamento
    1-pa(c,n,difpop,lotto,tipo) }

difettosita <- function(c,n,probacc,lotto=1e10,tipo="automatico",errore=0.000001)
{	# Data una distribuzione binomiale restituisce la difettosita' della popolazione
    # che determina la probabilita' di accettazione data = probacc
    # c = numero di pezzi difettosi trovati
    # n = ampiezza del campione
    # probacc = probabilita' di accettazione voluta
    paSup <- 1; difSup <- 0
    paInf <- 0; difInf <- 1
    errCen<-1
    while (abs(errCen) > errore)
    {	difCen <- difSup+((difInf-difSup)/2)
    paCen <- pa(c,n,difCen)
    errCen <- paCen-probacc
    if (errCen!=0)
    {	if (errCen>0)
    { paSup=paCen; difSup=difCen }
        else
        { paInf=paCen; difInf=difCen }
    }
    }
    difCen
}

#' pa2pa
#'
#' Compare two attributive sample plan
#'
#' @param na1     numero di difetti accettati piano di campionamento 1
#' @param pz1     numero di pezzi campionati piano di campionamento 1
#' @param na2
#' @param pz2
#' @param xlim    scala dell'asse x
#' @param alfa    errore alfa ammesso
#'
#' @return none
#'
#' @examples
#' # pa2pa(0, 125, 3, 125)
#'
#' @export
#'

pa2pa <- function(na1, pz1, na2, pz2, xlim=c(0, 0.1), alfa=0.05)
{	if (na1/pz1 > na2/pz2)
        {# Inverte i due campionamenti
        i <- na1; ii <- pz1
        na1 <- na2; pz1 <- pz2
        na2 <- i; pz2 <- ii}

    #Calcolo delle differenze
    IvsII <- pr(na2, pz2, difettosita(na1, pz1, alfa))
    IIvsI <- pa(na1, pz1, difettosita(na2, pz2, 1 - alfa))

    # Grafico I vs II
    cv <- difettosita(na1,pz1,alfa) # Valore di
    y1<-dbinom(na1,pz1,na1/pz1)     # Valore massimo della densità della distribuzione I:na1-pz1
    y2<-dbinom(na2,pz2,na2/pz2)     # Valore massimo della densità della distribuzione II:na2-pz2
    if (y1>=y2)
        {curve(dbinom(na1,pz1,x),from=xlim[1],to=xlim[2],n=1000,main='Confronto distribuzioni binomiali\nIvsII',col='blue',xlab='Valori',ylab='Densita',bty='n',yaxt='n')
        curve(dbinom(na2,pz2,x),from=xlim[1],to=xlim[2],n=1000,col='red',add=T)}
    else
        {curve(dbinom(na2,pz2,x),from=xlim[1],to=xlim[2],n=1000,main='Confronto distribuzioni binomiali\nIvsII',col='red',xlab='Valori',ylab='Densita',bty='n',yaxt='n')
        curve(dbinom(na1,pz1,x),from=xlim[1],to=xlim[2],n=1000,col='blue',add=T)}

    lines(c(na1/pz1,na1/pz1),c(0,y1),col='blue',lty=2)                                    # Linea verticale dal valore di densita massimo della distribuzione I
    text(na1/pz1,y1,labels=paste('I: c=',na1,', n=',pz1),adj=c(.5,-0.2),cex=c(0.8,0.8))   # Testo identificativo della distribuzione
    lines(c(na2/pz2,na2/pz2),c(0,y2),col='red',lty=2)                                     # Linea verticale dal valore di densita massimo della distribuzione II
    text(na2/pz2,y2,labels=paste('II: c=',na2,', n=',pz2),adj=c(.5,-0.2),cex=c(0.8,0.8))  # Testo identificativo della distribuzione

    a <- seq(from=cv,to=xlim[2],length=1000)
    x <- c(a, xlim[2], cv)
    y <- c(dbinom(na1, pz1, a)*1,0.0,0.0)
    polygon(x,y,col='blue',border=NA)
    x4 <- difettosita(na1, pz1, alfa/2)
    y4 <- dbinom(na1,pz1,x4)
    arrows(x4,y4,x4*1.1,y4*1.1,length=0,code=1,col='blue')                                #
    text(x4*1.1,y4*1.1,labels=paste('pI = ',alfa),adj=c(0,0),col='blue',cex=c(0.7,0.7))   #

    a <- seq(from=xlim[1],to=cv,length=1000)
    x <- c(a,cv,xlim[1])
    y <- c(dbinom(na2,pz2,a)*1,0.0,0.0)
    polygon(x,y,col='red',border=NA)
    x5 <- difettosita(na2,pz2,1-IvsII/2)
    y5 <- dbinom(na2,pz2,x5)
    arrows(x5,y5,x5*.9,y5*1.1,length=0,code=1,col='red')
    text(x5*.9,y5*1.1,labels=paste('pII = ',round(IvsII,3)),adj=c(1,0),col='red',cex=c(0.7,0.7))

    # Grafico II vs I
    if (Sys.info()[1]=='Linux') X11() else windows()
    cv <- difettosita(na2,pz2,1-alfa) # Valore di
    y1<-dbinom(na1,pz1,na1/pz1)     # Valore massimo della densità della distribuzione I:na1-pz1
    y2<-dbinom(na2,pz2,na2/pz2)     # Valore massimo della densità della distribuzione II:na2-pz2
    if (y1>=y2)
        {curve(dbinom(na1,pz1,x),from=xlim[1],to=xlim[2],n=1000,main='Confronto distribuzioni binomiali\nIIvsI',col='blue',xlab='Valori',ylab='Densita',bty='n',yaxt='n')
        curve(dbinom(na2,pz2,x),from=xlim[1],to=xlim[2],n=1000,col='red',add=T)}
    else
        {curve(dbinom(na2,pz2,x),from=xlim[1],to=xlim[2],n=1000,main='Confronto distribuzioni binomiali\nIIvsI',col='red',xlab='Valori',ylab='Densita',bty='n',yaxt='n')
        curve(dbinom(na1,pz1,x),from=xlim[1],to=xlim[2],n=1000,col='blue',add=T)}
    lines(c(na1/pz1,na1/pz1),c(0,y1),col='blue',lty=2)                                    # Linea verticale dal valore di densita massimo della distribuzione I
    text(na1/pz1,y1,labels=paste('I: c=',na1,', n=',pz1),adj=c(.5,-0.2),cex=c(0.8,0.8))   # Testo identificativo della distribuzione
    lines(c(na2/pz2,na2/pz2),c(0,y2),col='red',lty=2)                                     # Linea verticale dal valore di densita massimo della distribuzione II
    text(na2/pz2,y2,labels=paste('II: c=',na2,', n=',pz2),adj=c(.5,-0.2),cex=c(0.8,0.8))  # Testo identificativo della distribuzione

    a <- seq(from=cv,to=xlim[2],length=1000)
    x <- c(a,xlim[2],cv)
    y <- c(dbinom(na1,pz1,a)*1,0.0,0.0)
    polygon(x,y,col='blue',border=NA)
    x4 <- difettosita(na1,pz1,IIvsI/3)
    y4 <- dbinom(na1,pz1,x4)
    arrows(x4,y4,x4*1.1,y4*1.1,length=0,code=1,col='blue')
    text(x4*1.1,y4*1.1,labels=paste('pI = ',round(IIvsI,3)),adj=c(0,0),col='blue',cex=c(0.7,0.7))

    a <- seq(from=xlim[1],to=cv,length=1000)
    x <- c(a,cv,xlim[1])
    y <- c(dbinom(na2,pz2,a)*1,0.0,0.0)
    polygon(x,y,col='red',border=NA)
    x5 <- difettosita(na2,pz2,0.97)
    y5 <- dbinom(na2,pz2,x5)
    arrows(x5,y5,x5*.9,y5*1.1,length=0,code=1,col='red')
    text(x5*.9,y5*1.1,labels=paste('pII = ',0.05),adj=c(1,0),col='red',cex=c(0.7,0.7))

    # Stampa dei risultati
    list("IvsII"=IvsII,"IIvsI"=IIvsI)
}

pv2pv <- function(media1,dev.st1,media2,dev.st2,normale=T,alfa=0.05)
{	# Confronto della media campionaria di due variabili
    # media1 = media campionaria del campione 1
    # dev.st1 = deviazione standard del campione 1
    # n1 = numerosita' del campione
    # alfa = errore alfa ammesso
    # Se media1 >media2 invertire i campioni
    if (media1 > media2)
    {# Inverte i due campionamenti
        i <- media1; ii <- dev.st1
        media1 <- media2; dev.st1 <- dev.st2
        media2 <- i; dev.st2 <- ii}
    else
    {if (normale)
    {# Utilizzo distribuzione normale
        z1 <- qnorm(1-alfa/2) ; x1 <- z1*dev.st1+media1
        z2 <- (x1-media2)/dev.st2 ; IvsII <- pnorm(z2)
        z2 <- -qnorm(1-alfa/2) ; x2 <- z2*dev.st2+media2
        z1 <- (x2-media1)/dev.st1 ;  IIvsI <- 1-pnorm(z1)
        xmin <- media1-(4*dev.st1) ; xmax <- media2+(4*dev.st2)
        # Grafico 1: I vs II
        cv <- qnorm(1-alfa,media1,dev.st1)
        y1<-dnorm(media1,media1,dev.st1)     # Valore massimo della densità della distribuzione I:na1-pz1
        y2<-dnorm(media2,media2,dev.st2)     # Valore massimo della densità della distribuzione II:na2-pz2
        if (dev.st1<dev.st2)
        {curve(dnorm(x,media1,dev.st1),from=xmin,to=xmax,n=1000,main='Confronto distribuzioni normali\nIvsII',col='blue',xlab='Valori',ylab='Densita',bty='n',yaxt='n')
            curve(dnorm(x,media2,dev.st2),from=xmin,to=xmax,n=1000,col='red',bty='n',yaxt='n',add=T)
            x<-xmax ; pos<-c(1,1)}
        else
        {curve(dnorm(x,media2,dev.st2),from=xmin,to=xmax,n=1000,main='Confronto distribuzioni',col='red',xlab='Valori',ylab='Densita',bty='n',yaxt='n')
            curve(dnorm(x,media1,dev.st1),from=xmin,to=xmax,n=1000,col='blue',bty='n',yaxt='n',add=T)
            x<-xmin ; pos<-c(0,1)}
        lines(c(media1,media1),c(0,y1),col='blue',lty=2)                                              # Linea verticale dal valore di densita massimo della distribuzione I
        text(media1,y1,labels=paste('I: x=',media1,', d.st=',dev.st2),adj=c(.5,-0.2),cex=c(0.8,0.8))  # Testo identificativo della distribuzione
        lines(c(media2,media2),c(0,y2),col='red',lty=2)                                               # Linea verticale dal valore di densita massimo della distribuzione II
        text(media2,y2,labels=paste('II: x=',media2,', d.st=',dev.st2),adj=c(.5,-0.2),cex=c(0.8,0.8)) # Testo identificativo della distribuzione

        a <- seq(from=cv,to=xmax,length=1000)
        x <- c(a,xmax,cv)
        y <- c(dnorm(a,media1,dev.st1)*1,0.0,0.0)
        polygon(x,y,col='blue',border=NA)
        x4 <- qnorm(0.97,media1,dev.st1)
        y4 <- dnorm(x4,media1,dev.st1)
        arrows(x4,y4,x4*1.1,y4*1.1,length=0,code=1,col='blue')
        text(x4*1.1,y4*1.1,labels=paste('pI = ',0.05),adj=c(0,0),col='blue',cex=c(0.7,0.7))

        a <- seq(from=xmin,to=cv,length=1000)
        x <- c(a,cv,xmin)
        y <- c(dnorm(a,media2,dev.st2)*1,0.0,0.0)
        polygon(x,y,col='red',border=NA)
        x5 <- qnorm(IvsII/3,media2,dev.st2)
        y5 <- dnorm(x5,media2,dev.st2)
        arrows(x5,y5,x5*.9,y5*1.1,length=0,code=1,col='red')
        text(x5*.9,y5*1.1,labels=paste('pII = ',round(IvsII,3)),adj=c(1,0),col='red',cex=c(0.7,0.7))

        # Grafico 2: II vs I
        if (Sys.info()[1]=='Linux') X11() else windows()
        cv <- qnorm(alfa,media2,dev.st2)
        y1<-dnorm(media1,media1,dev.st1)     # Valore massimo della densità della distribuzione I:na1-pz1
        y2<-dnorm(media2,media2,dev.st2)     # Valore massimo della densità della distribuzione II:na2-pz2
        if (dev.st1<dev.st2)
        {curve(dnorm(x,media1,dev.st1),from=xmin,to=xmax,n=1000,main='Confronto distribuzioni normali\nIIvsI',col='blue',xlab='Valori',ylab='Densita',bty='n',yaxt='n')
            curve(dnorm(x,media2,dev.st2),from=xmin,to=xmax,n=1000,col='red',bty='n',yaxt='n',add=T)
            x<-xmax ; pos<-c(1,1)}
        else
        {curve(dnorm(x,media2,dev.st2),from=xmin,to=xmax,n=1000,main='Confronto distribuzioni',col='red',xlab='Valori',ylab='Densita',bty='n',yaxt='n')
            curve(dnorm(x,media1,dev.st1),from=xmin,to=xmax,n=1000,col='blue',bty='n',yaxt='n',add=T)
            x<-xmin ; pos<-c(0,1)}
        lines(c(media1,media1),c(0,y1),col='blue',lty=2)                                               # Linea verticale dal valore di densita massimo della distribuzione I
        text(media1,y1,labels=paste('I: x=',media1,', d.st=',dev.st2),adj=c(.5,-0.2),cex=c(0.8,0.8))   # Testo identificativo della distribuzione
        lines(c(media2,media2),c(0,y2),col='red',lty=2)                                                # Linea verticale dal valore di densita massimo della distribuzione II
        text(media2,y2,labels=paste('II: x=',media2,', d.st=',dev.st2),adj=c(.5,-0.2),cex=c(0.8,0.8))  # Testo identificativo della distribuzione

        a <- seq(from=cv,to=xmax,length=1000)
        x <- c(a,xmax,cv)
        y <- c(dnorm(a,media1,dev.st1)*1,0.0,0.0)
        polygon(x,y,col='blue',border=NA)
        x4 <- qnorm(1-IIvsI/3,media1,dev.st1)
        y4 <- dnorm(x4,media1,dev.st1)
        arrows(x4,y4,x4*1.1,y4*1.1,length=0,code=1,col='blue')
        text(x4*1.1,y4*1.1,labels=paste('pI = ',round(IIvsI,3)),adj=c(0,0),col='blue',cex=c(0.7,0.7))

        a <- seq(from=xmin,to=cv,length=1000)
        x <- c(a,cv,xmin)
        y <- c(dnorm(a,media2,dev.st2)*1,0.0,0.0)
        polygon(x,y,col='red',border=NA)
        x5 <- qnorm(0.03,media2,dev.st2)
        y5 <- dnorm(x5,media2,dev.st2)
        arrows(x5,y5,x5*.9,y5*1.1,length=0,code=1,col='red')
        text(x5*.9,y5*1.1,labels=paste('pII = ',0.05),adj=c(1,0),col='red',cex=c(0.7,0.7))

        #dmax<-max(c(dnorm(seq(xmin,xmax,l=1000),media1,dev.st1),dnorm(seq(xmin,xmax,l=1000),media2,dev.st2)))
        #text(x,dmax,labels=paste('I:  media = ',media1,' - dev.std = ',dev.st1),col='blue',adj=pos)
        #text(x,dmax*0.95,labels=paste('II: media = ',media2,' - dev.std = ',dev.st2),col='red',adj=pos)
        #text(x,dmax*0.90,labels=paste('IvsII : ',round(IvsII,4)*100,' %\nIIvsI : ',round(IIvsI,4)*100,' %'),col='black',adj=pos)
    }
        else
        { # Utilizzo distribuzione t
        }
        #c(IvsII,IIvsI)
        list("IvsII"=IvsII,"IIvsI"=IIvsI)}
}

GCombi1var <- function(X,legX="assex",Titolo="Titolo")
{	# aggiungere help in caso si chiami la funzione senza x o y
    # spiegazione introduttiva
    # togliere i valori na

    # 1o Grafico
    par(fig=c(0.0,0.7,0.0,0.7),mar=c(4,4,2,2),cex=0.8)
    qqnorm(X,main=Titolo, pch="*")
    qqline(X)
    # 2o Grafico
    par(fig=c(0.0,0.7,0.7,1),mar=c(2,4,2,2),cex=0.7,new=T)
    hist(X,freq=F,col="yellow",main=legX)
    lines(density(X))
    rug(jitter(X))
    # 3o Grafico
    par(fig=c(0.7,1,0.05,0.7),mar=c(2,4,2,2),cex=0.7,new=T)
    boxplot(X,notch=T,col="gray",main="legX")
    points(1,mean(X),col="blue",pch=4) # Inserisce il valore della media
}

GCombi2var <- function(X,Y,legX="assex",legY="assey",Titolo="Titolo")
{   # aggiungere help in caso si chiami la funzione senza x o y
    # spiegazione introduttiva
    # togliere i valori na

    # 1o Grafico
    par(fig=c(0.0,0.7,0.0,0.7),mar=c(4,4,2,2),cex=0.8)
    plot(X,Y,xlab=legX,ylab=legY,pch="*")
    #qqplot(X,Y,pch="*")
    title(Titolo)
    # 2o Grafico
    par(fig=c(0.0,0.7,0.7,1),mar=c(2,4,2,2),cex=0.7,new=T)
    hist(X,col="yellow",main=legX)
    rug(jitter(X))
    # 3o Grafico
    par(fig=c(0.7,1,0.05,0.7),mar=c(2,4,2,2),cex=0.7,new=T)
    th<-hist(Y,plot=F)
    tcut<-cut(Y,breaks=th$breaks)
    barplot(table(tcut),horiz=T,col="gray",main=legY)
}

Pareto1 <- function(X,linee=T,testo=T)
{	length(X);length(names)
    ptab<-prop.table(rev(sort(table(X, exclude=c(NA,NaN)))))
    dati<-barplot(ptab,ylim=c(0,1.05),main="Diagramma di Pareto")
    if(linee==T)
        lines(dati,cumsum(ptab), type="b",pch=16)
    if(testo==T)
        text(dati,cumsum(ptab)+0.03,paste(round(cumsum(ptab)*100,digit=1),"%"))
}

Pareto2 <- function(X,Y,linee=T,testo=T)
{	length(X);length(names)
    X<-X[rev(order(Y))]
    Y<-rev(sort(Y))
    aaa<-matrix(Y,1,length(Y))
    colnames(aaa)<-X
    rownames(aaa)<-c("")
    bbb<-as.table(aaa)
    ptab<-prop.table(bbb)
    dati<-barplot(ptab,ylim=c(0,1.05),main="Diagramma di Pareto")
    if(linee==T)
        lines(dati,cumsum(ptab), type="b",pch=16)
    if(testo==T)
        text(dati,cumsum(ptab)+0.03,paste(round(cumsum(ptab)*100,digit=1),"%"))
}
