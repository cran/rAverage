# Ricompone gli R a partire dai parametri in input. E' una
# funzione interna che non puo' essere richiamata dall'utente.
averaging.int <- function(param,lev,fact,sumlev)
{
    Rlen <-
        .C("respdim",
            lev = as.integer(lev),
            fact = as.integer(fact),
            dim = as.integer(0),
            PACKAGE = "rAverage"
        )$dim
    R <-
        .C("averaging",
            param = as.double(param),
            lev = as.integer(lev),
            fact = as.integer(fact),
            sumlev = as.integer(sumlev),
            R = as.double(rep.int(0,Rlen)),
            PACKAGE = "rAverage"
        )$R
    return(R)
}

# Crea una matrice contenente tutte le possibili k combinazioni
# di n elementi. Corrisponde alla funzione combn, nativa di R,
# ma e' molto piu' veloce.
combin <- function(n,k)
{
    ncomb <- choose(n,k)
    x <-
    .C("combin",
        n = as.integer(n),
        k = as.integer(k),
        out = as.integer(rep.int(0,k*ncomb)),
        PACKAGE = "rAverage"
    )$out
    return(matrix(x,nrow=ncomb,byrow=TRUE))
}

# Crea tutte le combinazioni dei livelli dei fattori
# specificati, creando la griglia di un disegno fattoriale.
# Fa un lavoro simile alla funzione expand.grid, nativa di
# R, ma e' molto piu' veloce. N.B.: attualmente questa
# funzione e' utilizzata solo da 'averaging'.
design.grid <- function(lev)
{
    fact <- length(lev)
    x <-
    .C("grid",
        lev = as.integer(lev),
        fact = as.integer(fact),
        out = as.integer(rep.int(0,fact*prod(lev))),
        PACKAGE = "rAverage"
    )
    x <- matrix(x$out,ncol=fact)
    return(x)
}

# Calcola il numero di parametri.
numpar <- function(param,len)
{
    .C("numpar",
        param = as.double(param),
        len = as.integer(len),
        out = as.integer(len),
        PACKAGE = "rAverage"
    )$out
}

# Individua i pesi uguali entro il delta e li eguaglia alla loro media.
parmeanlast <- function(param, fixed, sumlev, delta.weights, nwfix)
{
    .C("parmeanlast",
        param = as.double(param),
        fixed = as.double(fixed),
        sumlev = as.integer(sumlev),
        deltaweights = as.double(delta.weights),
        numfix = as.integer(nwfix$num),
        valfix = as.double(nwfix$nwval),
        NAOK = TRUE,
        PACKAGE = "rAverage"
    )$param
}

# Crea un'istanza della classe 'indexes'.
fit.indexes <- function(output,data,lev,fact,sumlev,N,I0,dim.data,TSS,names,title)
{
    Bic <- N*log(output$value/N)+output$n.pars*log(N)
    Aic <- 2*output$n.pars*(N/(N-output$n.pars-1))+N*log(output$value/N)
    fitted <- averaging.int(output$par,lev,fact,sumlev)
    names(fitted) <- colnames(data)
    estim <- matrix(rep.int(fitted,dim.data[1]),nrow=dim.data[1],byrow=TRUE)
    R2 <- 1-output$value/TSS
    adjR2 <- (1-(1-R2)*((N-1)/(N-output$n.pars-1)))
    observed <- data
    expected <- estim
    observed[which(abs(observed)<1e-5)] <- 1e-5
    expected[which(abs(expected)<1e-5)] <- 1e-5
    # Chi-quadrato: vedi Corbetta (1992), pag. 31,34,123
    L2 <- 2*sum(observed*log(observed/expected),na.rm=TRUE)
    df <- (2+2*sum(lev))-output$n.pars
    p.value <- 1-pchisq(L2,df)
    ChiSq <- list(L2=L2, df=df, p.value=p.value)
    msg <- ""
    if(output$value<TSS) RSS.control <- NULL
    else
        RSS.control <- "FATAL ERROR: the model does not fit the data"
    if(output$convergence==0) msg <- ""
    else {
        if(output$convergence==1)
            msg <- "Convergence error: the iteration limit maxit had been reached"
        if(output$convergence==10)
            msg <- "Convergence error: degeneracy of the Nelder-Mead simplex"
        if(output$convergence==51)
            msg <- paste("Convergence warning:",output$message)
        if(output$convergence==52)
            msg <- paste("Convergence error:",output$message)
    }
    if(!is.null(RSS.control)) msg <- paste(RSS.control,"\n",msg,sep="")
    new("indexes",
        param=output$par, I0=I0, levels=lev, fitted=fitted, residuals=data-estim,
        AIC=Aic, BIC=Bic, RSS=output$value, TSS=TSS, R2=R2, adjR2=adjR2, ChiSq=ChiSq,
        n.pars=output$n.pars, title=c(title,"\n"), names=names, message=msg)
}

# Esegue lo stesso lavoro di fit.indexes, pero' non viene invocata da rav()
# ma da una chiamata diretta dell'utente.
rav.indexes <- function(param,lev,data,subset=NULL,I0=FALSE,names=NULL,title=NULL)
{
    # Verifiche su names e title ------------------------------
    if(is.null(names)) names <- paste("Factor",LETTERS[1:length(lev)])
    if(is.null(title)) title <- as.character()
    fact <- length(lev)
    sumlev <- as.integer(sum(lev))
    sumlev <- c(sumlev,2+sumlev,2+2*sumlev)
    
    # subset e data
    if(!is.null(subset)) {
        set <- TRUE
        for(i in 1:length(subset)) set <- set|(data[,1]==subset[i])
        # Si elimina la colonna che codifica per i soggetti:
        data <- data[set,-1]
        rm(set)
    }
    data <- as.matrix(data)
    dim.data <- dim(data) 
    if(dim.data[2]==1) data <- t(data)
    
    all.resp <-
        .C("respdim",
            lev = as.integer(lev),
            fact = as.integer(fact),
            dim = as.integer(0),
            PACKAGE = "rAverage"
        )$dim
    # Se c'e' la colonna di raggruppamento la si elimina:
    if(dim.data[2]!=all.resp) {
        if(dim.data[2] == (all.resp+1)) {
            data <- data[,-1]
            dim.data[2] <- dim.data[2]-1
        } else stop("Error occurred in columns check\n")
    }
    rm(all.resp)
    
    # Costruzione finto modello di output ---------------------
    sumlev <- as.integer(sum(lev)) # numero totale di livelli
    sumlev <- c(sumlev,2+sumlev,2+2*sumlev)
    estim <- averaging.int(param,lev,fact,sumlev)
    estim <- matrix(rep.int(estim,dim.data[1]),nrow=dim.data[1],byrow=TRUE)
    output <- list(par=param,value=NULL,n.pars=NULL,convergence=0,message="")
    output$value <- sum((data-estim)^2,na.rm=TRUE)
    weights <- param[(3+sumlev[1]):sumlev[3]]
    output$n.pars <- ifelse(I0,2,0)+sumlev[1]+numpar(weights,sumlev[1])
    N <- sum(!is.na(data)) # numero di R osservati
    TSS <- sum((data-mean(data,na.rm=TRUE))^2,na.rm=TRUE) # Total Sum of Squares
    model <- fit.indexes(output,data,lev,fact,sumlev,N,I0,dim.data,TSS,names,title)
    return(model)
}

# Genera parametri random per il modello averaging.
pargen <- function(lev,s.range=c(0,20),w.range=c(0.5,3),I0=FALSE,digits=1)
{
    if(I0) {
        I0[1] <- runif(1,min=s.range[1],max=s.range[2]/2)
        I0[2] <- runif(1,min=w.range[1],max=w.range[2]/2)
    }
    else
        I0 <- c(0,0)
    s.par <- w.par <- NULL
    for(i in 1:length(lev)) {
        s.par <- c(s.par,runif(lev[i],min=s.range[1],max=s.range[2]))
        w.par <- c(w.par,runif(lev[i],min=w.range[1],max=w.range[2]))
    }
    param <- round(c(I0,s.par,w.par),digits)
    return(param)
}

# Funzione averaging esterna, che puo' essere chiamata dall'utente.
# Se sd>0 e trials>1 viene costruita una matrice di R con errore random.
averaging <- function(param, lev, trials=1, sd=0, range=NULL)
{
    fact <- length(lev)
    sumlev <- as.integer(sum(lev))
    sumlev <- c(sumlev,2+sumlev,2+2*sumlev)
    R <- averaging.int(param,lev,fact,lev)
    R <- matrix(rep.int(R,trials), nrow=trials, byrow=TRUE)
    # ----------------------------------------------
    # Aggiunta di errore alle colonne
    # ----------------------------------------------
    if(sd>0 & trials>1) {
        numcol <- ncol(R)
        j <- 1 # j-esima colonna
        repeat {
            R.err <- R[,j] + rnorm(trials,mean=0,sd=sd)
            check <- TRUE
            if(!is.null(range)) {
                if(any(R.err<range[1]) | any(R.err>range[2]))
                    check <- FALSE
            }
            if(check) {
                R[,j] <- R.err
                j <- j+1
            }
            if(j>numcol) break
        }
    }
    # ----------------------------------------------
    # Attribuzione dei nomi alle colonne
    # ----------------------------------------------
    R.names <- NULL
    # Disegni a 1 via
    for(i in 1:fact)
        R.names <- c(R.names,paste(LETTERS[i],1:lev[i],sep=""))
    # Disegni a k vie
    for(k in 2:fact) {
        cc <- t(combin(fact,k))
        for(j in 1:ncol(cc)) {
            f <- LETTERS[cc[,j]]
            g <- design.grid(lev[cc[,j]])
            for(i in 1:nrow(g)) {
                each <- paste(f,g[i,],sep="")
                collapse <- paste(each[1],each[2],sep="")
                h <- 3
                while(h<=k) {
                    collapse <- paste(collapse,each[h],sep="")
                    h <- h+1
                }
                R.names <- c(R.names,collapse)
            }
        }
    }
    colnames(R) <- R.names
    return(R)
}
