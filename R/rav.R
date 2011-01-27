rav <- function(data, subset=NULL, lev, all=FALSE, sim=0, range=NULL, start=NULL, lower=NULL, upper=NULL,
    I0=FALSE, s.fixed=FALSE, w.fixed=FALSE, IC.diff=c(2,2), delta.weights=0.1,
    method="L-BFGS-B", control=list(), title=NULL, names=NULL, verbose=FALSE)
{
    # --------------------------------------
    # Inizializzazione variabili e controlli
    # --------------------------------------
    
    # lev deve essere numerico:
    if(!is.numeric(lev)) stop("Mistaken definition of the lev argument: check the values of the vector.\n")
    IC.diff <- abs(IC.diff)
    fact <- length(lev) # numero di fattori
    sumlev <- as.integer(sum(lev)) # numero totale di livelli
    sumlev <- c(sumlev,2+sumlev,2+2*sumlev)
    fixed <- rep.int(NaN,sumlev[3]) # Vettore dei param fissi
    if(!I0) fixed[1:2] <- 0
    
    # Attribuzione dei valori di default a names:
    if(is.null(names)) names <- paste("Factor",LETTERS[1:fact])
    else names <- as.character(names[1:fact])
    
    # Indici per l'estrazione dei parametri s e w dai vettori:
    pos <- list(
        slast = seq_len(sumlev[2]), # sequenza che va da 1 alla posizione dell'ultimo s
        spos = seq_len(sumlev[1])+2, # posizioni degli s:
        wfirst = as.integer(cumsum(c(1,lev[-fact]))+sumlev[2]), # posizioni dei primi pesi di ogni fattore
        wpos = (sumlev[2]+1):sumlev[3], # posizioni di tutti i pesi
        fixed = NULL # posizioni dei valori fissati
    )
        
    # ----------------------------------
    # Verifiche sul data set
    # ----------------------------------
    
    # Se richiesto, si estrae un subset di dati:
    if(!is.null(subset)) {
        set <- FALSE
        for(i in 1:length(subset)) set <- set|(data[,1]==subset[i])
        # Si elimina la colonna che codifica:
        data <- data[set,-1]
        rm(set)
    }
    data <- as.matrix(data)
    dim.data <- dim(data) 
    if(dim.data[2]==1) {
        data <- t(data)
        dim.data <- dim(data)
    }
    # Numero corretto di colonne della matrice di dati:
    all.resp <-
        .C("respdim",
            lev = as.integer(lev),
            fact = as.integer(fact),
            dim = as.integer(0),
            PACKAGE = "rAverage"
        )$dim
    # Se c'e' la colonna di codifica si elimina:
    if(dim.data[2]!=all.resp) {
        if(dim.data[2] == (all.resp+1)) {
            data <- data[,-1]
            dim.data[2] <- dim.data[2]-1
        } else stop("Error occurred in columns check")
    }
    
    # -----------------------------------------------
    # Fissaggio dei valori di scala e dei pesi
    # -----------------------------------------------
    
    if(s.fixed) {
        fixed[pos$spos] <- colMeans(data[,1:sumlev[1]],na.rm=TRUE)  # Medie dei sotto-disegni
        if(all(is.na(fixed[pos$spos]))) s.fixed <- FALSE
    }
    
    if(is.logical(w.fixed))
        if(w.fixed)
            w.fixed <- fixparam(lev,"w",names) # apre l'interfaccia per fissare i pesi
    
    if(is.list(w.fixed)) {
        fixed[pos$wpos] <- unlist(w.fixed)
        fixed[pos$wpos][which(fixed[pos$wpos]==0)] <- 1e-10
    }
    
    pos$fixed <- which(!is.na(fixed))
    nwfix <- list(
        num = NULL, # quanti pesi fissi
        pos = which(!is.na(fixed[pos$wpos])), # posizioni
        nwval = fixed[pos$wpos][which(!is.na(fixed[pos$wpos]))] # valori
    )
    nwfix$num <- length(nwfix$nwval)
    
    # Non ci devono essere pesi fissi uguali entro il delta:
    if(nwfix$num>1) {
        w.fixed.diff <- NULL
        for(i in 1:nwfix$num)
            w.fixed.diff <- c(w.fixed.diff,nwfix$nwval[i]-nwfix$nwval[-c(1:i)])
        w.fixed.diff <- abs(w.fixed.diff)
        if(sum(w.fixed.diff > 1e-7 & w.fixed.diff<=delta.weights))
            stop("Some of the fixed weights are different within the delta.weights")
    }
    
    # ----------------------------------
    # Start, lower, upper
    # ----------------------------------
    if(is.null(range)) {
        range <- range(data,na.rm=TRUE)
        range[1] <- floor(range[1])
        range[2] <- ceiling(range[2])
    }
    middle <- mean(range)
    pos$fixed <- which(!is.na(fixed))
    
    if (is.null(start)) {
        start <- list(I0=fixed[1:2],s=NULL,w=NULL)
        for(i in 1:fact) {
            start$s <- c(start$s,rep.int(middle,lev[i]))
            start$w <- c(start$w,rep.int(1,lev[i]))
        }
        if(any(is.na(start$I0))) start$I0 <- c(range[1],1)
        start <- as.numeric(unlist(start))
    }
    start[pos$fixed]<-fixed[pos$fixed]
    
    if(method=="L-BFGS-B") {
        # Upper -------
        if (is.null(upper)) {
            upper <- list(s=NULL,w=NULL)
            for(i in 1:fact) {
                upper$s <- c(upper$s,rep.int(range[2],lev[i]))
                upper$w <- c(upper$w,rep.int(10,lev[i]))
            }
        upper <- as.numeric(c(middle,range[2]/10,unlist(upper)))
        }
        # Lower -------
        if (is.null(lower)) {
            lower <- list(s=NULL,w=NULL)
            for(i in 1:fact) {
                lower$s <- c(lower$s,rep.int(range[1],lev[i]))
                lower$w <- c(lower$w,rep.int(1e-5,lev[i]))
            }
            lower <- as.numeric(c(range[1],1e-5,unlist(lower)))
        }
        if(!I0) {
            # Dato che s0=w0=0, bisogna assicurarsi che i
            # bounds non siano tutti sopra o sotto lo zero.
            upper[1:2] <- 2L
            lower[1:2] <- 0L
        }
    } else {
        upper <- +Inf
        lower <- -Inf
        }
    rm(all.resp,middle)
    
    # --------------------
    # chiamate alle rav
    # --------------------
    if(sim<1)
        rav.base(
            data=data, lev=lev, fact=fact, dim.data=dim.data, sumlev=sumlev, pos=pos,
            start=start, upper=upper, lower=lower, I0=I0, fixed=fixed, s.fixed=s.fixed,
            nwfix=nwfix, delta.weights=delta.weights, all=all, verbose=verbose,
            IC.diff=IC.diff, method=method, control=control, names=names, title=title
        )
    else
        rav.sim(
            data=data, lev=lev, fact=fact, dim.data=dim.data, sumlev=sumlev, pos=pos,
            start=start, upper=upper, lower=lower, range=range, I0=I0, fixed=fixed,
            s.fixed=s.fixed, nwfix=nwfix, sim=sim, delta.weights=delta.weights, all=all,
            IC.diff=IC.diff, method=method, control=control, names=names, title=title
        )
}

rav.base <- function(data, lev, fact, dim.data, sumlev, pos, start, upper, lower, I0, nwfix,
    fixed, s.fixed, delta.weights, all, verbose, IC.diff, method, control, names, title)
{
    # Potrebbero venire spostati
    bounds <- method=="L-BFGS-B"
    N <- sum(!is.na(data)) # numero di R osservati
    TSS <- sum((data-mean(data,na.rm=TRUE))^2,na.rm=TRUE) # Total Sum of Squares
    par.base <- ifelse(I0,2L,0L)+sumlev[1]
    
    # EQUAL WEIGHT CASE
    
    # Di tutti i pesi passati come start bisogna mantenerne solo uno per fattore:
    pos.val <- c(pos$slast,pos$wfirst)
    start.eq <- start[pos.val]
    if(bounds) {
        upper.eq <- upper[pos.val]
        lower.eq <- lower[pos.val]
    } else {
            upper.eq <- +Inf
            lower.eq <- -Inf
        }
    rm(pos.val)
    
    # Stima dei parametri:
    output <- optim(par=start.eq, fn=Residual.eq, fixed=fixed, data=data, lev=lev, fact=fact,
        sumlev=sumlev, dim.data=dim.data, method=method, lower=lower.eq, upper=upper.eq, control=control) 
    
    # Replica dei pesi e parmetri del modello:
    param.equal <- output$par[pos$slast]
    for(i in 1:fact)
        param.equal <- c(param.equal,rep.int(output$par[pos$wpos[i]],lev[i]))
    output$par <- param.equal
    
    # Si eguagliano i pesi uguali entro delta:
    output$par <- parmeanlast(output$par,fixed,sumlev,delta.weights,nwfix=list(num=0,nwval=0))
    
    # Calcolo del numero di parametri del modello:
    output$n.pars <- par.base+numpar(output$par[pos$wfirst],fact)
    
    # Istanza di classe:
    case.equal <- fit.indexes(output=output, data=data, lev=lev, fact=fact, sumlev=sumlev,
        N=N, I0=I0, dim.data=dim.data, TSS=TSS, names=names, title=modelNames[1])
    
    # DIFFERENTIAL WEIGHT CASE  
    
    # Stima dei parametri:
    output <- optim(par=start, fn=Residual, fixed=fixed, data=data, lev=lev, fact=fact,
        sumlev=sumlev, dim.data=dim.data, delta.weights=delta.weights, nwfix=nwfix[c(1,3)],
        method=method, lower=lower, upper=upper, control=control)
    
    # Si eguagliano i pesi uguali entro delta:
    output$par <- parmeanlast(output$par,fixed,sumlev,delta.weights,nwfix)
    
    # Calcolo del numero di parametri del modello:
    output$n.pars <- par.base+numpar(output$par[pos$wpos],sumlev[1])
    
    # Istanza di classe:
    case.diff <- fit.indexes(output=output, data=data, lev=lev, fact=fact, sumlev=sumlev,
        N=N, I0=I0, dim.data=dim.data, TSS=TSS, names=names, title=modelNames[2])
    
    # INFORMATION CRITERION
    
    case.start <- case.equal
    # Se gli s o alcuni pesi sono stati fissati, bisogna ricalcolare gli
    # indici del modello di partenza:
    if(s.fixed | nwfix$num) {
        case.start@param[pos$fixed] <- fixed[pos$fixed]
        # Si eguagliano i pesi uguali entro delta.weights:
        case.start@param <- parmeanlast(case.start@param,fixed,sumlev,delta.weights,nwfix)
        # Riconteggio del numero di parametri:
        case.start@n.pars <- par.base+numpar(case.start@param,sumlev[1])
        # Calcolo del nuovo RSS:
        case.start@RSS <- Residual(case.start@param,fixed,data,lev,fact,sumlev,dim.data,delta.weights,nwfix)
        # Nuova istanza di classe:
        output$par <- case.start@param
        output$value <- case.start@RSS
        output$convergence <- 0
        output$message <- ""
        output$n.pars <- case.start@n.pars
        case.start <- fit.indexes(output=output, data=data, lev=lev, fact=fact, sumlev=sumlev,
            I0=I0, N=N, dim.data=dim.data, TSS=TSS, names=names, title=modelNames[1])
    }
    
    # Stima dei parametri:
    output <- optimization.IC(data=data, fact=fact, lev=lev, sumlev=sumlev, pos=pos, N=N,
        dim.data=dim.data, model.start=case.start, par.base=par.base, nwfix=nwfix,
        fixed=fixed, delta.weights=delta.weights, IC.diff=IC.diff, all=all, verbose=verbose,
        lower=lower, upper=upper, method=method, control=control)
    
    # Istanza di classe:
    case.IC <- fit.indexes(output=output, data=data, lev=lev, fact=fact, sumlev=sumlev,
        N=N, I0=I0, dim.data=dim.data, TSS=TSS, names=names, title=modelNames[3])
    
    # MODEL SELECTION
    
    # Selezione in base al BIC
    Bic <- c(case.equal@BIC,case.diff@BIC,case.IC@BIC)
    Bic.diff <- Bic-min(Bic)
    selection <- Bic.diff<IC.diff[1]
    if(Bic.diff[1] == Bic.diff[3]) {
        # Il modello equal non è stato modificato dalla procedura IC
        selection[3] <- FALSE
    }
    # Se e' stato selezionato piu' di un modello:
    if(sum(selection)>1) {
        # Selezione in base all'AIC
        Aic <- c(case.equal@AIC,case.diff@AIC,case.IC@AIC)
        Aic.diff <- Aic-min(Aic)
        selection <- Aic.diff<IC.diff[1]
        if(Aic.diff[1] == Aic.diff[3]) {
            # Il modello equal non è stato modificato dalla procedura IC
            selection[3] <- FALSE
        }
    }
    selection <- seq_len(3)[selection]
    new("rav", observed=data, factors=fact, levels=lev,
        title=as.character(title), names=names, selection=selection,
        start=start, lower=lower, upper=upper,
        equal=case.equal, differential=case.diff, IC=case.IC)
}

rav.sim <- function(data, lev, fact, dim.data, sumlev, pos, start, upper, lower, range, I0, 
    fixed, s.fixed, nwfix, sim, delta.weights, all, IC.diff, method, control, names, title)
{
    # Costruzione matrici
    m <- colMeans(data,na.rm=TRUE) # medie degli R
    s <- apply(data,2,sd,na.rm=TRUE) # dev.st.
    R <- matrix(rep.int(m,dim.data[1]),ncol=dim.data[2],byrow=TRUE)
    check.col <- which(!is.na(m)) # colonne senza NA, da controllare
    check.len <- length(check.col)
    # Creazione liste matrici e modelli stimati:
    R.list <- model <- vector("list",sim)
    # Costruzione matrice parametri stimati:
    par <- matrix(NA,nrow=sim,ncol=sumlev[3])
    
    # Simulazione per la stima dei parametri
    for(i in 1:sim) { # spanna le matrici
        R.list[[i]] <- R
        # Aggiunta errore alla i-esima matrice:
        j <- 1 # j-esima colonna
        repeat {
            R.err <- R.list[[i]][,check.col[j]] + rnorm(dim.data[1],sd=s[check.col[j]])
            if(all(R.err>=range[1]) & all(R.err<=range[2])) {
                R.list[[i]][,check.col[j]] <- R.err
                j <- j+1
            }
            if(j>check.len) break
        }
        # Stima dei paramerti per la i-esima matrice:
        model[[i]] <- rav.base(
            data=R.list[[i]], lev=lev, fact=fact, dim.data=dim.data, sumlev=sumlev, pos=pos,
            start=start, upper=upper, lower=lower, I0=I0, fixed=fixed, s.fixed=s.fixed,
            nwfix=nwfix, delta.weights=delta.weights, all=all, verbose=FALSE,
            IC.diff=IC.diff, method=method, control=control, names=names, title=title
        )
        # Inserimento dei parametri stimati nella matrice:
        if(model[[i]]@selection[1]==3)
            par[i,] <- model[[i]]@IC@param
        else {
            if(model[[i]]@selection[1]==1)
                par[i,] <- model[[i]]@equal@param
            else
                par[i,] <- model[[i]]@differential@param
        }
    }
    # Calcolo dei rapporti fra i pesi:
    w.base <- apply(par[,pos$wpos],2,median)
    w.base <- sumlev[2]+which(w.base==max(abs(w.base)))[1]
    par[,pos$wpos] <- par[,pos$wpos]/par[,w.base]
    start <- apply(par,2,median)
    # Stima del nuovo modello:
    model <- rav.base(
        data=data, lev=lev, fact=fact, dim.data=dim.data, sumlev=sumlev, pos=pos,
        start=start, upper=upper, lower=lower, I0=I0, fixed=fixed, s.fixed=s.fixed,
        nwfix=nwfix, delta.weights=delta.weights, all=all, verbose=FALSE,
        IC.diff=IC.diff, method=method, control=control, names=names, title=title
    )
    return(model)
}
