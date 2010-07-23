setClass("indexes",
    representation(
        param="numeric",
        I0="vector",
        levels="numeric",
        fitted="vector",
        residuals="matrix",
        AIC="numeric",
        BIC="numeric",
        RSS="numeric",
        TSS="numeric",
        R2="numeric",
        adjR2="numeric",
        ChiSq="list",
        n.pars="numeric",
        title="character",
        names="character",
        message="character"
    )
)

setClass("rav",
    representation(
        observed="matrix",
        factors="numeric",
        levels="numeric",
        title="character",
        names="character",
        selection="numeric",
        equal="indexes",
        differential="indexes",
        IC="indexes"
    )
)

setMethod("show","indexes",
    function(object) {
        fact <- length(object@levels)
        param <- list(I0=object@param[1:2],s=NULL,w=NULL)
        param$s <- object@param[3:(2+sum(object@levels))]
        param$w <- object@param[(3+sum(object@levels)):length(object@param)]
        s <- list(param$s[1:object@levels[1]])
        w <- list(param$w[1:object@levels[1]])
        for(i in 2:fact) {
            s[[i]]<-param$s[(sum(object@levels[1:(i-1)])+1):sum(object@levels[1:i])]
            w[[i]]<-param$w[(sum(object@levels[1:(i-1)])+1):sum(object@levels[1:i])]
        }
        
        cat(object@title)
        if(object@I0) cat("Initial state:")
        else cat("Initial state (fixed):")
        cat("  s0:",sprintf("%.2f",param$I0[1]))
        cat("  w0:",sprintf("%.2f",param$I0[2]))
        cat("\n")
        # Le stringhe dei nomi dei fattori vengono rese a lunghezza uguale.
        # Questo viene fatto aggiungendo spazi vuoti alle stringhe piu' corte.
        empty <- paste(rep(" ",50),collapse=" ") # emorme stringa vuota
        # numero di caratteri di ogni stringa:
        charNames <- nchar(object@names)
        # massimo numero di caratteri in una stringa:
        maxChar <- max(charNames)
        # posizioni occupate dalle stringhe piu' lunghe:
        posMaxChar <- which(charNames==maxChar)
        # posizioni occupate dalle stringhe piu' corte:
        posMinChar <- (1:fact)[-posMaxChar]
        # Le stringhe, se gia' non lo sono, vengono rese a lunghezza uguale:
        if(length(posMinChar)!=0) {
            for(i in 1:length(posMinChar)) {
                # numero di caratteri vuoti da aggiungere:
                ladd <- maxChar-charNames[posMinChar[i]]
                # Amplia le stringhe piu' corte:
                object@names[posMinChar[i]]<-
                    paste(object@names[posMinChar[i]],substr(empty,1,ladd),sep="")
            }
        }
        # Nomi dei parametri:
        maxLev <- max(object@levels)
        labels <- list(s=NULL,w=NULL)
        # I parametri, per essere stampati a video, devono essere trasformati in stringhe
        # a uguale numero di caratteri. Questo è il numero di caratteri che deve possedere
        # ogni stringa:
        charStr <- max(nchar(as.integer(object@param))+3)
        # (+3 perche' ci sono da aggiungere i caratteri del punto e delle due cifre decimali)
        ladd <- substr(empty,1,charStr-2)
        for(i in 1:maxLev) {
            labels$s <- c(labels$s,c(paste(ladd,"s",i,sep="")))
            labels$w <- c(labels$w,c(paste(ladd,"w",i,sep="")))
        }
        
        # Se i fattori hanno numeri diversi di livelli, in corrispondenza
        # dei valori mancanti bisogna mettere degli spazi vuoti (NA):
        for(i in 1:fact) {
            if(object@levels[i]<maxLev) {
                s[[i]]<-c(s[[i]],rep.int(NA,maxLev-object@levels[i]))
                w[[i]]<-c(w[[i]],rep.int(NA,maxLev-object@levels[i]))
            }
        }
        
        # Il comando sprintf lavorara in modo diverso a seconda che il parametro
        # sia un numero oppure un NA. Bisogna utilizzare due diverse regole di stampa:
        printRuleNA <- paste("%",charStr,"s",sep="")
        printRule <- paste("%",charStr,".2f",sep="")
        
        # Stampa le etichette contenenti i nomi dei parametri:
        spacer <- paste(substr(empty,1,maxChar),"  ",sep="")
        cat(spacer,labels$s,"",labels$w,"\n")
        
        # Questo ciclo, ormai deprecato, stampa a video i parametri; dove non c'e'
        # alcun parametro viene mantenuto il valore 'NA':
        # for (i in 1:fact)
        # cat(object@names[i]," ",sprintf(printRule,s[[i]]),"",sprintf(printRule,w[[i]]),"\n")
        
        # Questo ciclo stampa a video i parametri, inserendo una stringa vuota
        # in corripondenza dei valori 'NA':
        parlen <- max(object@levels)
        for (i in 1:fact) {
            par <- unlist(c(s[i],w[i]))
            cat(object@names[i],"  ")
            for(j in 1:(2*parlen)) {
                if(!is.na(par[j]))
                    cat(sprintf(printRule,par[j]),"")
                else
                    cat(sprintf(printRuleNA,""), "")
                if(j==parlen) cat(" ")
                if(j==(2*parlen)) cat("\n")
            }
        }
        
        # Stampa a vieo gli indici di fit:
        cat("---\n")
        cat("AIC:",   sprintf("%.2f",object@AIC),"",
            "BIC:",   sprintf("%.2f",object@BIC),"",
            "n.pars:",object@n.pars,
            "\n")
        cat("R-squared:", sprintf("%.4f",object@R2),"",
            "Adjusted R-squared:", sprintf("%.4f",object@adjR2),
            "\n")
        cat("Residual Sum Sq:",   sprintf("%.2f",sum(object@RSS)),"",
            "Total Sum Sq:",   sprintf("%.2f",sum(object@TSS)),
            "\n")
        cat(paste(
                "Chi-square: ",sprintf("%.2f",object@ChiSq$L2),
                " (df = ",object@ChiSq$df,")",
                " p = ",sprintf("%.4f",object@ChiSq$p.value),
                ifelse(object@ChiSq$p.value<0.001," ***",
                    ifelse(object@ChiSq$p.value<0.01," **",
                        ifelse(object@ChiSq$p.value<0.05," *","")
                    )
                ),sep=""
            )
        )
        if(object@message!="") {
            cat("\n")
            cat(object@message,"\n")
        }
        cat("\n")
    }
)

setMethod("show","rav", 
    function(object) {
        cat("Parameter Estimation for Averaging Model\n")
        design <- NULL
        for(i in 1:object@factors)
            design <- c(design,paste("(",object@levels[i]," lev",")",sep=""))
        design <- paste(object@names,design)
        for(i in 2:object@factors)
            design[1] <- paste(design[1],design[i],sep=" x ")
        cat(design[1],"\n")
        if(length(object@title)!=0) cat(object@title,"\n")
        cat("\n")
        cat("BEST MODEL(S) SELECTED:\n\n")
        for(i in 1:length(object@selection)) {
            if (object@selection[i]==1) { print(object@equal);        cat("\n") }
            if (object@selection[i]==2) { print(object@differential); cat("\n") }
            if (object@selection[i]==3) { print(object@IC);           cat("\n") }
        }
    }
)

setMethod("summary","rav", 
    function(object) {
        cat("Parameter Estimation for Averaging Model\n")
        design <- NULL
        for(i in 1:object@factors)
            design <- c(design,paste("(",object@levels[i]," levels",")",sep=""))
        design <- paste(object@names,design)
        for(i in 2:object@factors)
            design[1] <- paste(design[1],design[i],sep=" x ")
        cat(design[1],"\n")
        if(length(object@title)!=0) cat(object@title,"\n")
        cat("\n")
        cat("FITTED MODELS\n\n")
        print(object@equal);        cat("\n\n")
        print(object@differential); cat("\n\n")
        print(object@IC);           cat("\n\n")
        cat("Best Model(s):", modelNames[object@selection],"\n")
    }
)

setGeneric("fitted",package="stats")
setMethod("fitted","rav",
    function(object) {
        if(object@selection[1]==1) return(object@equal@fitted)
        if(object@selection[1]==2) return(object@differential@fitted)
        if(object@selection[1]==3) return(object@IC@fitted)
    }
)

setGeneric("residuals",package="stats")
setMethod("residuals","rav",
    function(object) {
        if(object@selection[1]==1) return(object@equal@residuals)
        if(object@selection[1]==2) return(object@differential@residuals)
        if(object@selection[1]==3) return(object@IC@residuals)
    }
)

parameters <- function(object) {
    if(object@selection[1]==1) param <- object@equal@param
    if(object@selection[1]==2) param <- object@differential@param
    if(object@selection[1]==3) param <- object@IC@param
    s.labels <- w.labels <- NULL
    for(k in 1:object@factors) {
        s.labels <- c(s.labels,paste("s",LETTERS[k],1:object@levels[k],sep=""))
        w.labels <- c(w.labels,paste("w",LETTERS[k],1:object@levels[k],sep=""))
    }
    names(param) <- c("s0","w0",s.labels,w.labels)
    return(param)
}

modelNames <- c(
    "Equal weight case",
    "Differential weight case",
    "Information criterion"
)
