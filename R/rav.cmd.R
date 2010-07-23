rav.cmd <- function(lev=NULL,range=NULL,names=NULL)
{
    # tclRequire("Tktable")
    # xxxVar: tcl variable
    # xxxVal: value of tcl variable
    
    # Verifiche iniziali
    if(!is.null(lev) & !is.vector(lev)) stop("lev must be a vector")
    if(!is.null(lev) & !is.numeric(lev)) stop("lev must be a numeric type")
    if(!is.null(range) & !is.vector(range)) stop("range must be a vector")
    if(!is.null(range) & !is.numeric(range)) stop("range must be a numeric type")
    namesVal <- names
    if(!is.null(lev) & is.null(namesVal))
        namesVal <- paste("factor",LETTERS[1:length(lev)])
    if(!is.null(lev)) nfact <- length(lev)
    envirFun <- environment()
    fg <- "blue"  # foreground (text color)
    bg <- "white" # background (cell color)
    
    libSum <- suppressWarnings(library())
    libMsg <- paste(
        "is not available, will be impossible to load xls files.\n",
        "You should use 'install.packages()' for install it.")
    
    #if(.Platform$OS.type == "windows") {
    #    if(any(libSum$results=="RODBC")) require(RODBC)
    #    else warning(paste("The package RODBC",libMsg))
    #}
    #if(any(libSum$results=="foreign")) require(foreign)
    #else warning(paste("The package foreign",libMsg))
    
    # ====================================================== #
    #                  Funzioni di supporto                  #
    # ====================================================== #
    
    # --------------------------------------------------------
    # Funzione 'char.detect'
    # --------------------------------------------------------
    # Data una stringa contenente sia caratteri che spazi,
    # individua le sequenze di caratteri e le isola.
    # --------------------------------------------------------
    char.detect <- function(x) {
        val <- NULL
        flag <- 1
        for(i in 1:nchar(x)) {
            each <- substr(x,i,i)
            if(each==" ") {
                val <- c(val,substr(x,flag,i-1))
                flag <- i+1
            }
        }
        # Aggiunge l'ultima stringa:
        if(flag>1) val <- c(val,substr(x,flag,nchar(x)))
        if(is.null(val)) return(x)
        else {
            # Se nel vettore ci sono stringhe
            # vuote ("") bisogna eliminarle:
            val2 <- NULL
            for(i in 1:length(val)) {
                if(!val[i]=="") {
                    each <- val[i]
                    val2 <- c(val2,each)
                }
            }
            if(is.null(val2)) return(x)
            else
            return(val2)
        }
    }
    
    # --------------------------------------------------------
    # Funzione 'comma.detect'
    # --------------------------------------------------------
    # All'interno di una stringa di caratteri, individua le
    # virgole e le sostituisce con punti.
    # --------------------------------------------------------
    comma.detect <- function(x) {
        x <- as.character(x)
        dotx <- NULL
        for(j in 1:length(x)) {
            each <- x[j]
            for(i in 1:nchar(each))
                if(substr(each,i,i)==",")
                    substr(each,i,i)<-"."
            dotx <- c(dotx,each)
        }
        return(dotx)
    }
    
    # --------------------------------------------------------
    # Funzione 'index.detect'
    # --------------------------------------------------------
    # Valuta se i caratteri contenuti in una stringa sono
    # adatti per essere usati come indici per l'estrazione
    # di elementi da una matrice. Se sono adatti, costruisce
    # il vettore di indici. Valori in output:
    #   ("") significa che la cella è stata lasciata vuota
    #    (0) significa che i caratteri nella cella non sono
    #        adatti
    # (vect) vettore di indici, restituito se i caratteri
    #        sono adatti
    # --------------------------------------------------------
    index.detect <- function(x) {
        if(x=="") return("")
        val <- NULL
        for(i in 1:nchar(x)) {
            each <- substr(x,i,i)
            val <- c(val,each)
        }
        for(i in 1:length(val))
            if(is.na(suppressWarnings(as.numeric(val[i]))) &
                val[i]!=":" & val[i]!=",")
                return(0)
        xVect <- paste("c(",x,")")
        vect <- eval(parse(text=xVect))
        if(any(vect < 1)) return(0)
        return(vect)
    }
    
    # --------------------------------------------------------
    # Funzione 'symbol.detect'
    # --------------------------------------------------------
    # Fa uno scan di una stringa di caratteri, e verifica
    # che i simboli che la costituiscono siano adeguati
    # perchè possa essere usata come nome per una variabile.
    # Output: (0) la stringa non va bene
    #         (1) la stringa va bene
    # --------------------------------------------------------
    symbol.detect <- function(x) {
        x <- as.character(x)
        # La prima lettera del nome della variabile deve
        # essere o una lettera o un underscore:
        x1 <- substr(x,1,1)
        charOk <- c(letters,LETTERS,"_")
        firstOk <- FALSE
        for(i in 1:length(charOk))
            if(x1==charOk[i]) firstOk <- TRUE
        if(firstOk==FALSE) return(0)
        # Check su tutti gli altri i simboli costituenti il
        # nome della variabile:
        charOk <- c(letters,LETTERS,"_",".",0:9)
        for(i in 1:nchar(x)) {
            if(any(substr(x,i,i)==charOk)) next
            else return(0)
        }
        return(1)
    }
    
    # ====================================================== #
    #       Funzioni richiamate direttamente dai widget      #
    # ====================================================== #
    
    # --------------------------------------------------------
    # Funzione 'fact.send'
    # --------------------------------------------------------
    # Chiamata dalla prima maschera.
    # Invia i dettagli sul numero di fattori e il range.
    # --------------------------------------------------------
    fact.send <- function() {
        valFact <- suppressWarnings(as.numeric(tclvalue(valFact)))
        if(is.na(valFact) | valFact<2) {
            tcl("tk_messageBox",icon="error",
            message="Invalid number of factors")
            return()
        }
        range <- c(suppressWarnings(as.numeric(tclvalue(valMin))),
            suppressWarnings(as.numeric(tclvalue(valMax))))
        if(TRUE %in% is.na(range)) {
            tcl("tk_messageBox",icon="error",
            message="Invalid scale response range")
            return()
        }
        else {
            if(range[1]>=range[2]) {
                tcl("tk_messageBox",icon="error",
                message="Invalid scale response range")
                return()
            }
        }
        assign("nfact",valFact,pos=envirFun)
        assign("click",TRUE,pos=envirFun)
        assign("range",range,pos=envirFun)
        tkdestroy(FactRange.tt)
    }
    
    # --------------------------------------------------------
    # Funzione 'design.send'
    # --------------------------------------------------------
    # Chiamata dalla seconda maschera.
    # Invia i dettagli sui nomi e i livelli dei fattori.
    # --------------------------------------------------------
    design.send <- function() {
        namesVal <- NULL
        lev <- NULL
        for (i in 1:nfact) {
            eachNameFact <- paste("valName",LETTERS[i],sep="")
            eachlev <- paste("valLev",LETTERS[i],sep="")
            namesVal <- c(namesVal,tclvalue(eval(parse(text=eachNameFact))))
            lev <- c(lev,tclvalue(eval(parse(text=eachlev))))
        }
        lev <- suppressWarnings(as.numeric(lev))
        
        for(i in 1:length(namesVal))
            if (namesVal[i]=="")
                namesVal[i] <- paste("factor",LETTERS[i])
        
        if(any(is.na(lev))|any(lev<2)) {
            tcl("tk_messageBox",icon="error",
            message="Invalid level specification")
            return()
        }
        assign("namesVal",namesVal,pos=envirFun)
        assign("lev",lev,pos=envirFun)
        
        assign("click",TRUE,pos=envirFun)
        tkdestroy(design.tt)
    }
    
    # --------------------------------------------------------
    # Funzione 'browse'
    # --------------------------------------------------------
    # Ritorna in una list-box gli oggetti del workspace.
    # --------------------------------------------------------
    browse <- function() {
        selectLS <- function() {
            choice <- ls(.GlobalEnv)[as.numeric(tkcurselection(boxLS))+1]
            dataVar <- tclVar(choice)
            tkdestroy(box)
            # Fa comparire il nome dell'oggetto scelto della casella di testo:
            tkconfigure(cellData,textvariable=dataVar,background=bg)
            assign("dataVar",dataVar,pos=envirFun)
        }
        
        box <- tktoplevel()
        tktitle(box) <- "Browse"
        labelLS <- tklabel(box,
            text="Select a data object from the workspace",foreground="black")
        boxLS <- tklistbox(box,selectmode="single",highlightcolor="white",
            selectforeground="white",selectbackground="gray",background="white",
            yscrollcommand=function(...)tkset(scrLS,...))
        # Riempimento della box-list:
        for(i in 1:length(ls(.GlobalEnv)))
            tkinsert(boxLS,"end",ls(.GlobalEnv)[i])
        # Scrollbar:
        scrLS <- tkscrollbar(box,repeatinterval=1,command=function(...)tkyview(boxLS,...))
        selectBut <-tkbutton(box,text=" Select ",relief="raised",default="active",command=selectLS)
        
        tkgrid(tklabel(box,text=""))
        tkgrid(labelLS,row=1,column=1,columnspan=2)
        tkgrid(tklabel(box,text=""))
        tkgrid(boxLS,row=2,column=1,sticky="e")
        tkgrid(scrLS,row=2,column=2)
        tkgrid.configure(scrLS,sticky="nsw")
        tkgrid(tklabel(box,text=""))
        tkgrid(selectBut,row=3,column=1,columnspan=2)
        tkgrid(tklabel(box,text=""))
    }
    
    # --------------------------------------------------------
    # Funzione 'load.data'
    # --------------------------------------------------------
    # Richiama l'input dei dati.
    # --------------------------------------------------------
    load.data <- function() {
        if(tclvalue(loadVar)=="1") file.load()
        else browse()
    }
    
    # --------------------------------------------------------
    # Funzione 'file.load'
    # --------------------------------------------------------
    # Crea una maschera per la scelta del file da caricare.
    # --------------------------------------------------------
    file.load <- function() {
     
        #--------------------#
        # Selezione del file #
        #--------------------#
        filetypes <- c(
            "{{All files}      * }",
            "{{Text Files}  {.txt}}",
            "{{Excel Files} {.xls}}",
            "{{CSV Files}   {.csv}}",
            "{{SPSS Files}  {.sav}}"
        )
        # Se il SO non e' Windows non e' possibile caricare file xls
        if(.Platform$OS.type!="windows")
            filetypes <- filetypes[-3]
        repeat {
            if(length(filetypes)==1) break
            filetypes <- c(
                paste(filetypes[1],filetypes[2],sep="\n"),
                filetypes[-c(1,2)]
            )
        }
        
        file <- tclvalue(tkgetOpenFile(filetypes=filetypes))
        
        if(nchar(file)==0) return()
        file <- as.character(file)
        ext <- substr(file,(nchar(file)-2),nchar(file))
        
        if(ext!="txt" & ext!="csv" & ext!="xls" & ext!="sav") {
            tcl("tk_messageBox",icon="error",message="Format file not supported.")
            return()
        }
        
        #----------------------------------#
        # Funzione per la lettura dei dati #
        #----------------------------------#
        read.data <- function() {
            # Check sul nome della variabli
            sd <- symbol.detect(tclvalue(DATA_NAME))
            if(sd==0) {
                msg <- "Invalid symbol in the data variable name"
                tcl("tk_messageBox",icon="error",message=msg)
                return()
            }
            
            # Check sull'header
            headVal <- "error"
            if(tclvalue(headVar)=="TRUE" |tclvalue(headVar)=="T")
                headVal<-TRUE
            if(tclvalue(headVar)=="FALSE"|tclvalue(headVar)=="F")
                headVal<-FALSE
            if(headVal=="error") {
                msg <- paste("Syntax error in column header",
                    "specification: header must be TRUE or FALSE")
                tcl("tk_messageBox",icon="error",message=msg)
                return()
            }
            
            sepVal <- tclvalue(sepVar)
            decVal <- tclvalue(decVar)
            sheetVal <- tclvalue(sheetVar)
            
            if(ext=="txt" | ext=="csv")
                dataTable <- read.table(file,header=headVal,sep=sepVal,dec=decVal)
            
            if(ext=="xls") {
                channel <- odbcConnectExcel(file)
                dataTable <- sqlFetch(channel,sheetVal)
                odbcClose(channel)
            }
            
            if(ext=="sav")
                dataTable <- read.spss(file,to.data.frame=TRUE)
            
            assign("dataVar",DATA_NAME,pos=envirFun)
            assign(tclvalue(DATA_NAME),dataTable,pos=.GlobalEnv)
            
            cat("\n","Data loaded in",
            paste("'",tclvalue(DATA_NAME),"'",sep=""),"object","\n\n")
            tkdestroy(load.tt)
        }
        
        #----------------------------------#
        # Menu per il caricamento dei dati #
        #----------------------------------#
        
        load.tt <- tktoplevel()
        tktitle(load.tt) <- "File input structure"
            
        DATA_NAME <- tclVar("") # to export in the .GlobalEnv
        headVar <- tclVar("TRUE")
        if(ext=="csv") sepVar <- tclVar(";")
            else sepVar <- tclVar()
        decVar <- tclVar(".")
        sheetVar <- tclVar("Sheet1")
            
        labelDN <- tklabel(load.tt,text="Data Variable Name")
        labelHead <- tklabel(load.tt,text="  Column Header (T/F)")
        labelSep <- tklabel(load.tt,text="  Row Separator")
        labelDec <- tklabel(load.tt,text="  Decimal Separator")
        labelSheet <- tklabel(load.tt,text="  Excel Sheet Name")
        
        ## Query functions
        
        qName.fun <- function()
            tcl("tk_messageBox",type="ok",icon="info",
            message="Name of variable wherein to load the data.")
        qHead.fun <- function()
            tcl("tk_messageBox",type="ok",icon="question",
            message="The first row of data matrix contain the column names? Insert T (TRUE) or F (FALSE).")
        qSep.fun <- function()
            tcl("tk_messageBox",type="ok",icon="info",
            message="Row separator of data, usually a empty space (or ';' for csv files).")
        qDec.fun <- function()
            tcl("tk_messageBox",type="ok",icon="info",
            message="Decimal separator, usually a dot or a comma.")
        qSheet.fun <- function()
            tcl("tk_messageBox",type="ok",icon="info",
            message="Excel sheet from which to load the data.")
            
        ## Query buttons
            
        qName.but <- tkbutton(load.tt,text="?",state="active",command=qName.fun)
        qHead.but <- tkbutton(load.tt,text="?",state="active",command=qHead.fun)
        qSep.but <- tkbutton(load.tt,text="?",state="active",command=qSep.fun)
        qDec.but <- tkbutton(load.tt,text="?",state="active",command=qDec.fun)
        qSheet.but <- tkbutton(load.tt,text="?",state="active",command=qSheet.fun)
            
        cellDN  <- tkentry(load.tt,textvariable=DATA_NAME,background=bg)
            
        if(ext=="sav") {
            cellHead <- tkentry(load.tt,textvariable=headVar,state="disabled")
            cellSep <- tkentry(load.tt,textvariable=sepVar,state="disabled")
            cellDec <- tkentry(load.tt,textvariable=decVar,background=bg,state="disabled")
            cellSheet <- tkentry(load.tt,textvariable=sheetVar,state="disabled")
        }
        if(ext=="xls") {
            cellHead <- tkentry(load.tt,textvariable=headVar,background=bg)
            cellSep <- tkentry(load.tt,textvariable=sepVar,background=bg)
            cellDec <- tkentry(load.tt,textvariable=decVar,background=bg)
            cellSheet <- tkentry(load.tt,textvariable=sheetVar,background=bg)
        }
        if(ext=="txt" | ext=="csv") {
            cellHead <- tkentry(load.tt,textvariable=headVar,background=bg)
            cellSep <- tkentry(load.tt,textvariable=sepVar,background=bg)
            cellDec <- tkentry(load.tt,textvariable=decVar,background=bg)
            cellSheet <- tkentry(load.tt,textvariable=sheetVar,state="disabled")
        }
        
        sendBut <- tkbutton(load.tt,text=" Load Data ",default="active",relief="raised",command=read.data)
            
        tkgrid(tklabel(load.tt,text=""),
            tklabel(load.tt,text=""),
            tklabel(load.tt,text=""))
        tkgrid(labelDN,cellDN,qName.but,tklabel(load.tt,text=""))
        tkgrid(labelHead,cellHead,qHead.but,tklabel(load.tt,text=""))
        tkgrid(labelSep,cellSep,qSep.but,tklabel(load.tt,text=""))
        tkgrid(labelDec,cellDec,qDec.but,tklabel(load.tt,text=""))
        tkgrid(labelSheet,cellSheet,qSheet.but,tklabel(load.tt,text=""))
        tkgrid(tklabel(load.tt,text=""))
        tkgrid(tklabel(load.tt,text=""),sendBut)
        tkgrid(tklabel(load.tt,text=""))
        tkwait.window(load.tt)
        
        # Fa comparire il nome dell'oggetto della casella di testo:
        tkconfigure(cellData,textvariable=DATA_NAME,background=bg)
    }
    
    # --------------------------------------------------------
    # Funzione 'edit.data'
    # --------------------------------------------------------
    # Costruisce l'editor dei dati (attualmente non in uso).
    # --------------------------------------------------------
    edit.data <- function() {
        if(!any(ls(.GlobalEnv)==tclvalue(dataVar))) {
            if(tclvalue(dataVar)=="")
                msg <- "You must insert the dataset name"
            else
                msg <- paste("Object",paste("'",tclvalue(dataVar),"'",sep=""),
                    "not found in the workspace.")
            tcl("tk_messageBox",icon="error",message=msg)
            return()
        }
        sd <- symbol.detect(tclvalue(OUT_NAME))
        if(sd==0) {
            msg <- "Invalid symbol in the output variable name"
            tcl("tk_messageBox",icon="error",message=msg)
            return()
        }
        dataVal <- get(tclvalue(dataVar))
        dataValMod <- edit(dataVal)
        
        # Bisogna confrontare la matrice originale (dataVal) con
        # quella su cui ha lavorato l'utente (dataValMod) per capire
        # se c'è stata una modifica (solo in questo caso dovrà essere
        # dato un output). Il confronto potrà essere eseguito solo
        # se righe e colonne delle due matrici coincidono; se così
        # non fosse, le due non sarebbero confrontabili ma comunque
        # sarebbe ovvio che una modifica c'è stata.
        
        if( (ncol(dataVal)==ncol(dataValMod)) &
            (nrow(dataVal)==nrow(dataValMod)) ) {
            # Numero di elementi dentro la matrice modificata:
            elemNum <- length(dataValMod[!is.na(dataValMod)])
            # Numero di elementi uguali fra le due matrici:
            matchNum <- sum(dataVal==dataValMod,na.rm=TRUE)
            # Se il numero di elementi uguali fra la matrice nuova e
            # quella vecchia è uguale al numero di elementi contenuti
            # nella matrice vecchia, significa che questa non è stata
            # modificata.
        }
        else {
            # Assegno dei valori casuali a elemNum e matchNum: infatti,
            # dato che l'if di prima non ha avuto buon esito, so già che
            # le due matrici sono diverse, quindi assegno due numeri
            # diversi alle due variabili:
            elemNum <- 1
            matchNum <-2
        }
        if(elemNum!=matchNum) {
            assign(tclvalue(dataVar),dataValMod,pos=.GlobalEnv)
            cat("\n","The data object",
            paste("'",tclvalue(dataVar),"'",sep=""),"has been edited","\n\n")
        }
    }
    
    # --------------------------------------------------------
    # Funzione 'filter.data'
    # --------------------------------------------------------
    # Costruisce un'interfaccia per il filtro dei dati
    # (attualmente non in uso).
    # --------------------------------------------------------
    filter.data <- function() {
        # Verifica che il nome dell'oggetto in input
        # corrisponda a un oggetto presente nel workspace:
        if ((tclvalue(dataVar)=="")) {
            tcl("tk_messageBox",type="ok",icon="error",
                message="Data name cell is empty")
            return()
        }
        if(!any(tclvalue(dataVar)==ls(.GlobalEnv))) {
            tcl("tk_messageBox",type="ok",icon="error",
                message="The data object not exsist")
            return()
        }
        # Lancia il filtro dei dati:
        executeFilter <- function() {
            if (tclvalue(dataVar)=="") {
                tcl("tk_messageBox",type="ok",icon="error",
                    message="Data name cell is empty")
                return()
            }
            
            if(tclvalue(newDataVar)=="")
                newDataVar <- tclVar(tclvalue(dataVar))
            
            dataTable <- get(tclvalue(dataVar))
            rowVal <- index.detect(tclvalue(rowVar))
            colVal <- index.detect(tclvalue(colVar))
            
            # Verifica la correttezza degli indici di riga e di colonna:
            if(rowVal[1]==0 | colVal[1]==0) {
                tcl("tk_messageBox",icon="warning",
                message="Error in the filter definition")
                return()
            }
            if(rowVal[1]=="") rowVal <- 1:nrow(dataTable)
            if(colVal[1]=="") colVal <- 1:ncol(dataTable)
            if(symbol.detect(tclvalue(newDataVar))==0) {
                msg <- "Invalid symbol in the new variable name"
                tcl("tk_messageBox",icon="error",message=msg)
                return()
            }
            
            newDataTable <- dataTable[rowVal,colVal]
            newDataTable <- matrix(newDataTable,length(rowVal),length(colVal))
            colnames(newDataTable) <- colnames(dataTable)[colVal]
            assign(tclvalue(newDataVar),newDataTable,pos=.GlobalEnv)
            cat("\n","Filtered data loaded in",
            paste("'",tclvalue(newDataVar),"'",sep=""),"object","\n\n")
            rm(newDataVar)
        }
        
        filter.tt <- tktoplevel()
        tktitle(filter.tt) <- "Filter data"
        rowVar <- tclVar()
        colVar <- tclVar()
        newDataVar <- tclVar()
        
        filter.par <- tkframe(filter.tt,pady=5)
        
        explain <- "Select a row/column subset of data matrix:"
        filterLabel <- tklabel(filter.par,text=explain,foreground="black")
        
        rowLabel <- tklabel(filter.par,text="Rows",foreground="black")
        rowCell <- tkentry(filter.par,textvariable=rowVar,background=bg)
        
        colLabel <- tklabel(filter.par,text="Columns",foreground="black")
        colCell <- tkentry(filter.par,textvariable=colVar,background=bg)
        
        newDataLabel <- tklabel(filter.par,text="New data object",foreground="black")
        newDataCell <- tkentry(filter.par,textvariable=newDataVar,background=bg)
        
        qRow.fun <- function() {
            msg <- paste(
                "Insert the row numbers separated with a comma",
                "and/or the row interval, e.g. 1:3,5,7,9:10")
            tcl("tk_messageBox",type="ok",icon="info",message=msg)
        }
        qCol.fun <- function() {
            msg <- paste(
                "Insert the column numbers separated with a comma",
                "and/or the column interval, e.g. 1:3,5,7,9:10")
            tcl("tk_messageBox",type="ok",icon="info",message=msg)
        }
        qData.fun <- function() {
            msg <- "Insert a name for the new data object"
            tcl("tk_messageBox",type="ok",icon="info",
            message=msg)
        }
        
        queryRowBut <- tkbutton(filter.par,text="?",command=qRow.fun)
        queryColBut <- tkbutton(filter.par,text="?",command=qCol.fun)
        queryDataBut <- tkbutton(filter.par,text="?",command=qData.fun)
        
        filter.but <- tkframe(filter.tt,pady=5)
        filterBut <- tkbutton(filter.but,text="Filter",width=7,
            default="active",relief="raised",command=executeFilter)
        closeBut <- tkbutton(filter.but,text="Close",width=7,default="active",
            relief="raised",command=function() tkdestroy(filter.tt))
        
        tkgrid(filter.par)
        tkgrid(filterLabel,row=1,columnspan=3)
        tkgrid(rowLabel,row=2,column=1,sticky="e")
        tkgrid(rowCell,row=2,column=2,sticky="e")
        tkgrid(queryRowBut,row=2,column=3,sticky="w")
        
        tkgrid(colLabel,row=3,column=1,sticky="e")
        tkgrid(colCell,row=3,column=2,sticky="e")
        tkgrid(queryColBut,row=3,column=3,sticky="w")
        
        tkgrid(newDataLabel,row=4,column=1,sticky="e")
        tkgrid(newDataCell,row=4,column=2,sticky="e")
        tkgrid(queryDataBut,row=4,column=3,sticky="w")
        
        tkgrid(filter.but)
        tkgrid(filterBut,row=1,column=1,sticky="e")
        tkgrid(closeBut,row=1,column=2,sticky="w")
    }
    
    # --------------------------------------------------------
    # Funzione 'analyze'
    # --------------------------------------------------------
    # Esegue l'analisi con rav.
    # --------------------------------------------------------
    analyze <- function() {
        if(!any(ls(.GlobalEnv)==tclvalue(dataVar))) {
            tcl("tk_messageBox",icon="error",
                message=paste("Object",paste("'",tclvalue(dataVar),"'",sep=""),
                "not found in the workspace."))
            return()
        }
        
        sd <- symbol.detect(tclvalue(OUT_NAME))
        if(sd==0) {
            msg <- "Invalid symbol in the output variable name"
            tcl("tk_messageBox",icon="error",message=msg)
            return()
        }
        
        dataTable <- get(tclvalue(dataVar))
        if(dim(dataTable)[2]==1) dataTable <- t(dataTable)
        
        #-------------------------------------#
        # Start vector: contruction and check #
        #-------------------------------------#
        
        startVal <- list(s0w0=NULL,s=NULL,w=NULL)
        startVal$s0w0 <- c(tclvalue(startVar.s0),tclvalue(startVar.w0))
        for(i in 1:length(allNamesVar.s$start)) {
            startVal$s <- c(startVal$s,tclvalue(eval(parse(text=allNamesVar.s$start[i]))))
            startVal$w <- c(startVal$w,tclvalue(eval(parse(text=allNamesVar.w$start[i]))))
        }
        startVal <- suppressWarnings(as.numeric(unlist(startVal)))
        
        if(any(is.na(startVal))) {
            tcl("tk_messageBox",icon="error",message=paste("Invalid start bound"))
            return()
        }
        
        #-------------------------------------#
        # Lower vector: contruction and check #
        #-------------------------------------#
        
        lowerVal <- list(s0w0=NULL,s=NULL,w=NULL)
        lowerVal$s0w0 <- c(tclvalue(lowerVar.s0),tclvalue(lowerVar.w0))
        for(i in 1:length(allNamesVar.s$lower)) {
            lowerVal$s <- c(lowerVal$s,tclvalue(eval(parse(text=allNamesVar.s$lower[i]))))
            lowerVal$w <- c(lowerVal$w,tclvalue(eval(parse(text=allNamesVar.w$lower[i]))))
        }
        lowerVal <- suppressWarnings(as.numeric(unlist(lowerVal)))
        
        if(any(is.na(lowerVal))) {
            tcl("tk_messageBox",icon="error",message=paste("Invalid lower bound"))
            return()
        }
        
        #-------------------------------------#
        # Upper vector: contruction and check #
        #-------------------------------------#
        
        upperVal <- list(s0w0=NULL,s=NULL,w=NULL)
        upperVal$s0w0 <- c(tclvalue(upperVar.s0),tclvalue(upperVar.w0)) 
        for(i in 1:length(allNamesVar.s$upper)) {
            upperVal$s <- c(upperVal$s,tclvalue(eval(parse(text=allNamesVar.s$upper[i]))))
            upperVal$w <- c(upperVal$w,tclvalue(eval(parse(text=allNamesVar.w$upper[i]))))
        }
        upperVal <- suppressWarnings(as.numeric(unlist(upperVal)))
        
        if(any(is.na(upperVal))) {
            tcl("tk_messageBox",icon="error",message=paste("Invalid upper bound"))
            return()
        }
        
        #-----------------#
        # Further Options #
        #-----------------#
        
        # All
        if(tclvalue(chkVarAll)=="1") allVal <- TRUE
        else allVal <- FALSE
        # Verbose
        if(tclvalue(chkVarVerbose)=="1") verboseVal <- TRUE
        else verboseVal <- FALSE
        # Title
        if(tclvalue(titleVar)=="") titleVal <- NULL
        else titleVal <- tclvalue(titleVar)
        # IC difference
        IC.diff <- c(
            as.numeric(tclvalue(BIC.diff)),
            as.numeric(tclvalue(AIC.diff))
        )
        
        #--------#
        # Output #
        #--------#
        output <- rav(
            data = dataTable,
            subset = NULL,
            lev = lev,
            all = allVal,
            sim = as.numeric(tclvalue(simVar)),
            start = startVal,
            lower = lowerVal,
            upper = upperVal,
            I0 = as.logical(as.numeric(tclvalue(chkVarI0))),
            s.fixed = as.logical(as.numeric(tclvalue(chkVarS.fixed))),
            w.fixed = as.logical(as.numeric(tclvalue(chkVarW.fixed))),
            IC.diff = IC.diff,
            delta.weights = as.numeric(tclvalue(delta.weights)),
            method = "L-BFGS-B",
            control = list(),
            title = titleVal,
            names = namesVal,
            verbose = verboseVal
        )
        
        assign(tclvalue(OUT_NAME),output,pos=.GlobalEnv)
        show(output)
        cat("\n")
        cat("\n","Output loaded in",
        paste("'",tclvalue(OUT_NAME),"'",sep=""),"object","\n\n")
    }
    
    # ====================================================== #
    #              Costruzione dell'interfaccia              #
    # ====================================================== #
    
    # --------------------------------------------------------
    # Specificazione del numero di fattori e del range
    # --------------------------------------------------------
    
    # Se lev e range non sono stati definiti dall'utente, compare
    # una maschera per l'inserimento delle variabili:
    if(is.null(lev)|is.null(range)) {
        
        FactRange.tt <- tktoplevel()
        tktitle(FactRange.tt) <- " "
        click <- FALSE # se dopo il click sarà ancora F, verrà dato errore
        # Costruzione interfaccia -------
        valFact <- tclVar()
        valMin <- tclVar()
        valMax <- tclVar()
        fontHeading <- tkfont.create(family="times",size=12,weight="bold",slant="italic")
        headFrame <- tkframe(FactRange.tt,pady=17)
        labelTitle <- tklabel(headFrame,text="R-Average options",
            font=c("Bold",14),foreground="red",font=fontHeading)
        factFrame <- tkframe(FactRange.tt,pady=5,padx=25)
        labelFact <- tklabel(factFrame,text="Number of factors:",foreground=fg)
        cellFact <- tkentry(factFrame,textvariable=valFact,width=3,background=bg)
        rangeFrame <- tkframe(FactRange.tt,pady=5)
        labelRange <- tklabel(rangeFrame,text="Scale response range:",foreground=fg)
        labelMin <- tklabel(rangeFrame,text="Min",foreground=fg)
        cellMin <- tkentry(rangeFrame,textvariable=valMin,width=3,background=bg)
        labelMax <- tklabel(rangeFrame,text="Max",foreground=fg)
        cellMax <- tkentry(rangeFrame,textvariable=valMax,width=3,background=bg)
        butFrame <- tkframe(FactRange.tt,pady=15)
        factBut <- tkbutton(butFrame,text="Next",width=8,default="active",
            relief="raised",command=fact.send)
        # Mostra a video ----------------
        tkgrid(headFrame)
        tkgrid(labelTitle)
        # ---
        tkgrid(factFrame)
        tkgrid(labelFact,row=1,column=1,sticky="w")
        tkgrid(cellFact,row=1,column=2,sticky="e")
        # ---
        tkgrid(rangeFrame)
        tkgrid(labelRange,row=1,columnspan=4)
        tkgrid(labelMin,row=2,column=1,sticky="e")
        tkgrid(cellMin,row=2,column=2,sticky="w")
        tkgrid(labelMax,row=2,column=3,sticky="e")
        tkgrid(cellMax,row=2,column=4,sticky="w")
        # ---
        tkgrid(butFrame)
        tkgrid(factBut)
        # ---
        tkwait.window(FactRange.tt)
        if (!click) return()
        
        # --------------------------------------------------------
        # Specificazione del disegno sperimentale
        # --------------------------------------------------------
        
        design.tt <- tktoplevel()
        tktitle(design.tt) <- " "
        click <- FALSE # se dopo il click sarà ancora F, verrà dato errore
        
        labelTitle <- tklabel(design.tt,text="R-Average options",
            font=c("Bold",14),foreground="red",font=fontHeading)
        lev <- FALSE # se alla fine sarà ancora false, verrà dato errore
        labelNames <- tklabel(design.tt,text="Factor Names",foreground=fg)
        labelLevels <- tklabel(design.tt,text="Levels Num.",foreground=fg)
        designBut <- tkbutton(design.tt,text="Next",width=8,default="active",
            relief="raised",command=design.send)
        
        tkgrid(tklabel(design.tt,text=""),row=1)
        tkgrid(labelTitle,row=2,columnspan=4)
        tkgrid(tklabel(design.tt,text=""),row=3)
        tkgrid(tklabel(design.tt,text=""),labelNames,labelLevels,tklabel(design.tt,text=""))
        for(i in 1:nfact) {
            # Variabili che conterranno i nomi dei fattori:
            eachValName <- paste("valName",LETTERS[i],sep="")
            assign(eachValName,tclVar())
            
            # Variabili che conterranno il numero di livelli:
            eachValLev <- paste("valLev",LETTERS[i],sep="")
            assign(eachValLev,tclVar())
            
            # Etichette coi nomi dei fattori:
            eachLabelFact <- paste("labelFact",LETTERS[i],sep="")
            assign(eachLabelFact,tklabel(design.tt,text=paste("Factor",LETTERS[i]),foreground=fg))
            
            # Celle per i nomi di fattori:
            eachCellFact <- paste("cellFact",LETTERS[i],sep="")
            assign(eachCellFact,tkentry(design.tt,textvariable=eval(parse(text=eachValName)),background=bg))
            
            # Celle per il numero di livelli:
            eachCellLev <- paste("cellLev",LETTERS[i],sep="")
            assign(eachCellLev,tkentry(design.tt,textvariable=eval(parse(text=eachValLev)),background=bg,width=3))
            
            # Output grafico:
            tkgrid(get(eachLabelFact),get(eachCellFact),get(eachCellLev),tklabel(design.tt,text=""))
        }
        tkgrid(tklabel(design.tt,text=""))
        tkgrid(designBut,columnspan=4)
        tkgrid(tklabel(design.tt,text=""))
        
        tkwait.window(design.tt)
        
        if (!click) return()
    }
    
    # --------------------------------------------------------
    # Corpo principale
    # --------------------------------------------------------
    
    tt <- tktoplevel()
    tktitle(tt) <- "R-Average Commander"
    #tkwm.geometry(tt,"540x545")
    
    frame0 <- tkframe(tt,padx=10)
    frame1.1 <- tkwidget(tt,"labelframe",text="Data input",foreground="red",padx=20,pady=15)
    frame1.2 <- tkwidget(tt,"labelframe",text="Output",foreground="red",padx=20,pady=15)
    frame2 <- tkwidget(tt,"labelframe",text="Bound Settings",foreground="red",padx=20,pady=15)
    frame3 <- tkwidget(tt,"labelframe",text="Procedure Options",foreground="red",padx=20,pady=15)
    frame4 <- tkframe(tt,pady=15)
    
    fontHeading <- tkfont.create(family="times",size=16,weight="bold",slant="italic")
    Title <- tklabel(frame0,text="R-Average",font=c("Bold",14),foreground="red",pady=2,font=fontHeading)
    analyzeBut <- tkbutton(frame4,text="Analyze",width=12,default="active", relief="raised",command=analyze)
    
    # --------------------------------
    # Inserimento dei dati
    # --------------------------------
    
    px <- 11
    dataVar <- tclVar() # to extract from .GlobalEnv
    labelData <- tklabel(frame1.1,text="Data name",foreground=fg)
    cellData <- tkentry(frame1.1,textvariable=dataVar,width=px,background=bg)
    browseBut <- tkbutton(frame1.1,text="browse",default="active",relief="raised",command=load.data)
    
    loadVar <- tclVar(1) # decide da dove caricare i dati
    loadRadio1 <- tkradiobutton(frame1.1,text="Load from file",variable=loadVar,value=1,foreground=fg)
    loadRadio2 <- tkradiobutton(frame1.1,text="Load from R work space",variable=loadVar,value=2,foreground=fg)
    
    # --------------------------------
    # Output
    # --------------------------------
    
    OUT_NAME <- tclVar("fm") # to export in .GlobalEnv
    labelVN <- tklabel(frame1.2,text="Output name",foreground=fg)
    cellVN  <- tkentry(frame1.2,textvariable=OUT_NAME,width=px,background=bg)
    
    printFun <- function() {
        objectName <- tclvalue(OUT_NAME)
        # Se la casella nome è stata lasciata vuota, o nel workspace
        # non c'è alcun oggetto corrispondente al nome, si manda un
        # messaggio di errore.
        if(!any(ls(.GlobalEnv)==objectName)) {
            if(objectName=="")
                msg <- "You must insert the output name"
            else
                msg <- paste("Object",paste("'",objectName,"'",sep=""),
                    "not found in the workspace.")
            tcl("tk_messageBox",icon="error",message=msg)
            return()
        }
        objectOut <- eval(parse(text=objectName))
        if(class(objectOut)=="rav") {
            if (tclvalue(titleVar)=="") objectOut@title <- character(0)
            else objectOut@title <- tclvalue(titleVar)
            if(tclvalue(printVar)=="1") show(objectOut)
            else summary(objectOut)
        }
        else
            stop(paste(objectName,"isn't a object of 'rav' class"))
    }
    
    printVar <- tclVar(1) # decide cosa stampare
    printRadio1 <- tkradiobutton(frame1.2,text="Model selected",variable=printVar,value=1,foreground=fg)
    printRadio2 <- tkradiobutton(frame1.2,text="Fitted models",variable=printVar,value=2,foreground=fg)
    printBut <- tkbutton(frame1.2,text="Print",default="active",relief="raised",command=printFun)
    
    ## Titolo dell'output
    titleVar <- tclVar()
    labeltitle <- tklabel(frame1.2,text="Output Title",foreground=fg)
    celltitle <- tkentry(frame1.2,textvariable=titleVar,background=bg,width=20)
    
    # --------------------------------
    # Start, upper, lower
    # --------------------------------
    
    ## Costruzione delle intestazioni delle celle
    labelRowStart.s <- tklabel(frame2,text="Starting values",foreground=fg)
    labelRowUpper.s <- tklabel(frame2,text="Upper values",foreground=fg)
    labelRowLower.s <- tklabel(frame2,text="Lower values",foreground=fg)
    labelRowStart.w <- tklabel(frame2,text="Starting values",foreground=fg)
    labelRowUpper.w <- tklabel(frame2,text="Upper values",foreground=fg)
    labelRowLower.w <- tklabel(frame2,text="Lower values",foreground=fg)
    
    # --------------------------------
    # Opzioni
    # --------------------------------
    
    chkVarAll <- tclVar(FALSE)
    chkAll <- tkcheckbutton(frame3,text="  All weight combinations",variable=chkVarAll,foreground=fg)
    
    chkVarI0 <- tclVar(FALSE)
    chkI0 <- tkcheckbutton(frame3,text="I0",variable=chkVarI0,foreground=fg)
    
    chkVarS.fixed <- tclVar(FALSE)
    chkS.fixed <- tkcheckbutton(frame3,text="s-fixed",variable=chkVarS.fixed,foreground=fg)
    
    chkVarW.fixed <- tclVar(FALSE)
    chkW.fixed <- tkcheckbutton(frame3,text="w-fixed",variable=chkVarW.fixed,foreground=fg)
    
    chkVarVerbose <- tclVar(FALSE)
    chkVerbose <- tkcheckbutton(frame3,text="  Verbose output",variable=chkVarVerbose,foreground=fg)
    
    simVar <- tclVar(0)
    sim.Cell <- tkentry(frame3,textvariable=simVar,width=5,background=bg)
    sim.label <- tklabel(frame3,text="  sim  ",foreground=fg)
    
    BIC.diff <- tclVar(2.5)
    AIC.diff <- tclVar(0.01)
    BIC.diff.Cell <- tkentry(frame3,textvariable=BIC.diff,width=5,background=bg)
    BIC.diff.label <- tklabel(frame3,text="  BIC Difference  ",foreground=fg)
    AIC.diff.Cell <- tkentry(frame3,textvariable=AIC.diff,width=5,background=bg)
    AIC.diff.label <- tklabel(frame3,text="  AIC Difference  ",foreground=fg)
    
    delta.weights <- tclVar(0.1)
    delta.weights.Cell <- tkentry(frame3,textvariable=delta.weights,width=5,background=bg)
    delta.weights.label <- tklabel(frame3,text="  Delta Weights  ",foreground=fg)
    
    # --------------------------------
    # Mostra a video
    # --------------------------------
    
    ##################################
    tkgrid(frame0,sticky="e")
    ##################################
    tkgrid(Title)
    
    ##################################
    tkgrid(frame1.1,sticky="we")
    ##################################
    tkgrid(labelData,row=1,column=1,sticky="e")
    tkgrid(cellData,row=1,column=2,sticky="w")
    tkgrid(browseBut,row=1,column=3,sticky="w")
    # ----
    tkgrid(loadRadio1,row=1,column=4,sticky="w")
    tkgrid(loadRadio2,row=2,column=4,sticky="w")
    # ---
    # editBut <- tkbutton(frame1.1,text="Edit data",default="active",relief="raised",command=edit.data)
    # filterBut <- tkbutton(frame1.1,text="Filter data",default="active",relief="raised",command=filter.data)
    # ----
    # tkgrid(editBut,row=3,column=1)
    # tkgrid(filterBut,row=3,column=2)
    
    ##################################
    tkgrid(frame1.2,sticky="we")
    ##################################
    tkgrid(labelVN,row=1,column=1,sticky="e")
    tkgrid(cellVN,row=1,column=2,sticky="w")
    tkgrid(printBut,row=1,column=3,sticky="w")
    # ---
    tkgrid(printRadio1,row=1,column=4,sticky="w")
    tkgrid(printRadio2,row=2,column=4,sticky="w")
    # ---
    tkgrid(labeltitle,row=3,column=1,sticky="e")
    tkgrid(celltitle, row=3,column=2,columnspan=3,sticky="w")
    
    ##################################
    tkgrid(frame2,sticky="we")
    ##################################
    
    ## Nomi dei fattori
    incr <- 2
    posLabel <- c(0,cumsum(lev)[-length(cumsum(lev))])
    for(i in 1:length(posLabel)) {
        tkgrid(
            tklabel(frame2,text=namesVal[i],foreground="red"),
            row=1,column=posLabel[i]+1+incr,columnspan=lev[i]
        )
        incr <- incr+1
    }
    
    ## Nomi dei parametri
    parNames <- list(s=NULL,w=NULL)
    for(i in 1:length(lev)) {
        parNames$s <- c(parNames$s,paste(rep("s",lev[i]),1:lev[i],sep=""))
        parNames$w <- c(parNames$w,paste(rep("w",lev[i]),1:lev[i],sep=""))
    }
    parNames <- unlist(parNames)
    
    #----------------#
    # Stato iniziale #
    #----------------#
    
    px <- 5 # cell width
    
    startVar.s0 <- tclVar(range[1]/2)
    startVar.w0 <- tclVar(1)
    lowerVar.s0 <- tclVar(range[1])
    lowerVar.w0 <- tclVar(1e-5)
    upperVar.s0 <- tclVar(range[2]/2)
    upperVar.w0 <- tclVar(range[2]/10)
    label.s0 <- tklabel(frame2,text="s0",foreground=fg)
    label.w0 <- tklabel(frame2,text="w0",foreground=fg)
    cellStart.s0 <- tkentry(frame2,textvariable=startVar.s0,background=bg,width=px)
    cellStart.w0 <- tkentry(frame2,textvariable=startVar.w0,background=bg,width=px)
    cellLower.s0 <- tkentry(frame2,textvariable=lowerVar.s0,background=bg,width=px)
    cellLower.w0 <- tkentry(frame2,textvariable=lowerVar.w0,background=bg,width=px)
    cellUpper.s0 <- tkentry(frame2,textvariable=upperVar.s0,background=bg,width=px)
    cellUpper.w0 <- tkentry(frame2,textvariable=upperVar.w0,background=bg,width=px)
    
    tkgrid(label.s0,row=2,column=2,ipadx=12)
    tkgrid(cellUpper.s0,row=3,column=2)
    tkgrid(cellStart.s0,row=4,column=2)
    tkgrid(cellLower.s0,row=5,column=2)
    
    tkgrid(label.w0,row=6,column=2)
    tkgrid(cellUpper.w0,row=7,column=2)
    tkgrid(cellStart.w0,row=8,column=2)
    tkgrid(cellLower.w0,row=9,column=2)
    
    #------------------------#
    # Valori di scala e pesi #
    #------------------------#
    
    ## Valori di default
    
    upper <- start <- lower <- list(s=NULL,w=NULL)
    for(i in 1:nfact){
        upper$s <- c(upper$s,rep.int(range[2],lev[i]))
        start$s <- c(start$s,rep.int(mean(range),lev[i]))
        lower$s <- c(lower$s,rep.int(range[1],lev[i]))
        upper$w <- c(upper$w,rep.int(10,lev[i]))
        start$w <- c(start$w,rep.int(1,lev[i]))
        lower$w <- c(lower$w,rep.int(1e-5,lev[i]))
    }
    upper <- unlist(upper)
    start <- unlist(start)
    lower <- unlist(lower)
    
    #-------------#
    # Parametri s #
    #-------------#
    
    tkgrid(labelRowUpper.s,row=3,column=1,sticky="e")
    tkgrid(labelRowStart.s,row=4,column=1,sticky="e")
    tkgrid(labelRowLower.s,row=5,column=1,sticky="e")
    space <- " "
    
    ## Mostra a video i nomi dei parametri
    incr <- 2
    for(i in 1:sum(lev)) {
        eachLabel<-paste("label",i,sep="")
        assign(eachLabel,tklabel(frame2,text=parNames[i],foreground=fg))
        if (any(i==cumsum(lev))) {
            tkgrid(get(eachLabel),row=2,column=i+incr)
            incr <- incr+1
            tkgrid(tklabel(frame2,text=space),row=2,column=i+incr)
        }
        else
            tkgrid(get(eachLabel),row=2,column=i+incr)
    }
    
    ## Mostra a video le celle
    incr <- 2
    eachNameVar.s <- allNamesVar.s <- list(start=NULL,lower=NULL,upper=NULL)
    for(i in 1:sum(lev)) {
        # variabili
        eachNameVar.s$start <- paste("startVar.s",i,sep="")
        eachNameVar.s$lower <- paste("lowerVar.s",i,sep="")
        eachNameVar.s$upper <- paste("upperVar.s",i,sep="")
        assign(eachNameVar.s$start,tclVar(start[i]))
        assign(eachNameVar.s$lower,tclVar(lower[i]))
        assign(eachNameVar.s$upper,tclVar(upper[i]))
        # vettori con tutti i nomi delle variabili
        allNamesVar.s$start <- c(allNamesVar.s$start,eachNameVar.s$start)
        allNamesVar.s$lower <- c(allNamesVar.s$lower,eachNameVar.s$lower)
        allNamesVar.s$upper <- c(allNamesVar.s$upper,eachNameVar.s$upper)
        # celle
        cellStart.s <- paste("cellStart.s",i,sep="")
        cellLower.s <- paste("cellLower.s",i,sep="")
        cellUpper.s <- paste("cellUpper.s",i,sep="")
        assign(cellStart.s,tkentry(frame2,textvariable=eval(parse(text=eachNameVar.s$start)),width=px,background=bg))
        assign(cellLower.s,tkentry(frame2,textvariable=eval(parse(text=eachNameVar.s$lower)),width=px,background=bg))
        assign(cellUpper.s,tkentry(frame2,textvariable=eval(parse(text=eachNameVar.s$upper)),width=px,background=bg))
        # mostra a video
        if(any(i==cumsum(lev))) {
            tkgrid(get(cellUpper.s),row=3,column=i+incr)
            tkgrid(get(cellStart.s),row=4,column=i+incr)
            tkgrid(get(cellLower.s),row=5,column=i+incr)
            incr <- incr+1
            tkgrid(tklabel(frame2,text=space),row=3,column=i+incr)
            tkgrid(tklabel(frame2,text=space),row=4,column=i+incr)
            tkgrid(tklabel(frame2,text=space),row=5,column=i+incr)
        }
        else {
            tkgrid(get(cellUpper.s),row=3,column=i+incr)
            tkgrid(get(cellStart.s),row=4,column=i+incr)
            tkgrid(get(cellLower.s),row=5,column=i+incr)
        }
    }
    
    #-------------#
    # Parametri w #
    #-------------#
    
    tkgrid(labelRowUpper.w,row=7,column=1,sticky="e")
    tkgrid(labelRowStart.w,row=8,column=1,sticky="e")
    tkgrid(labelRowLower.w,row=9,column=1,sticky="e")
    
    ## Mostra a video i nomi dei parametri
    incr <- 2
    for(i in (sum(lev)+1):(sum(lev)*2)) {
        eachLabel<-paste("label",i,sep="")
        assign(eachLabel,tklabel(frame2,text=parNames[i],foreground=fg))
        if(any((i-sum(lev)-1)==cumsum(lev))) {
            tkgrid(get(eachLabel),row=6,column=i-sum(lev)+incr)
            incr <- incr+1
            tkgrid(tklabel(frame2,text=space),row=6,column=i+incr)
        }
        tkgrid(get(eachLabel),row=6,column=i-sum(lev)+incr)
    }
    
    ## Mostra a video le celle
    incr <- 2
    eachNameVar.w <- allNamesVar.w <- list(start=NULL,lower=NULL,upper=NULL)
    # Ciclo for sulle celle
    for(i in 1:sum(lev)) {
        # variabili
        eachNameVar.w$start <- paste("startVar.w",i,sep="")
        eachNameVar.w$lower <- paste("lowerVar.w",i,sep="")
        eachNameVar.w$upper <- paste("upperVar.w",i,sep="")
        assign(eachNameVar.w$start,tclVar(1))
        assign(eachNameVar.w$lower,tclVar(0.01))
        assign(eachNameVar.w$upper,tclVar(range[2]/2))
        # vettori con tutti i nomi delle variabili
        allNamesVar.w$start <- c(allNamesVar.w$start,eachNameVar.w$start)
        allNamesVar.w$lower <- c(allNamesVar.w$lower,eachNameVar.w$lower)
        allNamesVar.w$upper <- c(allNamesVar.w$upper,eachNameVar.w$upper)
        # celle
        cellStart.w <- paste("cellStart.w",i,sep="")
        cellLower.w <- paste("cellLower.w",i,sep="")
        cellUpper.w <- paste("cellUpper.w",i,sep="")
        assign(cellStart.w,tkentry(frame2,textvariable=eval(parse(text=eachNameVar.w$start)),width=px,background=bg))
        assign(cellLower.w,tkentry(frame2,textvariable=eval(parse(text=eachNameVar.w$lower)),width=px,background=bg))
        assign(cellUpper.w,tkentry(frame2,textvariable=eval(parse(text=eachNameVar.w$upper)),width=px,background=bg))
        # mostra a video
        if(any(i==cumsum(lev))) {
            tkgrid(get(cellUpper.w),row=7,column=i+incr)
            tkgrid(get(cellStart.w),row=8,column=i+incr)
            tkgrid(get(cellLower.w),row=9,column=i+incr)
            incr <- incr+1
            tkgrid(tklabel(frame2,text=space),row=7,column=i+incr)
            tkgrid(tklabel(frame2,text=space),row=8,column=i+incr)
            tkgrid(tklabel(frame2,text=space),row=9,column=i+incr)
        }
        else {
            tkgrid(get(cellUpper.w),row=7,column=i+incr)
            tkgrid(get(cellStart.w),row=8,column=i+incr)
            tkgrid(get(cellLower.w),row=9,column=i+incr)
        }
    }
    
    ##################################
    tkgrid(frame3,sticky="we")
    ##################################
    
    # Colonna 1
    tkgrid(chkI0,row=1,column=1,sticky="w",columnspan=1)
    tkgrid(chkS.fixed,row=2,column=1,sticky="w",columnspan=1)
    tkgrid(chkW.fixed,row=3,column=1,sticky="w",columnspan=1)
    # Colonna 2
    tkgrid(chkAll,row=1,column=2,sticky="w",columnspan=1)
    tkgrid(chkVerbose,row=2,column=2,sticky="w",columnspan=1)
    # Colonna 3
    tkgrid(BIC.diff.label,row=1,column=3,sticky="e")
    tkgrid(AIC.diff.label,row=2,column=3,sticky="e")
    # Colonna 4
    tkgrid(BIC.diff.Cell,row=1,column=4,sticky="w")
    tkgrid(AIC.diff.Cell,row=2,column=4,sticky="w")
    # Colonna 5
    tkgrid(delta.weights.label,row=1,column=5,sticky="e")
    tkgrid(sim.label,row=2,column=5,sticky="e")
    # Colonna 6 
    tkgrid(delta.weights.Cell,row=1,column=6,sticky="w")
    tkgrid(sim.Cell,row=2,column=6,sticky="w")
    
    ##################################
    tkgrid(frame4)
    ##################################
    tkgrid(analyzeBut,row=1,column=2)
    
    tkfocus(tt)
}
