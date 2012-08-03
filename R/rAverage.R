# 0.3-0     Versione ottimizzata della 0.2, modificata parmean, aggiunto argomento sim
# 0.2-2     Corretto bug sul calcolo di TSS
# 0.2-1     Funzioni averaging, residuals, combin e calcolo dei parameteri implementate in C.
#           Aggiunta possibilita' di fissaggio dei pesi, uguagliamento dei 'pesi uguali'
#               entro un certo delta
#           Generale ottimizzazione del codice
# 0.2-0     Estensione a modelli con infiniti fattori e livelli
#           Generale ottimizzazione del codice
#           Eliminata provvisoriamente models_12 dalla libreria
#           Eliminate alcune funzioni importate nel namespace
#           Cambiati i nomi delle funzioni e delle classi
# 0.1-0     Riorganizzato il codice di models_3
# 0.0-12    Aggiunto l'help delle funzioni, Inserimento dataset sperimentazione
#               Verifica dei modelli add + mul per 4 fattori, Verificare ANOVA-RM
#               sui modelli averaging, Creazione dei file di help e del manuale
#               Inserimento file di esempio
# 0.0-11    Correzione per compatibilita con gcc4 e R2.6
# 0.0-10    Correzione di numerosi bug nelle funzioni di stima
# 0.0-09    Inserire funzione che date osservazioni e parametri,
#               calcola indici di fit (2007/07/21)
#               Inserire nativo Chi-Quadro negli indici di bonta
# 0.0-08    Predisposizione per modelli averaging a 4 fattori (2007/07/17)
#               Verifica di tutte le componenti da rendere pubbliche 
# 0.0-07    rAverage reso come pacchetto autonomo da mvUtils (2007/05/15)
# 0.0-06    Inserimento test Chi-Quadro su modelli Average
# 0.0-05    Organizzazione dei parametri, delle funzioni e delle classi
# 0.0-04    Verifica con R CMD CHECK su Windows XP / minGW / R 2.3.0 (2006/05/10)
#               Verifica con R CMD CHECK su Mac Os X - Tiger / R 2.3.0
#               Installazione del pacchetto rAverage
# 0.0-03    Implentazione autonoma dei modelli additivi e moltiplicativi (2006/04/25)
# 0.0-02    Selezione modelli secondo AIC e BIC congiuntamente
# 0.0-02    Funzioni principali scritte in C (2006/02/15)
# 0.0-01    Funziona con modelli 3x3 e 3x3x3x3 (2006/02/01)

.packageName <- "rAverage"

.onLoad <- function(lib, pkg)
{
    where <- match(paste("package:", pkg, sep = ""), search())
    ver <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
    ver <- as.character(ver)
    title <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Title")
    title <- as.character(title)
    packageStartupMessage(paste(title, " (version ", ver, ")\n", sep = ""))
    library.dynam("rAverage", pkg, lib)
}
