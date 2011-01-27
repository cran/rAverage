\name{rAverage}
\alias{rAverage}
\docType{package}

\title{Parameters estimation for the Averaging model of Information Integration Theory}

\description{
    The R-Average package implements a procedure to identify the
    parameters of the Averaging model of Information Integration Theory (Anderson, 1981),
    following the spirit of the so-called "principle of parsimony".
    
    \emph{Name of the parameters}:\cr
    
    \code{s0,w0}: initial state values of the Averaging Model.\cr
    \code{s(k,j)}: scale value of the \code{j}-th level of \code{k}-th factor.\cr
    \code{w(k,j)}: weight value of the \code{j}-th level of \code{k}-th factor.\cr
}

\details{
    \tabular{ll}{
        Package: \tab rAverage\cr
        Type: \tab Package\cr
        Version: \tab 0.3-5\cr
        Date: \tab 2010-10-31\cr
        License: \tab GNU (version 2 or later)\cr
    }
    
    \emph{Functions of the R-Average package}:\cr
    
    \code{rav}: analyzes the averaging model and identifies the parameters.\cr
    \code{averaging}: returns the responses \code{R} for averaging models
                      given the set of parameters \code{s} and \code{w}.\cr
    \code{pargen}: generates random data for the averaging model.\cr
    \code{rav.cmd}: calls the GUI (graphical user interface) for handling the 
                    attribute of the \code{rav} procedure.\cr
    \code{rav.indexes}: returns the indexes of fit of a model.\cr
}

\author{
    \bold{version 0.0}:\cr
    Marco Vicentini \email{marco.vicentini@gmail.com}
    
    \bold{version 0.1 and following}:\cr
    Stefano Noventa \email{stefano.noventa@unipd.it}\cr
    Davide Massidda \email{davide.massidda@unipd.it}\cr
    
    \bold{Supervisor}: Prof. Giulio Vidotto \email{giulio.vidotto@unipd.it}
    
    University of Padova, Department of General Psychology\cr
    QPLab: Quantitative Psychology Laboratory
    \url{http://qplab.psy.unipd.it/}
}

\references{
    Akaike, H. (1976). Canonical correlation analysis of time series and the use
    of an information criterion. In: R. K. Mehra & D. G. Lainotis (Eds.),
    \emph{System identification: Advances and case studies} (pp. 52-107). New
    York: Academic Press.
    
    Anderson, N. H. (1981). \emph{Foundations of Information Integration Theory}.
    New York: Academic Press.
    
    Anderson, N. H. (1982). \emph{Methods of Information Integration Theory}. New
    York: Academic Press.
    
    Anderson, N. H. (1991). Contributions to information integration theory:
    volume 1: cognition. Lawrence Erlbaum Associates, Hillsdale, New Jersey.
    
    Anderson, N. H. (2007). Comment on article of Vidotto and Vicentini.
    \emph{Teorie & Modelli}, Vol. 12 (1-2), 223-224.
    
    Byrd, R. H., Lu, P., Nocedal, J., & Zhu, C. (1995). A limited memory algorithm
    for bound constrained optimization. \emph{Journal Scientific Computing}, 16, 1190-1208.
    
    Kuha, J. (2004). AIC and BIC: Comparisons of Assumptions and Performance.
    \emph{Sociological Methods & Research}, 33 (2), 188-229.
    
    Nelder, J. A., & Mead, R. (1965). A Simplex Method for Function Minimization.
    \emph{The Computer Journal, 7, 308-313}.
    
    Vidotto, G., Massidda, D., & Noventa, S. (2010). Averaging models: parameters
    estimation with the R-Average procedure. \emph{Psicologica}, 31, 461-475.
    
    Vidotto, G. & Vicentini, M. (2007). A general method for parameter
    estimation of averaging models. \emph{Teorie & Modelli}, Vol. 12 (1-2), 211-221.
}

\keyword{RAverage}
\seealso{
    \code{\link[rAverage]{rav}},
    \code{\link[rAverage]{averaging}},
    \code{\link[rAverage]{pargen}},
    \code{\link[rAverage]{rav.indexes}},
    \code{\link[rAverage]{rav.cmd}},
    \code{\link[rAverage]{fmdata1}},
    \code{\link[rAverage]{pasta}},
    \code{\link[stats]{optim}} 
}
