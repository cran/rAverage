\name{rAverage}
\alias{rAverage}
\docType{package}

\title{Parameter estimation for the averaging model of Information Integration Theory}

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
        Version: \tab 0.4-12\cr
        Date: \tab 2015-01-12\cr
        License: \tab GNU (version 2 or later)\cr
    }
    
    \emph{Functions of the R-Average package}:\cr
    
    \code{rav}: parameter estimation for averaging models.\cr
    \code{rav.fitted}: extracts the predicted values of the best model from a \code{rav} object.\cr
    \code{rav.resid}: extracts the residuals from a \code{rav} object.\cr
    \code{rav.param}: extracts the parameters from a \code{rav} object.\cr
    \code{outlier.replace}: given an estimated averaging model with the \code{rav} function, it
                            detects and replace outliers from the residual matrix.
    \code{rav.indices}: given a set of parameters \code{s} and \code{w} and a matrix of observed
                        data, it calculates the fit indices for the averaging model.\cr
    \code{datgen}: returns the responses \code{R} for averaging models
                   given the set of parameters \code{s} and \code{w}.\cr
    \code{pargen}: generates pseudorandom parameters for the averaging model.\cr
    \code{rav.grid}: generates an empty matrix in 'rav' format.\cr
	\code{rav.single}: single subjects analysis over an aggregated data matrix.\cr
	\code{rav2file}: store the reesults of \code{rav} into a text file.\cr
}

\author{
    \bold{Supervisor}: Prof. Giulio Vidotto \email{giulio.vidotto@unipd.it}
    
    University of Padova, Department of General Psychology\cr
    QPLab: Quantitative Psychology Laboratory
    \url{http://qplab.psy.unipd.it/}
    
    \bold{version 0.0}:\cr
    Marco Vicentini \email{marco.vicentini@gmail.com}
    
    \bold{version 0.1 and following}:\cr
    Stefano Noventa \email{stefano.noventa@univr.it}\cr
    Davide Massidda \email{davide.massidda@gmail.com}\cr
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

\keyword{rAverage}
\seealso{
    \code{\link[rAverage]{rav}},
    \code{\link[rAverage]{datgen}},
    \code{\link[rAverage]{pargen}},
    \code{\link[rAverage]{rav.indices}},
    \code{\link[rAverage]{fmdata1}},
    \code{\link[rAverage]{pasta}},
    \code{\link[stats]{optim}} 
}
