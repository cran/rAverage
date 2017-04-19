\name{AIC}
\alias{AIC}
\alias{BIC}
\alias{AIC,rav-method}
\alias{BIC,rav-method}

\title{Information Criteria for averaging models}

\description{
    Functions to extract or recalculate the Akaike Information Criterion and the Bayesian Information
    Criterion of an averaging model fitted by the \code{rav} function.
}

\usage{
AIC(object, ..., k = 2)
BIC(object, ...)
}

\arguments{
    \item{object}{An object of class \code{rav} containing an estimated averaging model.}
    \item{...}{Optionally more fitted model objects (see details).}
    \item{k}{Numeric, the penalty per parameter to be used; the default k = 2 is the classical AIC.}
}

\details{
    The functions AIC and BIC are used, respectively, to extract the Akaike Information Criterion and the
    Bayesian Information Criterion of a model fitted by the function \code{rav}.
    
    AIC is calculated as:
    
    \deqn{ AIC = n \ln \left( \frac{RSS}{n} \right) + k p }
    
    where \eqn{n} is the number of data available, \eqn{k} is the penalty per parameter ()usually equal to 2),
    \eqn{p} is the number of parameters and \eqn{RSS} is the residual sum of squares.
    
    BIC is calculated as:
    
    \deqn{ BIC = n \ln \left( \frac{RSS}{n} \right) + \ln(n) p }
    
    As default, when \eqn{n / p < 40}, AIC and BIC are corrected in AICc and BICc:
    
    \deqn{ AICc = AIC + \frac{2 (p+1) p}{n-p-1} }
    \deqn{ BICc = BIC + \frac{\ln(n) (p+1) p}{n-p-1} }
    
    to avoid the correction, set \code{correct = FALSE}. On the contrary, if you want the correction, set
    \code{correct = TRUE}. When the argument \code{correct} is not specified, the rule \eqn{n / p < 40} is
    applied.
    
    As default, the functions extract the indices of the (first) best model. The optional argument
    \code{whichModel} can be specified to extract the indices of another model. Options are:
    \enumerate{
        \item \code{"null"}: null model
        \item \code{"ESM"}: equal scale values model
        \item \code{"SAM"}: simple averaging model
        \item \code{"EAM"}: equal-weights averaging model
        \item \code{"DAM"}: differential-weight averaging model
        \item \code{"IC"}: information criteria model
    }
}

\value{
    A numeric value representing the information criterion of the selected model.
}

\seealso{
    \code{\link[rAverage]{rav}},
    \code{\link[rAverage]{rAverage-package}}
    \code{\link[stats]{AIC}},
    \code{\link[stats]{BIC}}
}

\examples{\dontrun{
data(fmdata1)
fm1 <- rav(fmdata1, lev=c(3,3))
AIC(fm1)
BIC(fm1)
}}

\keyword{misc}
