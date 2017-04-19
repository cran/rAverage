\name{coef}
\alias{coef}
\alias{coef,rav-method}

\title{Extract coefficients from an averaging model}

\description{
    Function to extract fit model coefficients from an object returned by \code{rav}.
}

\usage{coef(object, ...)}

\arguments{
    \item{object}{An object of class \code{rav} containing an estimated averaging model.}
    \item{...}{Optionally more fitted model objects.}
}

\details{
    Returns the parameters of an averaging model fitted by the \code{rav} function, in the order:
    \code{s0}, \code{w0}, \code{s(k,j)}, and \code{w(k,j)}.
    
    As default, the function extract the cofficients of the (first) best model. The optional argument
    \code{whichModel} can be specified to extract the values of another model. Options are:
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
    A numeric vector.
}

\seealso{
    \code{\link[rAverage]{rav}},
    \code{\link[rAverage]{rAverage-package}}
}

\examples{\dontrun{
data(fmdata1)
fm1 <- rav(fmdata1, lev=c(3,3))
coef(fm1)
coef(fm1, whichModel="EAM")
}}

\keyword{misc}
