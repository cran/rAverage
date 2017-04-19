\name{residuals}
\alias{residuals}
\alias{residuals,rav-method}

\title{Extract residuals from an averaging model}

\description{
    Function to extract residuals from an object returned by \code{rav}.
}

\usage{residuals(object, ...)}

\arguments{
    \item{object}{An object of class \code{rav} containing an estimated averaging model.}
    \item{...}{Optionally more fitted model objects.}
}

\details{
    Returns the residuals of an averaging model fitted by the \code{rav} function. When \code{standard
    = TRUE}, residuals will be transformed in z-scale (mean=0 and sd=1 in each column).

    As default, the function extract the residuals of the (first) best model. The optional argument
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
    A matrix of numeric values.
}

\seealso{
    \code{\link[rAverage]{rav}},
    \code{\link[rAverage]{rAverage-package}}
}

\examples{\dontrun{
data(fmdata1)
fm1 <- rav(fmdata1, lev=c(3,3))
residuals(fm1)
residuals(fm1, whichModel="EAM")
residuals(fm1, whichModel="EAM", standard=TRUE)
}}

\keyword{misc}
