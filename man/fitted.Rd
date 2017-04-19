\name{fitted}
\alias{fitted}
\alias{fitted,rav-method}

\title{Extract fitted values from an averaging model}

\description{
    Function to extract fitted values from an object returned by \code{rav}.
}

\usage{fitted(object, ...)}

\arguments{
    \item{object}{An object of class \code{rav} containing an estimated averaging model.}
    \item{...}{Optionally more fitted model objects.}
}

\details{
    Returns the expected responses given an averaging model fitted by the \code{rav} function.
    
    As default, the function extract the fitted values of the (first) best model. The optional
    argument \code{whichModel} can be specified to extract the values of another model. Options are:
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
library(rAverage)
data(fmdata1)
fm1 <- rav(fmdata1, lev=c(3,3))
fitted(fm1)
fitted(fm1, whichModel="EAM")
}}

\keyword{misc}
