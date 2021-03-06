\name{pargen}
\alias{pargen}

\title{Generating random parameters for averaging responses}

\description{
    Generates a random set of parameters that follows an averaging rule.
}

\usage{
pargen(lev, s.range = c(0,20), w.range = exp(c(-5,5)),
        I0 = FALSE, t.par = FALSE, digits = 2)
}

\arguments{
    \item{lev}{
        Numeric vector. Number of levels of each factor.
    }
    \item{s.range}{
        Numeric vector. Range of variability of the s-parameters.
    }
     \item{w.range}{
        Numeric vector. Range of variability of the w-parameters.
    }
     \item{I0}{
        Logical. If set to \code{FALSE}, parameter s0 and w0 are set to zero.
        If set \code{TRUE} initial parameters are free to be estimated.
    }
    \item{t.par}{
        Specifies if the weight parameters should be the 't' instead the 'w'.
    }
    \item{digits}{
        Numeric. Decimal rounding of the parameters.
    }
}

\value{
    Vector contaning the random-generated parameters in the order \code{s0,w0,s(k,i),w(k,i)}.
}

\seealso{
    \code{\link[rAverage]{datgen}},
    \code{\link[rAverage]{rav}},
    \code{\link[rAverage]{rav.indices}}, 
    \code{\link[rAverage]{rAverage-package}}
}

\examples{
# Generating random parameters for a 3x4 design:
param <- pargen(lev = c(3,4))
}

\keyword{misc}