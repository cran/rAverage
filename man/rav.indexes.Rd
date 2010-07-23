\name{rav.indexes}
\alias{rav.indexes}

\title{Fit indexes for an averaging model}

\description{
    This function returns the infexes of fit for an averaging model given
    the parameters \code{s0}, \code{w0}, \code{s(k,j)}, and \code{w(k,j)}.
}

\usage{
rav.indexes(param, lev, data, subset=NULL,
    I0=FALSE, names=NULL, title=NULL)
}

\arguments{
    \item{param}{
        Numerical vector containing the parameters for the function, with the order 
        \code{s0}, \code{w0}, \code{s(k,j)}, and \code{w(k,j)}.
    }
    \item{lev}{
        Vector containing the number of levels of each factor. For instance, two factors with
        respectively 3 and 4 levels require \code{lev = c(3,4)}.
    }
    \item{data}{
        A \code{matrix} or a \code{data.frame} object containing the experimental data.
        Each column corresponds to an experimental design (in order: one-way design,
        two-way design, ..., full factorial design; see the example for further details).
        WARNING: previous versions needed a first column filled with the initial
        state values (s0*w0) or \code{NA} values. This is no longer valid. Nevertheless, 
        the first column can be used to indexes the data (see the attribute \code{subset}).
    }
    \item{subset}{
        Character, numeric or factor attribute that selects a subset of experimental
        data for the analysis (see the examples).
    }
    \item{I0}{
        Logical. If set \code{FALSE}, the \code{s0} and \code{w0} parameters are forced to be zero.
        If set \code{TRUE}, the \code{s0} and \code{w0} parameters are free to be estimated.
    }
    \item{names}{
        Vector of character strings containing the names of the factors.
    }
    \item{title}{
        Character. Puts a title on the output.
    }
}

\details{
    Returns the principal indexes of fit (AIC, BIC, R squared, Adjusted R squared).
}

\value{
    An object of class \code{"indexes"}.
}

\seealso{
    \code{\link[rAverage]{averaging}},
    \code{\link[rAverage]{pargen}},
    \code{\link[rAverage]{rav}},
    \code{\link[rAverage]{rAverage-package}},
    \code{\link[rAverage]{rav.cmd}}
}

\examples{
# data(fmdata1)
# s <- c(12.9, 1.5, 18.3, 5.2, 5.0, 2.3)
# w <- c(1.4, 0.3, 0.5, 1.6, 1.7, 1.7)
# par <- c(0,0, s, w)
# Estimated model by rav:
# fit1 <- rav(fmdata1, lev=c(3,3))
# Fitted model by original parameters:
# fit2 <- rav.indexes(param=par, lev=c(3,3), data=fmdata1)
}

\keyword{misc}