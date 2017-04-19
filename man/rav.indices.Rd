\name{rav.indices}
\alias{rav.indices}

\title{Fit indices for averaging models}

\description{
    The function \code{fit.indices} returns the fit indices for the averaging model given the parameters
    \code{s0}, \code{w0}, \code{s(k,j)}, and \code{w(k,j)}.}

\usage{
rav.indices(param, lev, data, t.par = FALSE, subset = NULL,
    n.pars = NULL, names = NULL, title = NULL)
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
        the first column can be used to label the data (see the attribute \code{subset}).
    }
    \item{t.par}{
        Specifies whether the weight parameters should be the in 't' form or in the 'w' form.
    }
    \item{subset}{
        Character, numeric or factor attribute that selects a subset of experimental
        data for the analysis (see the examples).
    }
    \item{n.pars}{
        Number of parameters of the model. If \code{NULL}, \code{n.pars} will be calculated
        from the function.
    }
    \item{names}{
        Vector of character strings containing the names of the factors.
    }
    \item{title}{
        Character specifying a title for the output.
    }
}

\details{
    Returns the main fit indices (AIC, BIC, R-squared, Adjusted R-squared), the estimated
    parameters, the fitted values and the residuals of an averaging model.
}

\value{
    An object of class \code{"indices"}.
}

\seealso{
    \code{\link[rAverage]{rav}},
    \code{\link[rAverage]{rAverage-package}}
}

\examples{\dontrun{
data(fmdata1)
s <- c(12.9, 1.5, 18.3, 5.2, 5.0, 2.3)
w <- c(1.4, 0.3, 0.5, 1.6, 1.7, 1.7)
param <- c(NA,NA, s, w)
# Estimated model by rav:
fit1 <- rav(fmdata1, lev=c(3,3)) ; fit1
# Fitted model by original parameters:
fit2 <- rav.indices(param=param, lev=c(3,3), data=fmdata1) ; fit2
}}

\keyword{misc}
