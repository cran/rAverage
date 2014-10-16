\name{datgen}
\alias{datgen}

\title{Generating Noisy Responses for an Averaging Model}

\description{
    This function generates noisy synthetic responses \code{R} for an averaging model given
    the true parameters \code{s0}, \code{w0}, \code{s(k,j)}, and \code{w(k,j)}.
}

\usage{
datgen(param, lev, t.par = FALSE, trials = 1, sd = 0, range = NULL)
}

\arguments{
    \item{param}{
        Numerical vector containing the true parameters for the function, with
        the order  \code{s0}, \code{w0}, \code{s(k,j)}, and \code{w(k,j)}.
    }
    \item{lev}{
        Vector containing the number of levels of each factor. For instance, two
        factors with respectively 3 and 4 levels require \code{lev = c(3,4)}.
    }
    \item{t.par}{
        Attribute that specifies whether the weight parameters should be written in the 't' form or in the 'w' form.
    }
    \item{trials}{
        Number of rows of the output matrix.
    }
     \item{sd}{
        Standard deviation of the noise addded to the responses R whithin each column of the output matrix.
    }
    \item{range}{
        Numeric vector. Range of the responses.
    }
}

\value{
    A \code{matrix} object containing the generated responses of the averaging model, in the order:
    one-way design, two-way design, three way design, etc. See \code{rav} function.
}

\seealso{
    \code{\link[rAverage]{rav}},
    \code{\link[rAverage]{pargen}},
    \code{\link[rAverage]{rav.indices}},
    \code{\link[rAverage]{rAverage-package}}
    }

\examples{\dontrun{
# Generating random parameters for a 3x4 design:
par <- pargen(lev = c(3,4), s.range = c(0,20))
# Computing the responses:
R <- datgen(param=par, lev=c(3,4), sd=0) ; R
R <- datgen(param=par, lev=c(3,4), sd=1, trials=10, range=c(0,20)) ; R
}}

\keyword{misc}