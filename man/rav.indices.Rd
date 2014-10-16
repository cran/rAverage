\name{rav.indices}
\alias{rav.indices}
\alias{rav.fitted}
\alias{rav.param}
\alias{rav.resid}
\alias{rav.AIC}
\alias{rav.BIC}

\title{Fit indices for averaging models}

\description{
    Group of functions to extract fit indices, parameters, fitted values and residuals. The function
    \code{fit.indices} returns the fit indices for the averaging model given the parameters
    \code{s0}, \code{w0}, \code{s(k,j)}, and \code{w(k,j)}.
    The functions \code{rav.AIC} and \code{rav.BIC} extract the Akaike Information Criterion and
    the Bayesian Information Criterion from a fitted model.
    The functions \code{rav.param}, \code{rav.fitted} and \code{rav.resid}, extract respectively the
    parameters, fitted values and residuals from an estimated averaging model. The function
    \code{rav.resid} extracts, besides the matrix of residuals, the sum of squared residuals for
    both columns and rows of the data matrix.
}

\usage{
rav.indices(param, lev, data, t.par = FALSE, subset = NULL,
    n.pars = NULL, names = NULL, title = NULL)
rav.AIC(object, whichModel = NULL)
rav.BIC(object, whichModel = NULL)
rav.fitted(object, whichModel = NULL)
rav.param(object, whichModel = NULL)
rav.resid(object, whichModel = NULL, standard = FALSE)
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
    \item{object}{
        An object of class \code{rav} containing an estimated averaging model.
    }
    \item{whichModel}{
        Specifies from which model extract the values (parameters, fitted values or fit indices).
        Options are:
        \enumerate{
            \item \code{"null"}: null model
            \item \code{"ESM"}: equal scale values model
            \item \code{"SAM"}: simple averaging model
            \item \code{"EAM"}: equal-weights averaging model
            \item \code{"DAM"}: differential-weight averaging model
            \item \code{"IC"}: information criteria
        }
        As default, the values of the (first) best model will be extracted.
    }
    \item{standard}{
        It specifies if to standardize residuals.
    }
}

\details{
    Returns the main fit indices (AIC, BIC, R-squared, Adjusted R-squared), the estimated
    parameters, the fitted values, the residuals and the squared residuals.
}

\value{
    An object of class \code{"indices"}.
}

\seealso{
    \code{\link[rAverage]{rav}},
    \code{\link[rAverage]{outlier.replace}},
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