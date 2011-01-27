\name{rav}
\alias{rav}
\alias{indexes-class}
\alias{rav-class}
\alias{fitted,rav-method}
\alias{parameters}
\alias{residuals,rav-method}
\alias{show,rav-method}
\alias{show,indexes-method}
\alias{summary,rav-method}

\title{Analyzing the Family of the Averaging Models}

\description{ 
    \code{rav} (\bold{R}-Average for \bold{AV}eraging models) is a procedure for estimating
    the parameters of the averaging models of Information Integration Theory (Anderson, 1981).
    It provides reliable estimations of weights and scale values for a factorial experimental
    design (with any number of factors and levels) by selecting the most suitable subset of
    the parameters, according to the overall goodness of fit indexes and to the complexity
    of the design. Its attributes can be handled by menas of a GUI (see the \code{rav.cmd}
    command for details).
}

\usage{
rav(data, subset = NULL, lev, all= FALSE, sim = 0,
    range = NULL, start = NULL, lower = NULL, upper = NULL,
    I0 = FALSE, s.fixed = FALSE, w.fixed = FALSE,
    IC.diff = c(2, 2), delta.weights = 0.1,
    method = "L-BFGS-B", control = list(),
    title = NULL, names = NULL, verbose = FALSE)
}

\arguments{
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
    \item{lev}{
        Vector containing the number of levels of each factor. For instance, two factors
        with respectively 3 and 4 levels require \code{lev = c(3,4)}.
    }
    \item{all}{
        Logical. If set \code{TRUE} the information criterion tests all the possible
        combinations of weights (see details). The default value \code{FALSE} implies
        a preselection of a subset of combination based on the results of the previous
        steps of the algorithm. 
        WARNING: with \code{all = TRUE} the procedure is generally more time-consuming
        (depending on the size of the experimental design), but provides more reliable
        estimations than the standard procedure.
    }
    \item{sim}{
        Number of iterations for the Monte-Carlo simulation procedure. The default setting
        \code{0} implies that no Monte-Carlo will be runned.
    }
    \item{range}{
        A vector containing the scale response range. If \code{NULL}, the minimum and maximum
        values of the data matrix will be used. This values will be used as bounds for the
        s-parameters when the minimization algorithm is L-BFGS-B.
    }
    \item{start}{ 
        Vector containing the start scale and weight values, according to the following
        order: \code{s0, w0, s(k,j), w(k,j)}, for \code{k} and \code{j} set by the factorial
        design. With the default setting \code{NULL}, \code{rav} loads the default parameters:
        \code{s0 = 0, w0 = 1, s(k,j) = max(range)/2, w = 1}.
    }
    \item{lower}{
        Vector containing the lower scale and weight values, according to the following
        order: \code{s0, w0, s(k,j), w(k,j)}, for \code{k} and \code{j} set by the factorial
        design. With the default setting \code{NULL}, when the minimization algorithm is
        L-BFGS-B, \code{rav} loads the default parameters:
        \code{s0 = 0, w0 = 1e-5, s(k,j) = min(range), w = 1e-5} for each \code{k} and \code{j}.
    }
    \item{upper}{
        Vector containing the upper scale and weight values, according to the following
        order: \code{s0, w0, s(k,j), w(k,j)}, for \code{k} and \code{j} set by the factorial
        design. With the default setting \code{NULL}, when the minimization algorithm is L-BFGS-B,
        \code{rav} loads the default parameters:
        \code{s0 = max(range)/10, w0 = 10, s(k,j) = max(range), w = 10} for each \code{k} and \code{j}.
    }
    \item{I0}{
        Logical. If set \code{FALSE}, the \code{s0} and \code{w0} parameters are forced to be zero.
        If set \code{TRUE}, the \code{s0} and \code{w0} parameters are free to be estimated.
    }
    \item{s.fixed}{
        Logical. Default setting to \code{FALSE} indicates that the scale values parameters
        are estimated by the algorithmic procedure. Otherwise, their values are fixedto the
        mean values of the one-way sub-designs (strongly suggested for column data  that shows
        a normal distribution).
    }
    \item{w.fixed}{
        List or logical attribute. Can be used to set fixed values for the weight parameters.
        See the examples for further details. If set \code{TRUE} the procedure opens a pop-up
        that allows to fix the parameters without using a list object.
    }
    \item{IC.diff}{
        Vector containing the cut-off values (of both BIC and AIC indexes) at which different
        models are considered equivalent. Default setting: BIC difference = 2.5, AIC difference = 2.0
        (\code{IC.diff = c(2.5, 2.0)}).
    }
    \item{delta.weights}{
        Numeric attribute that set the cut-off value at which different weights must be considered equal.
    }
    \item{method}{
        The minimization algorithm that has to be used. Options are: "L-BFGS-B", "BFGS", "Nelder-Mead",
        "SANN", "CG". See \code{optim} documentation for further informations.
    }
    \item{control}{
        A list of control parameters. See \code{optim} documentation for further informations.
    }
    \item{title}{
        Character. Puts a title on the output.
    }
    \item{names}{
        Vector of character strings containing the names of the factors.
    }
    \item{verbose}{
        Logical. If set \code{TRUE} the function prints general informations for every step of the
        information criterion procedure.
    }
}

\details{ 
    The \code{rav} function implements the R-Average procedure (Vidotto & Vicentini, 2007),
    for the estimation of the parameters of the Averaging Model. Three different cases are
    tested: EAM (Equal weights Averaging Model), DAM (Differential weights Averaging Model)
    and a model selected by means of an Information Criterion. The routine estimates the scale
    values and the weight parameters by minimizing the residual sum of squares of the model.
    
    The Information Criterion procedure starts from the EAM and, step by step, analyzes 
    different combinations of weights, checking whether a new estimated model is better
    than the previous baseline (to set the criterion for AIC/BIC differences between models
    change the argument \code{IC.diff}). Finally, only the best model between EAM, DAM, and
    IC is shown.
}

\value{
    An object of class \code{"rav"}. The method \code{summary} applied to the \code{rav}
    object prints all the fitted models. The functions \code{fitted}, \code{residuals} and
    \code{parameters} can be used to extract respectively fitted values \code{R}, the matrix
    of residuals and the set of parameter estimated.
}

\author{
    \bold{version 0.0}:\cr
    Marco Vicentini \email{marco.vicentini@gmail.com}
    
    \bold{version 0.1 and following}:\cr
    Stefano Noventa \email{stefano.noventa@unipd.it}\cr
    Davide Massidda \email{davide.massidda@unipd.it}\cr
    
    \bold{Supervisor}: Prof. Giulio Vidotto \email{giulio.vidotto@unipd.it}
    
    University of Padova, Department of General Psychology\cr
    QPLab: Quantitative Psychology Laboratory
    \url{http://qplab.psy.unipd.it/}
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

\seealso{
    \code{\link[rAverage]{rAverage-package}},
    \code{\link[rAverage]{rav.cmd}},
    \code{\link[rAverage]{averaging}},
    \code{\link[rAverage]{pargen}},
    \code{\link[rAverage]{rav.indexes}},
    \code{\link[stats]{optim}}
}

\examples{\dontrun{
# --------------------------------------
# Example 1: 3x3 factorial design
# --------------------------------------
# The first column is filled with a sequence of NA values.
data(fmdata1)
fmdata1
# For a two factors design, the matrix data contains the one-way
# sub-design and the two-ways full factorial design observed data.
# Pay attention to the columns order:
# sub-design: A1, A2, A3, B1, B2, B3
# full factorial: A1B1, A1B2, A1B3, A2B1, A2B2, A2B3, A3B1, A3B2, A3B3
# Start the R-Average procedure:
fm1 <- rav(fmdata1, lev=c(3,3), range=c(0,20))
# (notice that 'range' argument specifies the range of the response scale)
fm1 # print the best model selected
summary(fm1) # print the fitted models

# To insert the factor names:
fact.names <- c("Name of factor A", "Name of factor B")
fm1 <- rav(fmdata1, lev=c(3,3), range=c(0,20), names=fact.names)

# To insert a output title:
out.title <- c("Put your title here")
fm1 <- rav(fmdata1, lev=c(3,3), title=out.title)
# (notice that the range argument is not required, but it is recommended)

# To supervise the information criterion work flow:
fm1 <- rav(fmdata1, lev=c(3,3), range=c(0,20), verbose=TRUE)

# To increase the number of iterations of the minimization routine:
fm1 <- rav(fmdata1, lev=c(3,3), range=c(0,20), control=list(maxit=5000))

# To set a fixed value for weights:
wfix <- list(A=c(NA,0.4,0.4), B=c(NA,NA,NA))
wfix
# Warning: NA specify no-fixed weights
fm1.fix <- rav(fmdata1, lev=c(3,3), range=c(0,20), w.fixed=wfix)
# Otherwise, it's possible to call a GUI:
fm1.fix <- rav(fmdata1, lev=c(3,3), range=c(0,20), w.fixed=TRUE)

# rav can work without sub-designs. If any sub-design is not available,
# the corresponding column must be coded with NA values. For example:
fmdata1[,1:3] <- NA
fmdata1
fmdata1 # the A sub-design is empty
fm1.bis <- rav(fmdata1, lev=c(3,3), range=c(0,20), title="Sub-design A is empty")

# Using a subset of data:
data(pasta)
pasta
# Analyzing "subj.04" only:
fact.names <- c("Price","Packaging")
fm.subj04 <- rav(pasta, subset="subj.04", lev=c(3,3), range=c(0,20), names=fact.names)

# --------------------------------------
# Example 2: 3x5 factorial design
# --------------------------------------
data(fmdata2)
# (Pay attention to the columns order)
fmdata2
fm2 <- rav(fmdata2, lev=c(3,5), range=c(0,20))
# Removing all the one-way sub-design:
fmdata2[,1:8] <- NA
fm2.bis <- rav(fmdata2, lev=c(3,5), range=c(0,20))

# --------------------------------------
# Example 3: 3x2x3 factorial design
# --------------------------------------
data(fmdata3)
# (Pay attention to the columns order)
fm3 <- rav(fmdata3, lev=c(3,2,3), range=c(0,20))
# Removing all the one-way design and the AxC sub-design:
fmdata3[,1:8] <- NA # one-way designs
fmdata3[,15:23] <- NA # AxC design
fm3 <- rav(fmdata3, lev=c(3,2,3), range=c(0,20))
}}

\keyword{misc}
