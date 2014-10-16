\name{rav}
\alias{rav}
\alias{indices-class}
\alias{rav-class}
\alias{show,rav-method}
\alias{show,indices-method}
\alias{summary,rav-method}

\title{Analyzing the Family of the Averaging Models}

\description{ 
    \code{rav} (\bold{R}-Average for \bold{AV}eraging models) is a procedure for estimating
    the parameters of the averaging models of Information Integration Theory (Anderson, 1981).
    It provides reliable estimations of weights and scale values for a factorial experimental
    design (with any number of factors and levels) by selecting the most suitable subset of
    the parameters, according to the overall goodness of fit indices and to the complexity
    of the design.
}

\usage{
rav( data, subset = NULL, mean = FALSE, lev, s.range = c(NA,NA),
     w.range = exp(c(-5,5)), I0 = FALSE, par.fixed = NULL, all = FALSE,
     IC.diff = c(2,2), Dt = 0.1, IC.break = FALSE, t.par = FALSE,
     verbose = FALSE, title = NULL, names = NULL, method = "BFGS",
     start = c(s=NA,w=exp(0)), lower = NULL, upper = NULL, control = list() )
}

\arguments{
    \item{data}{
        An object of type \code{matrix}, \code{data.frame} or \code{vector} containing the
        experimental data. Each column corresponds to an experimental design of factorial plan
        (in order: one-way design, two-way design, ..., full factorial design; see the example
        for further details). Columns must be sorted combining each level of the first factor with
        all the levels of the following factors. The first column be used to set an
        identification code (ID) to label the subjects (see the attribute \code{subset}).
    }
    \item{subset}{
        Character, numeric or factor attribute that selects a subset of experimental data for the
        analysis (see the examples).
    }
    \item{mean}{
        Logical value wich specifies if the analysis must be performed on raw data (\code{mean = FALSE})
        or on the average of columns of the data matrix (\code{mean = TRUE}).
    }
    \item{lev}{
        Vector containing the number of levels of each factor. For instance, two factors
        with respectively 3 and 4 levels require \code{lev = c(3,4)}.
    }
    \item{s.range,w.range}{
        The range of s and w parameters. Each vector must contains, respectively, the minimum and the
        maximum value. For s-parameters, if the default value \code{NA} is set, the minimum and the maximum
        values of data matrix will be used. For t-parameters, the values exp(-5) and exp(+5) will be used.
        This values will be the bounds for parameters in the estimation process when the minimization algorithm
        is L-BFGS-B.
        The arguments \code{s.range} and \code{w.range} are a simple and quick way to specify the bounds for
        scale and weight parameters. A more complex but complete way is to use the arguments \code{lower}
        and \code{upper}. If \code{lower} and \code{upper} will be specified, \code{s.range} and \code{w.range}
        will be ignored.
    }
    \item{I0}{
        Logical. If set \code{FALSE}, the \code{s0} and \code{w0} parameters are forced to be zero.
        If set \code{TRUE}, the \code{s0} and \code{w0} parameters are free to be estimated.
    }
    \item{par.fixed}{
        This argument allows to constrain one or more parameters to a specified value. Default
        setting to \code{NULL} indicates that all the scale and weight parameters will be estimated by the
        algorithmic procedure. Alternatively, it can be specified the name of the type of parameters to constrain.
        The argument \code{par.fixed = "s"} constrains s-parameters, \code{par.fixed = "w"} constrains
        w-parameters and \code{par.fixed = c("s","w")} constrains both s and w parameters. Also, using \code{"t"} 
		instead of \code{"w"} constrains directly t-parameters. in these cases a graphical interface is
        displayed and the values can be specified.
    }
    \item{all}{
        Logical. If set \code{TRUE} the information criterion tests all the possible combinations of weights
        (see details). The default value \code{FALSE} implies a preselection of a subset of combination based
        on the results of the previous steps of the algorithm.  WARNING: with \code{all = TRUE} the procedure
        is generally more time-consuming (depending on the size of the experimental design), but can provide more
        reliable estimations than the standard procedure.
    }
    \item{IC.diff}{
        Vector containing the cut-off values (of both BIC and AIC indices) at which different
        models are considered equivalent. Default setting: BIC difference = 2.0, AIC difference = 2.0
        (\code{IC.diff = c(2.0, 2.0)}).
    }
    \item{Dt}{
        Numeric attribute that set the cut-off value at which different t-parameters must be considered
        equal (see details).
    }
    \item{IC.break}{
        Logical argument which specifies if to run the Information Criteria Procedure.
    }
    \item{t.par}{
        Logical. Specifies if the output must shows the t-parameters instead of the w-parameters.
    }
    \item{verbose}{
        Logical. If set \code{TRUE} the function prints general informations for every step of the
        information criterion procedure.
    }
    \item{title}{
        Character. Label to use as title for output.
    }
    \item{names}{
        Vector of character strings containing the names of the factors.
    }
    \item{method}{
        The minimization algorithm that has to be used. Options are: "L-BFGS-B", "BFGS", "Nelder-Mead",
        "SANN" and "CG". See \code{optim} documentation for further information.
    }
    \item{start}{ 
        Vector containing the starting values for respectively scale and weight parameters. For the scale
        parameters, if the default value \code{NA} is set, the mean of data is used as starting value.
        For the weight parameters, the starting default value is 1.
    }
    \item{lower}{
        Vector containing the lower values for scale and weight parameters when the minimization
        routine is L-BFGS-B. With the default setting \code{NULL}, s-parameters are set to the
        first value specified in \code{s.range} while w-parameters are set to the first value specified in
        \code{w.range}. Values must be specified in the order: \code{s0,w0,s,w}. For example, for a
        3x3 design, in the \code{lower} vector the positions of parameters must be:
        \code{s0,w0, sA1,sA2,sA3, sB1,sB2,sB3, wA1,wA2,wA3, wB1,wB2,wB3}.
    }
    \item{upper}{
        Vector containing the upper values for scale and weight parameters when the minimization
        routine is L-BFGS-B. With the default setting \code{NULL}, s-parameters are set to the
        second value specified in \code{s.range} while w-parameters are set to the second value specified in
        \code{w.range}. Values must be specified in the order: \code{s0,w0,s,w}. For example, for a
        3x3 design, in the \code{upper} vector the positions of parameters must be:
        \code{s0,w0, sA1,sA2,sA3, sB1,sB2,sB3, wA1,wA2,wA3, wB1,wB2,wB3}.
    }
    \item{control}{
        A list of control parameters. See the \code{optim} documentation for further informations.
        \code{control} argument can be used to change the maximum iteration number of minimization
        routine. To increase the number, use: \code{control=list(maxit=N)}, where \code{N} is the
        number of iterations (100 for default).
    }
}

\details{ 
    The \code{rav} function implements the R-Average method (Vidotto & Vicentini, 2007; Vidotto,
    Massidda & Noventa, 2010), for the parameter estimation of averaging models. R-Average consists
    of several procedures which compute different models with a progressive increasing degree of complexity:
    \enumerate{
        \item Null Model (null):
            identifies a single scale value for all the levels of all factors.
            It assumes constant weights.
        \item Equal scale values model (ESM):
            makes a distinction between the scale values of different factors, estimating a single s-parameter
            for each factor. It assumes constant weights.
        \item Simple averaging model (SAM):
            estimates different scale values between factors and within the levels of each factor. It assumes
            constant weights.
        \item Equal-weight averaging model (EAM):
            differentiates the weighs between factors, but not within the levels of each factor.
        \item Differential-weight averaging model (DAM):
            differentiates the weighs both between factors and within the levels of each factor.
        \item Information criteria (IC):
            the IC procedure starts from the EAM and, step by step, it frees different
            combinations of weights, checking whether a new estimated model is better than the previous baseline.
            The Occam razor, applied by means of the Akaike and Bayesian information criteria, is used in order to
            find a compromise between explanation and parsimony.
    }
    Finally, only the best model is shown.
    
    The R-Average procedures estimates both scale values and weight parameters by minimizing the residual sum of
    squares of the model. The objective function is then the square of the distance between theoretical responses
    and observed responses (Residual Sum of Squares). For a design with \eqn{k} factors with \eqn{i} levels,
    theoretical responses are defined as:
    \deqn{ R = \sum (s_{ki} w_{ki}) / \sum w_{ki} }
    where any weight parameter \eqn{w} is defined as:
    \deqn{w = exp(t)}
    
    Optimization is performed on \eqn{t}-values, and weights are the exponential transformation of
    \eqn{t}. See Vidotto (2011) for details.
}

\value{
    An object of class \code{"rav"}. The method \code{summary} applied to the \code{rav}
    object prints all the fitted models. The functions \code{fitted}, \code{residuals} and
    \code{parameters} can be used to extract respectively fitted values \code{R}, the matrix
    of residuals and the set of parameter estimated.
}

\author{
    \bold{Supervisor}: Prof. Giulio Vidotto \email{giulio.vidotto@unipd.it}
    
    University of Padova, Department of General Psychology\cr
    QPLab: Quantitative Psychology Laboratory
    \url{http://qplab.psy.unipd.it/}
    
    \bold{version 0.0}:\cr
    Marco Vicentini \email{marco.vicentini@gmail.com}
    
    \bold{version 0.1 and following}:\cr
    Stefano Noventa \email{stefano.noventa@univr.it}\cr
    Davide Massidda \email{davide.massidda@unipd.it}\cr
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
    
    Vidotto, G. (2011). Note on differential weight averaging models in Functional
    Measurement. Quality and Quantity, on line first, DOI: 10.1007/s11135-011-9567-1.
 
    Vidotto, G., Massidda, D., & Noventa, S. (2010). Averaging models: parameters
    estimation with the R-Average procedure. \emph{Psicologica}, 31, 461-475.
 
    Vidotto, G. & Vicentini, M. (2007). A general method for parameter
    estimation of averaging models. \emph{Teorie & Modelli}, Vol. 12 (1-2), 211-221.
}

\seealso{
    \code{\link[rAverage]{rAverage-package}},
    \code{\link[rAverage]{rav.single}},
	\code{\link[rAverage]{datgen}},
    \code{\link[rAverage]{pargen}},
    \code{\link[rAverage]{rav.indices}},
	\code{\link[rAverage]{rav2file}},
	\code{\link[rAverage]{outlier.replace}},
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
fm1 <- rav(fmdata1, lev=c(3,3))
# (notice that 'range' argument specifies the range of the response scale)
fm1 # print the best model selected
summary(fm1) # print the fitted models

# To insert the factor names:
fact.names <- c("Name of factor A", "Name of factor B")
fm1 <- rav(fmdata1, lev=c(3,3), names=fact.names)

# To insert a title for the output:
fm1 <- rav(fmdata1, lev=c(3,3), title="Put your title here")

# To supervise the information criterion work flow:
fm1 <- rav(fmdata1, lev=c(3,3), verbose=TRUE)

# To increase the number of iterations of the minimization routine:
fm1 <- rav(fmdata1, lev=c(3,3), control=list(maxit=5000))

# To change the estimation bounds for the scale parameters:
fm1.sMod <- rav(fmdata1, lev=c(3,3), s.range=c(0,20))

# To change the estimation bounds for the weight parameters:
fm1.wMod <- rav(fmdata1, lev=c(3,3), w.range=c(0.01,10))

# To set a fixed value for weights:
fm1.fix <- rav(fmdata1, lev=c(3,3), par.fixed="w")

# rav can work without sub-designs. If any sub-design is not available,
# the corresponding column must be coded with NA values. For example:
fmdata1[,1:3] <- NA
fmdata1
fmdata1 # the A sub-design is empty
fm1.bis <- rav(fmdata1, lev=c(3,3), title="Sub-design A is empty")

# Using a subset of data:
data(pasta)
pasta
# Analyzing "subj.04" only:
fact.names <- c("Price","Packaging")
fm.subj04 <- rav(pasta, subset="subj.04", lev=c(3,3), names=fact.names)

# --------------------------------------
# Example 2: 3x5 factorial design
# --------------------------------------
data(fmdata2)
fmdata2 # (pay attention to the columns order)
fm2 <- rav(fmdata2, lev=c(3,5))
# Removing all the one-way sub-design:
fmdata2[,1:8] <- NA
fm2.bis <- rav(fmdata2, lev=c(3,5))

# --------------------------------------
# Example 3: 3x2x3 factorial design
# --------------------------------------
data(fmdata3) # (pay attention to the columns order)
fm3 <- rav(fmdata3, lev=c(3,2,3))
# Removing all the one-way design and the AxC sub-design:
fmdata3[,1:8] <- NA # one-way designs
fmdata3[,15:23] <- NA # AxC design
fm3 <- rav(fmdata3, lev=c(3,2,3))
}}

\keyword{misc}
