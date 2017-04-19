\name{fmdata1}
\alias{fmdata1}
\alias{fmdata2}
\alias{fmdata3}
\docType{data}

\title{rAverage dataset examples}

\description{
    Examples of dataset for R-Average analysis.\cr
    fmdata1: example of a 3x3 design. Original parameters:
    \tabular{ll}{
    s0 = 0.0           \tab w0 = 0.0 \cr
    sA = 12.9 1.5 18.3 \tab wA = 1.4 0.3 0.5 \cr
    sB =  5.2 5.0  2.3 \tab wB = 1.6 1.7 1.7
    }
    fmdata2: example of a 3x5 design. Original parameters:
    \tabular{ll}{
    s0 = 0.0                      \tab w0 = 0.0 \cr
    sA = 19.5 15.2  1.9           \tab wA = 0.9 1.2 0.6 \cr
    sB =  2.0  4.4 16.1  6.1  6.0 \tab wB = 1.1 1.0 1.7 0.6 1.3
    }
    fmdata3: example of a 3x2x3 design. Original parameters:
    \tabular{ll}{
    s0 = 0.0          \tab w0 = 0.0 \cr
    sA = 5.9 5.2 9.8  \tab wA = 0.9 1.1 2.2 \cr
    sB = 14.5 2.0     \tab wB = 0.5 1.9 \cr
    sC = 8.5 1.5 10.7 \tab wC = 0.6 0.7 1.4
    }
}

\usage{
    data(fmdata1)
    data(fmdata2)
    data(fmdata3)
}

\format{A \code{matrix} object.}

\examples{\dontrun{
data(fmdata1)
fm1 <- rav(fmdata1, lev=c(3,3))
data(fmdata2)
fm2 <- rav(fmdata2, lev=c(3,5))
data(fmdata3)
fm3 <- rav(fmdata3, lev=c(3,2,3))
}}

\keyword{datasets}
