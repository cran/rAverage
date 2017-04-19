\name{rav.single}
\alias{rav.single}

\title{Single subject analysis with averaging models}

\description{
    Analyzes averaging models for every single subjects in a data matrix and store the estimated
    parameters in a list.
}

\usage{
rav.single(data,...)
}

\arguments{
    \item{data}{
        An object of type \code{data.frame} containing data. Each column represents an experimental
        design of a factorial plan (see the function \code{\link[rAverage]{rav}} for details). Each
        row must contain single-trial responses for each subject. Further, the first column must
        contain labels describing an identification code for subjects.
    }
    \item{...}{
        Further arguments for the \code{rav} function (the argument \code{subset} must not be
        specified).
    }
}

\details{
    The \code{rav.single} function is a wrapper for the \code{rav} function. Using \code{rav},
    \code{rav.sigle} analyzes subjects one at time, specifying time by time a different value for
    \code{subset}.
}

\value{
    A \code{list} object in which each slot contains results of a single subject. The ordering of
    the subjects is the same as in the input data matrix.
}

\seealso{
    \code{\link[rAverage]{rav}},
    \code{\link[rAverage]{rAverage-package}}
}

\examples{\dontrun{
data(pasta)
model <- rav.single(pasta,lev=c(3,3))
model$s41 # extracts the subject 's41'
}}

\keyword{misc}