\name{rav.grid}
\alias{rav.grid}

\title{Generating an empty dataset in \code{rav} format}

\description{
    \code{rav.grid} is a function that generates an empty (\code{NA}s filled) dataset according to the 'rAverage' format.
}

\usage{
rav.grid(lev, trials = 1, subset = FALSE, names = NULL)
}

\arguments{
    \item{lev}{
        Vector containing the number of levels of each factor. For instance, two
        factors with respectively 3 and 4 levels require \code{lev = c(3,4)}.
    }
    \item{trials}{
        Number of rows of the output matrix.
    }
    \item{subset}{
        Logical. Indicates whether the matrix should contain a first column for subset coding.
    }
    \item{names}{
        Character. Indicates the column names (optional).
    }
}

\value{
    A \code{data.frame} object.
}

\seealso{
    \code{\link[rAverage]{rav}},
    \code{\link[rAverage]{pargen}},
    \code{\link[rAverage]{datgen}},
    \code{\link[rAverage]{rAverage-package}}
    }

\examples{
rav.grid(lev=c(3,2,3), trials=5, names=c("Hk","Fa","Mg"))
}

\keyword{misc}