\name{rav.cmd}
\alias{rav.cmd}

\title{GUI for rav function}

\description{
    The command \code{rav.cmd()} calls the graphical user interface for R-Average procedure.
}

\usage{rav.cmd(lev=NULL, range=NULL, names=NULL)}

\arguments{
     \item{lev}{
         Optional argument. A vector containing the number of levels of each factor. Example:
         two factors with 3 and 4 levels requires a code like \code{lev=c(3,4)}.
         If the default setting NULL is left, the number of levels and factors will be
         required by the GUI, otherwise that step will be skipped.
        }
     \item{range}{
         Optional argument.A vector containing the range of the responses R.
         If the default setting NULL is left, the range will be
         required by the GUI, otherwise that step will be skipped.
        }
     \item{names}{
         Optional argument. Vector of character strings, containing the names of the factors.
         If the default setting NULL is left, the range will be required by the GUI, otherwise
         that step will be skipped.
        }
    }

\details{The arguments are optional.}

\value{The GUI is called.}

\seealso{
    \code{\link[rAverage]{rAverage-package}},
    \code{\link[rAverage]{rav}}
}

\examples{
# Show the GUI
# rav.cmd()
# Show the GUI interface skipping the information about two factors
# With three levels, range between 0 and 20 and names A and B
# rav.cmd(lev=c(3,3),range=c(0,20),names=c("A","B"))
}

\keyword{misc}
