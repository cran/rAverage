\name{rav2file}
\alias{rav2file}

\title{Export \code{rav} results}

\description{
    The function exports to a text file the estimated parameters or the model residuals from a call to \code{rav}.
}

\usage{
rav2file(object, what, whichModel = NULL, file = file.choose(), sep = ",", dec = ".")
}

\arguments{
    \item{object}{
        An object analyzed by the function \code{\link[rAverage]{rav}}.
    }
    \item{what}{
        Character string indicating which output should be stored in the file, if raw residuals
        (\code{what = "resid"}) or parameters (\code{what = "param"}).
    }
    \item{whichModel}{
        Argument that specifies from which model values must be extracted.
        Options are:
        \enumerate{
            \item \code{"null"}: null model
            \item \code{"ESM"}: equal scale values model
            \item \code{"SAM"}: simple averaging model
            \item \code{"EAM"}: equal-weights averaging model
            \item \code{"DAM"}: differential-weight averaging model
            \item \code{"IC"}: information criteria
        }
        As default setting, the values of the (first) best model are extracted.
    }
    \item{file}{
        A character string naming the file to write. As default, the function opens a mask to
        choose or build a file interactively.
    }
    \item{sep}{
        Field separator string. Values within each row will be separated by this string.
    }
    \item{dec}{
        String argument used to specify the decimal separator.
    }
}

\seealso{
    \code{\link[rAverage]{rav}},
    \code{\link[rAverage]{rav.single}}
}

\examples{\dontrun{
data(pasta)
model <- rav.single(pasta,lev=c(3,3))
rav2file(model, what="resid", file="PastaResid-1.csv")
rav2file(model, what="resid", file="PastaResid-2.csv", sep=";", dec=",")
}}

\keyword{misc}