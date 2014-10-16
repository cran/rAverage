\name{outlier.replace}
\alias{outlier.replace}

\title{Outlier detection and substitution}

\description{
    Starting by a previously estimated averaging model, this function detect outliers according
    to a Bonferroni method. The outliers can be substituted with a user-defined value.
}

\usage{
outlier.replace(object, whichModel = NULL, alpha = 0.05, value = NA)
}

\arguments{
    \item{object}{
        An object of class \code{'rav'}, containing the estimated averaging models.
    }
    \item{whichModel}{
        Argument that specifies which of the predicted models has to be compared to the observed data.
        Options are:
        \enumerate{
            \item \code{"null"}: null model
            \item \code{"ESM"}: equal scale values model
            \item \code{"SAM"}: simple averaging model
            \item \code{"EAM"}: equal-weights averaging model
            \item \code{"DAM"}: differential-weight averaging model
            \item \code{"IC"}: information criteria
        }
        As default setting, the (first) best model is used.
    }
    \item{alpha}{
        Critical value for the z-test on residuals.
    }
    \item{value}{
        Argument that can be used to set a replacement for the outliers. If a function is specified, it is applied
        to each column of the final matrix: the resulting value is used to replace outliers detected on the same column.
    }
}

\value{
    A data object in which outliers have been removed or replaced.
}

\seealso{
    \code{\link[rAverage]{rav}},
    \code{\link[rAverage]{rAverage-package}},
}

\examples{
# outlier.replace(model, value=mean)
# outlier.replace(model, whichModel="IC", value=NA)
}

\keyword{misc}