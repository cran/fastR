\name{fumbles}
\alias{fumbles}
\docType{data}
\title{ NCAA football fumbles -- unfinished documentation}
\description{
The number of fumbles in each NCAA IA football game on one weekend.
}
\usage{data(fumbles)}
\format{
  A data frame with 8 observations on the following 4 variables.
  \describe{
    \item{\code{fumbles}}{number of fumbles}
    \item{\code{data.count}}{number of games with this many fumbles}
    \item{\code{data.freq}}{proportion of games with this many fumbles}
    \item{\code{model.freq}}{proportion of games with this many fumbles according
	to a Poisson model}
  }
}
\examples{
data(fumbles)
}
\keyword{datasets}
