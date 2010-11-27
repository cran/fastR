\name{mathnoise}
\alias{mathnoise}
\docType{data}
\title{ Test performance and noise -- unfinished documentation}
\description{
In this experiment, hyperactive and control students were given a mathematics test in
either a quiet or loud testing environment.
}
\usage{data(mathnoise)}
\format{
  A data frame with 40 observations on the following 3 variables.
  \describe{
    \item{\code{score}}{score on a mathematics test}
    \item{\code{noise}}{a factor with levels \code{hi} \code{lo}}
    \item{\code{group}}{a factor with levels \code{control} \code{hyper}}
  }
}
\examples{
data(mathnoise)
xyplot(score~noise,mathnoise, group=group, type='a', 
		auto.key=list(columns=2,lines=TRUE,points=FALSE))
}
\keyword{datasets}
