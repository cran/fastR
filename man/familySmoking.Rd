\name{familySmoking}
\alias{familySmoking}
\docType{data}
\title{ Family smoking -- unfinished documentation }
\description{
A cross-tabulation of whether a student smokes and how many of his or her parents smoke.
}
\usage{data(familySmoking)}
\format{
  A data frame with 5375 observations on the following 2 variables.
  \describe{
    \item{\code{Student}}{a factor with levels \code{DoesNotSmoke} \code{Smokes}}
    \item{\code{Parents}}{a factor with levels \code{NeitherSmokes} \code{OneSmokes}}
	\code{BothSmoke} 
  }
}
\examples{
data(familySmoking)
xchisq.test( xtabs(~Parents + Student, familySmoking) )
}
\keyword{datasets}
