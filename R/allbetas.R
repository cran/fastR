allbetas <- function(y, x) {

  r <- as.vector( cov(y, x) )
  n <- length(y)
  my <- sum(y) / n
  m <- colMeans(x)
  sx <- colVars(x, center = m)
  be <- r / sx
  a <- my - be * m

  result <- cbind(a, be)

  if ( is.null( colnames(x) ) ) {
    rownames(result) <- paste("X", 1:ncol(x), sep = "" )
  } else {
    rownames(result) <- colnames(x)
  }

  result

}
