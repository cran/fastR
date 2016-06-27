colVars <- function(x, center = NULL, std = FALSE) {
  ##  x is a matrix
  ##  if you want standard deviations set std = TRUE

  if ( !is.null(center) ) {
    m <- center
  } else {
    m <- colSums(x)
  }

  n <- nrow(x)
  x2 <- colSums(x^2)
  s <- ( x2 - m^2/n ) / (n - 1)

  if ( std == TRUE )  s <- sqrt(s)

  s

}
