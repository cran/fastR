\name{xplot}
\alias{xplot}
\alias{xplot.default}
\alias{xplot.lm}
\title{Augmented functions}
\usage{
  xplot(x, ...)

  \method{xplot}{default} (...)

  \method{xplot}{lm} (x, which = c(1L:3, 5),
    caption = list("Residuals vs Fitted", "Normal Q-Q", "Scale-Location", "Cook's distance", "Residuals vs Leverage", "Cook's distance vs Leverage"),
    panel.default = if (add.smooth) panel.xyplotsmooth else panel.xyplotpoints,
    sub.caption = NULL, main = "", print.plots = TRUE,
    ask = 1 < length(which) && dev.interactive(),
    type = "p", pch = trellis.par.get("plot.symbol")$pch,
    addline.col = trellis.par.get("add.line")$col,
    line.col = trellis.par.get("plot.line")$col,
    symbol.col = trellis.par.get("plot.symbol")$col,
    lty = trellis.par.get("superpose.line")$lty, ...,
    id.n = 3, labels.id = names(residuals(x)),
    cex.id = 0.7, qqline = TRUE, cook.levels = c(0.5, 1),
    add.smooth = TRUE, label.pos = c("left", "right"),
    cex.caption = 1)
}
\arguments{
  \item{\dots}{arguments passed to other functions.}

  \item{x}{A numeric vector or formula.}

  \item{which}{A numeric vector indicating which plots to
  produce}

  \item{panel.default}{default panel function}

  \item{sub.caption}{secondary caption}

  \item{main,}{to be documented}

  \item{type,}{to be documented}

  \item{pch,}{to be documented}

  \item{lty,}{to be documented}

  \item{qqline,}{to be documented}

  \item{caption,}{to be documented}

  \item{print.plots,}{to be documented}

  \item{ask,}{to be documented}

  \item{addline.col,}{to be documented}

  \item{line.col,}{to be documented}

  \item{symbol.col,}{to be documented}

  \item{id.n,}{to be documented}

  \item{labels.id,}{to be documented}

  \item{cex.id,}{to be documented}

  \item{cook.levels,}{to be documented}

  \item{add.smooth,}{to be documented}

  \item{label.pos,}{to be documented}

  \item{cex.caption,}{to be documented}

  \item{plot,}{to be documented}

  \item{panel}{to be documented}
}
\description{
  These functions all behave similarly to the functions
  with the initial \code{x} removed from their names.
}
\examples{
x <- runif(20)
xplot( lm ( 2*x + 5 + rnorm(20) ~ x ) )
}
\author{
  Randall Pruim
}
\seealso{
  \code{\link{plot}}.
}

