\name{xtras}
\alias{xchisq.test}
\alias{xhistogram}
\alias{xplot}
\alias{xplot.default}
\alias{xplot.lm}
\alias{xpnorm}
\alias{xqnorm}
\alias{xqqmath}
\alias{panel.xqqmath}
\alias{panel.xhistogram}
\title{
Augmented functions
}
\description{
These functions all behave similarly to the functions with the initial \code{x}
removed from their names.
}
\usage{
xchisq.test(...)

xhistogram(..., type = "density", labels = F, density = FALSE, fit = NULL, start = NULL)

xplot(x, ...)

xplot.default(...)

xplot.lm(x, which = c(1L:3, 5), 
	caption = list("Residuals vs Fitted", "Normal Q-Q", "Scale-Location", "Cook's distance", "Residuals vs Leverage", "Cook's distance vs Leverage"), 
	panel.default = if (add.smooth) panel.xyplotsmooth else panel.xyplotpoints, 
	sub.caption = NULL, main = "", print.plots = TRUE, 
	ask = 1 < length(which) && dev.interactive(), 
	type = "p", pch = trellis.par.get("plot.symbol")$pch, 
	addline.col = trellis.par.get("add.line")$col, 
	line.col = trellis.par.get("plot.line")$col, 
	symbol.col = trellis.par.get("plot.symbol")$col, 
	lty = trellis.par.get("superpose.line")$lty, ..., id.n = 3, 
	labels.id = names(residuals(x)), cex.id = 0.7, 
	qqline = TRUE, cook.levels = c(0.5, 1), add.smooth = TRUE, 
	label.pos = c("left", "right"), cex.caption = 1)

xpnorm(q, mean = 0, sd = 1, plot = TRUE, verbose = TRUE, digits = 5, 
	lower.tail = TRUE, log.p = FALSE, ...)

xqnorm(p, mean = 0, sd = 1, plot = TRUE, verbose = TRUE, digits = 5, 
	lower.tail = TRUE, log.p = FALSE, ...)

xqqmath(x, data=NULL, panel="panel.xqqmath", ...)

panel.xqqmath(x, qqmathline = !(fitline || idline), idline = FALSE, fitline = FALSE, 
    slope = NULL, intercept = NULL, overlines = FALSE, groups=NULL, ...,
    col.line = trellis.par.get("add.line")$col, pch = 16, lwd = 2, 
    lty = 2 ) 
}
\arguments{
  \item{\dots}{ arguments passed to other functions. }

  \item{data}{a data frame.}

  \item{density }{boolean -- should a density curve be overlaid on histogram? }

  \item{x}{ A numeric vector or formula. }

  \item{overlines}{boolean. If TRUE, place lines above the data, otherwise
  lines are below (default).
  }

  \item{idline}{boolean. If TRUE, add the line with slope 1 and intercept 0
  to the plot.
  }

  \item{qqmathline}{boolean.  
  	If TRUE, add a line using \code{panel.qqmathline}
	}

  \item{fitline}{boolean.  If TRUE, add a line to the plot, using
  slope and intercept given by \code{slope} and \code{intercept}.
  }

  \item{qqline}{boolean.  If TRUE, add a line using \code{panel.qqmathline()}. 
  }

  \item{slope}{ slope for \code{fitline}.  If NULL (the default)
  the mean of the data is used, which works well
  for normal-quantile plots but may not be what you want for fitting
  other distributions. }

  \item{intercept}{ intercept for \code{fitline}.  
  If NULL (the default), 
	the standard deviation of the data is used, which works well
	for normal-quantile plots but may not be what you want for fitting
	other distributions. }
\item{col.line}{like \code{col} but applied only to lines}
\item{
q,
p,
groups,
type,
labels,
which,
main,
pch,
lwd,
lty}{as in \code{\link{lattice}} graphics functions}
\item{
caption,
fit,
start,
panel.default,
sub.caption,
print.plots,
ask,
addline.col,
line.col,
symbol.col,
id.n,
labels.id,
cex.id,
cook.levels,
add.smooth,
label.pos,
cex.caption,
mean,
sd,
plot,
verbose,
digits,
lower.tail,
log.p,
panel
}{ to be documented}
}

\author{
Randall Pruim
}

\seealso{
\code{\link{histogram}}, 
\code{\link{chisq.test}}, 
\code{\link{pnorm}}, 
\code{\link{qnorm}}, 
\code{\link{qqmath}}, and
\code{\link{plot}}. 
}
\examples{
# Physicians' Health Study data
phs <- cbind(c(104,189),c(10933,10845)) 
rownames(phs) <- c("aspirin","placebo") 
colnames(phs) <- c("heart attack","no heart attack") 
phs 
xchisq.test(phs) 
}
