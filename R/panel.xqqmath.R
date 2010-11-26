panel.xqqmath <-
function (x, qqmathline = TRUE, idline = FALSE, fitline = FALSE, 
    slope = sd(x), intercept = mean(x), overlines = FALSE, ..., 
    lcol = trellis.par.get("add.line")$col, pch = 16, lwd = 2, 
    lty = 2) 
{
    require(lattice)
    lty <- rep(lty, length = 3)
    if (overlines) {
        panel.qqmath(x, ...)
    }
    if (idline) {
        panel.abline(0, 1, col = lcol, lty = lty[3], lwd = lwd)
    }
    if (fitline) {
        panel.abline(intercept, slope, col = lcol, lty = lty[2], 
            lwd = lwd)
    }
    if (qqmathline) {
        panel.qqmathline(x, col = lcol, lty = lty[1], lwd = lwd, 
            ...)
    }
    if (!overlines) {
        panel.qqmath(x, ...)
    }
}
