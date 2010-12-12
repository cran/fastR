statTally <-
function (sample, rdata, FUN, direction = 2, stemplot = dim(rdata)[direction] < 
    201, q = c(0.5, 0.9, 0.95, 0.99), ...) 
{
    dstat <- FUN(sample)
    cat("Test Stat function:\n\n")
    print(FUN)
    cat("\n")
    stats <- apply(rdata, direction, FUN)
    cat("\nTest Stat applied to sample data = ")
    cat(signif(dstat, 4))
    cat("\n\n")
    cat("Test Stat applied to random data:\n\n")
    print(quantile(stats, q))
    if (stemplot) {
        stem(stats)
    }
    plot1 <- histogram(~stats, ..., panel = function(x, ...) {
        panel.histogram(x, ...)
        panel.abline(v = dstat, col = trellis.par.get("plot.line")$col, 
            lwd = 3)
    })
    cat("\tOf the random samples")
    cat("\n\t\t", paste(sum(stats < dstat), "(", round(100 * 
        sum(stats < dstat)/length(stats), 2), "% )", "had test stats <", 
        signif(dstat, 4)))
    cat("\n\t\t", paste(sum(stats == dstat), "(", round(100 * 
        sum(stats == dstat)/length(stats), 2), "% )", "had test stats =", 
        signif(dstat, 4)))
    cat("\n\t\t", paste(sum(stats > dstat), "(", round(100 * 
        sum(stats > dstat)/length(stats), 2), "% )", "had test stats >", 
        signif(dstat, 4)))
    cat("\n")
    invisible(plot1)
}
