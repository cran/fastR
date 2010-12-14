panel.xhistogram <-
function (x, dcol = trellis.par.get("plot.line")$col, dlwd = 2, 
    gcol = trellis.par.get("superpose.line")$col, glwd = 2, dmath = dnorm, 
    dn = 100, args = NULL, labels = FALSE, density = FALSE, fit = NULL, 
    start = NULL, type = "density", v, h, ...) 
{
    panel.histogram(x, type = type, ...)
    if (labels) {
        h <- hist(x, plot = F, ...)
        if (type == "count") {
            aa <- max(h$counts) * 0.01
            grid.text(label = as.character(round(h$counts, 3)), 
                x = h$mids, y = aa + h$counts, just = c("centre", 
                  "bottom"), default.units = "native")
        }
        else if (type == "percent") {
            ss <- sum(h$counts)
            aa <- max(0.01 * h$counts/ss)
            cat(h$counts)
            cat("\n")
            cat(h$counts/ss)
            cat("\n")
            cat(ss)
            cat("\n")
            print(h)
            cat("\n")
            grid.text(label = as.character(round(h$counts/ss, 
                3)), x = h$mids, y = aa + (h$counts/ss), just = c("centre", 
                "bottom"), default.units = "native")
        }
        else {
            aa <- max(h$density) * 0.01
            grid.text(label = as.character(round(h$density, 3)), 
                x = h$mids, y = aa + h$density, just = c("centre", 
                  "bottom"), default.units = "native")
        }
    }
    if (!is.null(fit)) {
        x = x[!is.na(x)]
        density = T
        if (is.null(args)) {
            if (is.null(start)) {
                args = fitdistr(x, fit)$estimate
            }
            else {
                args = fitdistr(x, fit, start = start)$estimate
            }
        }
        if (tolower(fit) == "exponential") {
            dmath = dexp
        }
        if (tolower(fit) == "normal") {
            dmath = dnorm
        }
        if (tolower(fit) == "lognormal" | tolower(fit) == "log-normal") {
            dmath = dlnorm
        }
        if (tolower(fit) == "poisson") {
            dmath = dpois
        }
        if (tolower(fit) == "gamma") {
            dmath = dgamma
        }
        if (tolower(fit) == "chisq") {
            dmath = dchisq
        }
        if (tolower(fit) == "chi-squared") {
            dmath = dchisq
        }
    }
    if (is.null(args)) {
        args = list(mean = mean(x, na.rm = T), sd = sd(x, na.rm = T))
    }
    if (density) {
        cat("args for density function:\n")
        print(args)
        panel.mathdensity(dmath = dmath, args = args, n = dn, 
            col = dcol, lwd = dlwd)
    }
    if (!missing(v)) {
        for (x in v) {
            panel.abline(v = x, col = gcol, lwd = glwd)
        }
    }
    if (!missing(h)) {
        for (y in h) {
            panel.abline(h = y, col = gcol, lwd = glwd)
        }
    }
}
