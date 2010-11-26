xhistogram <-
function (..., type = "density", labels = F, density = FALSE, 
    fit = NULL, start = NULL) 
{
    histogram(..., panel = panel.xhistogram, type = type, fit = fit, 
        start = start, labels = labels, density = density)
}
