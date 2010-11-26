dotPlot <-
function (x, ..., panel = panel.dotPlot) 
{
    histogram(x, type = "count", panel = panel, ...)
}
