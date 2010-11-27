xqqmath.formula <-
function (x, data = NULL, panel = panel.xqqmath, ...) 
{
    require(lattice)
    qqmath(x, data = data, panel = panel, ...)
}
