xqqmath.numeric <-
function (x, data = NULL, ylab = deparse(substitute(x)), ...) 
{
    require(lattice)
    ocall <- sys.call(sys.parent())
    ocall[[1]] <- quote(qqmath)
    ccall <- match.call()
    if (!is.null(ccall$data)) 
        warning("explicit 'data' specification ignored")
    ccall$data <- environment()
    ccall$ylab <- ylab
    ccall$x <- ~x
    ccall[[1]] <- quote(fastR::xqqmath)
    ans <- eval.parent(ccall)
    ans$call <- ocall
    ans
}
