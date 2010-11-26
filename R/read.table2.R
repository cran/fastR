read.table2 <-
function (file, comment.char = "#", header = TRUE, na.strings = c("NA", 
    "."), ...) 
{
    return(read.table(file, comment.char = comment.char, header = header, 
        na.strings = na.strings, ...))
}
