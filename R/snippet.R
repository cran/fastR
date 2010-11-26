snippet <-
function (snippet = "snippet", echo = TRUE, execute = TRUE, view = !echo) 
{
    url <- paste("http://www.calvin.edu/~rpruim/fast/R/Rchunk/", 
        snippet, ".Rchunk", sep = "")
    if (view) {
        con <- url(url)
        cat(paste(readLines(con), collapse = "\n"))
        close(con)
    }
    if (execute) {
        source(url, echo = echo)
    }
}
