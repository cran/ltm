"print.descript" <-
function (x, digits = max(3, getOption("digits") - 4), ...) {
    cat("\nDescriptive statistics for", paste("'", x$name, "'", sep = ""), "data-set\n")
    cat("\nSample:\n", x$sample[1], "items and", x$sample[2], "sample units\n")
    cat("\nProportions for each level of response:\n")
    if (is.list(x$perc))
        print(lapply(x$perc, round, digits = digits))
    else
        print(round(x$perc, digits = digits))
    if (!is.null(x$missin)) {
        cat("\nMissing responses:\n")
        print(round(x$missin, digits = digits))
    }
    cat("\n\nFrequencies of total scores:\n")
    print(x$items)    
    cat("\n\nPair-wise Associations:\n")
    print(x$pw.ass[seq(1, min(x$n.print, nrow(x$pw.ass))), ])
    cat("\n\n")
    invisible(x)
}

