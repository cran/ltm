"print.descript" <-
function(x, digits = max(3, getOption("digits") - 4), ...){
    cat("\nDescriptive statistics for", x$name, "\n")
    cat("\nProportions of positive/negative responses:\n")
    print(round(x$perc[2:1, ], digits = digits))
    cat("\n\nFrequencies of total scores:\n")
    print(x$items)    
    cat("\n\nPair-wise Associations:\n")
    print(x$pw.ass[seq(1, min(x$n.print, nrow(x$pw.ass))), ])
    cat("\n\n")
    invisible(x)
}
