"print.margins.grm" <-
function (x, digits = 2, ...) {
    if (!inherits(x, "margins.grm"))
        stop("Use only with 'margins.grm' objects.\n")    
    combs <- x$combs
    type <- x$type
    cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n", sep = "")
    if (x$type == "two-way") {
        cat("\nFit on the Two-Way Margins\n\n")
        out <- diag(rep("-", x$nitems))
        out[lower.tri(out)] <- format(sapply(x$margins, "[[", "TotalResid"), digits = digits, nsmall = 2)
        out <- t(out)
        out[lower.tri(out)] <- sapply(x$margins, function (x) if(x$TotalResid > x$rule) "***" else "")
        dimnames(out) <- list(x$names, x$names)
        print(noquote(out))
        cat("\n'***' denotes pairs of items with lack-of-fit\n")
    } else {
        cat("\nFit on the Three-Way Margins\n\n")
        out <- data.frame(combinations(x$nitems, 3))
        names(out) <- c("Item i", "Item j", "Item k")
        out$"(O-E)^2/E" <- round(sapply(x$margins, "[[", "TotalResid"), digits = 2)
        out$" " <- sapply(x$margins, function (x) if(x$TotalResid > x$rule) "***" else "")
        print(out)
        cat("\n'***' denotes triplets of items with lack-of-fit\n")
    }
    cat("\n")
    invisible(x)
}

