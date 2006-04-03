"plot.grm" <-
function (x, type = c("ICC", "IIC"), items = NULL, category = NULL, legend = FALSE, cx = "top", cy = NULL, 
                        ncol = NULL, col = palette(), lty = 1, ...) {
    if (!inherits(x, "grm"))
        stop("Use only with 'grm' objects.\n")
    type <- match.arg(type)
    betas <- x$coefficients
    nitems <- length(betas)
    ncatg <- sapply(betas, length)
    z <- seq(-3.8, 3.8, length = 100)
    cpr <- if (type == "ICC") iprobs(betas, z) else infoprobs(betas, z)
    plot.items <- type == "ICC" || (type == "IIC" & (is.null(items) || all(items > 0)))
    plot.info <- !plot.items
    main <- if (type == "ICC") "Item Response Category Characteristic Curves" else { 
        if (plot.items) "Item Information Curves" else "Test Information Function"
    }
    ylab <- if (type == "ICC") "Probability" else "Information"
    if (is.null(items) && is.null(category)) {
        if (type == "ICC") {
            old.par <- par(ask = TRUE)
            on.exit(par(old.par))
            one.fig <- prod(par("mfcol")) == 1
            for (i in seq(along = cpr)) {
                main. <- if (one.fig) paste("- Item:", names(cpr)[i]) else paste("\nItem:", names(cpr)[i])
                plot(c(-3.8, 3.8), c(0, 1), type = "n", xlab = "Ability", ylab = ylab, 
                    main = paste(main, main.), ...)
                p <- cpr[[i]]
                pos <- round(seq(10, 90, length = ncol(p)))
                col <- rep(col, length.out = ncatg[i])
                lty <- rep(lty, length.out = ncatg[i])
                for (j in 1:ncol(p)) {
                    lines(z, p[, j], lty = lty[j], col = col[j], ...)
                    if (!legend)
                        text(z[pos[j]], p[pos[j], j], adj = c(0, 0), j, col = col[j], ...)
                }
                if (legend) {
                    ncol. <- if (is.null(ncol)) {
                            if (is.factor(x$X[[i]]) && any(nchar(levels(x$X[[i]])) > 11)) 1 else ncatg[i] %/% 2
                        } else
                            ncol
                    lab <- if (is.factor(x$X[[i]])) levels(x$X[[i]]) else 1:ncatg[i]
                    legend(cx, cy, legend = lab, lty = lty, col = col, bty = "n", ncol = ncol., ...)
                }
            }
        } else {
            plot(c(-3.8, 3.8), range(cpr), type = "n", xlab = "Ability", ylab = ylab, 
                    main = main, ...)
            if (plot.items) {
                col <- rep(col, length.out = nitems)
                lty <- rep(lty, length.out = nitems)
                p <- cpr
                pos <- round(seq(10, 90, length = ncol(p)))
                for (i in 1:nitems) {
                    lines(z, p[, i], lty = lty[i], col = col[i], ...)
                    if (!legend)
                        text(z[pos[i]], p[pos[i], i], adj = c(0, 0), i, col = col[i], ...)
                }
                if (legend) {
                    ncol. <- if (is.null(ncol))
                        ncol. <- if (nitems > 8) 2 else 1
                        else ncol
                    legend(cx, cy, legend = colnames(cpr), lty = lty, col = col, bty = "n", ncol = ncol., ...)
                }
            }
        }
    }
    if (!is.null(items) && is.null(category)) {
        if (length(items) > length(cpr))
            stop("'items' must be a vector of maximum length ", length(cpr), ".\n")
        if (length(items) > 1 && any(items < 1 | items > nitems))
            stop(paste("'items' must contain numbers between 1 and ", nitems, ".\n", sep = ""))
        if (type == "ICC") {
            if (length(items) > 1) {
                old.par <- par(ask = TRUE)
                on.exit(par(old.par))
            }
            one.fig <- prod(par("mfcol")) == 1
            for (i in seq(along = items)) {
                main. <- if (one.fig) paste("- Item:", names(cpr)[i]) else paste("\nItem:", names(cpr)[items[i]])        
                plot(c(-3.8, 3.8), c(0, 1), type = "n", xlab = "Ability", ylab = ylab, 
                    main = paste(main, main.), ...)
                p <- cpr[[items[i]]]
                pos <- round(seq(10, 90, length = ncol(p)))
                col <- rep(col, length.out = ncatg[items[i]])
                lty <- rep(lty, length.out = ncatg[items[i]])
                for (j in 1:ncol(p)) {
                    lines(z, p[, j], lty = lty[j], col = col[j], ...)
                    if (!legend)
                        text(z[pos[j]], p[pos[j], j], adj = c(0, 0), j, col = col[j], ...)
                }
                if (legend) {
                    ncol. <- if (is.null(ncol)) {
                            if (is.factor(x$X[[i]]) && any(nchar(levels(x$X[[i]])) > 11)) 1 else ncatg[i] %/% 2
                        } else 
                            ncol
                    lab <- if (is.factor(x$X[[i]])) levels(x$X[[i]]) else 1:ncatg[i]
                    legend(cx, cy, legend = lab, lty = lty, col = col, bty = "n", ncol = ncol., ...)
                }
            }
        } else {
            nitems <- length(items)
            r <- if (plot.items) range(cpr[, items]) else range(rowSums(cpr))
            plot(c(-3.8, 3.8), r, type = "n", xlab = "Ability", ylab = ylab, main = main, ...)
            if (plot.items) {
                col <- rep(col, length.out = nitems)
                lty <- rep(lty, length.out = nitems)
                p <- cpr[, items, drop = FALSE]
                pos <- round(seq(10, 90, length = ncol(p)))
                for (i in seq(along = items)) {
                    lines(z, p[, i], lty = lty[i], col = col[i], ...)
                    if (!legend)
                        text(z[pos[i]], p[pos[i], i], adj = c(0, 0), items[i], col = col[i], ...)
                }
                if (legend) {
                    ncol. <- if (is.null(ncol))
                        ncol. <- if (nitems > 10) 2 else 1
                        else ncol
                    legend(cx, cy, legend = colnames(cpr)[items], lty = lty, col = col, bty = "n", ncol = ncol., ...)
                }
            } else {
                col <- col[1]
                lty <- lty[1]
                p <- rowSums(cpr)
                lines(z, p, lty = lty, col = col, ...)
                if (legend)
                    legend(cx, cy, legend = "Information", lty = lty, col = col, bty = "n", ncol = 1, ...)
            }
        }
    }
    if (is.null(items) && !is.null(category)) {
        if (length(category) > 1)
            stop("'category' must be a number of length 1.\n")
        if (category < 0 || category > max(ncatg))
            stop(paste("'category' must be a number between 1 and ", max(ncatg), ".\n", sep = ""))
        if (any(ind <- category > ncatg)){
            if (sum(ind) > 1)
                warning("Items ", items[ind], " are excluded since they have only ", ncatg[ind], " categories, respectively")
            else
                warning("Item ", items[ind], " is excluded since they have only ", ncatg[ind], " categories")
            items <- items[!ind]
        }
        p <- sapply(cpr[!ind], function (x, category) x[, category], category = category)
        one.fig <- prod(par("mfcol")) == 1
        main. <- if (one.fig) paste("- Category:", category) else paste("\nCategory:", category)
        plot(c(-3.8, 3.8), c(0, 1), type = "n", xlab = "Ability", ylab = "Probability", 
                main = paste("Item Response Category Characteristic Curves", main.), ...)
         pos <- round(seq(10, 90, length = ncol(p)))
         col <- rep(col, length.out = nitems)
         lty <- rep(lty, length.out = nitems)         
         for (j in 1:ncol(p)) {
            lines(z, p[, j], lty = lty[j], col = col[j], ...)
            if (!legend)
                text(z[pos[j]], p[pos[j], j], adj = c(0, 0), names(cpr)[j], col = col[j], ...)
         }
         if (legend) {
            ncol. <- if (is.null(ncol)) {
                        if (any(nchar(names(cpr)) > 11)) 1 else nitems %/% 2
                    } else
                        ncol
            lab <- names(cpr)
            legend(cx, cy, legend = lab, lty = lty, col = col, bty = "n", ncol = ncol., ...)
         }
    }
    if (!is.null(items) && !is.null(category)) {
        if (length(category) > 1)
            stop("'category' must be a number of length 1.\n")
        if (category < 0 || category > max(ncatg))
            stop(paste("'category' must be a number between 1 and ", max(ncatg), ".\n", sep = ""))
        if (length(items) > length(cpr))
            stop("'items' must be a vector of maximum length ", length(cpr), ".\n")
        if (any(items < 0 | items > nitems))
            stop(paste("'items' must contain numbers between 1 and ", nitems, ".\n", sep = ""))
        if (any(ind <- category > ncatg[items])) {
            if (sum(ind) > 1)
                warning("Items ", items[ind], " are excluded since they have only ", nactg[ind], " categories, respectively")
            else
                warning("Item ", items[ind], " is excluded since they have only ", nactg[ind], " categories")
            items <- items[!ind]
        }
        p <- sapply(cpr[items], function (x, category) x[, category], category = category)
        one.fig <- prod(par("mfcol")) == 1
        main. <- if (one.fig) paste("- Category:", category) else paste("\nCategory:", category)
        plot(c(-3.8, 3.8), c(0, 1), type = "n", xlab = "Ability", ylab = "Probability", 
                main = paste("Item Response Category Characteristic Curves", main.), ...)
        pos <- round(seq(10, 90, length = ncol(p)))
        col <- rep(col, length.out = length(items))
        lty <- rep(lty, length.out = length(items))        
        for (j in 1:ncol(p)) {
            lines(z, p[, j], lty = lty[j], col = col[j], ...)
            if (!legend)
                text(z[pos[j]], p[pos[j], j], adj = c(0, 0), colnames(p)[j], col = col[j], ...)
        }
        if (legend) {
            ncol. <- if (is.null(ncol)) {
                        if (any(nchar(colnames(p)) > 11)) 1 else length(items) %/% 2
                    } else 
                        ncol
            lab <- colnames(p)
            legend(cx, cy, legend = lab, lty = lty, col = col, bty = "n", ncol = ncol., ...)
         }
    }
    invisible()
}

