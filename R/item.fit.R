`item.fit` <-
function (object, G = 10, FUN = median, simulate.p.value = FALSE, B = 100) {
    if (!class(object) %in% c("ltm", "rasch", "tpm"))
        stop("Use only with 'ltm', 'rasch' or 'tpm' objects.\n")
    if (inherits(object, "ltm") && any(object$ltst$factors > 1, unlist(object$ltst[2:4])))
        stop("currently only the two-parameter logistic model is supported.\n")
    itmFit <- function (X, z, betas, obs) {
        g <- if (is.numeric(G) && length(G) == 1) {
            seq(min(-4, z), max(z, 4), length.out = G + 1)
        } else {
            if (!is.numeric(G) || any(diff(G) < 0))
                stop("'G' must be a numeric vector sorted increasingly.\n")
            G
        }
        groups.ind <- findInterval(z, g, rightmost.closed = TRUE)
        Z <- cbind(1, tapply(z, groups.ind, FUN = FUN))
        pr <- if (inherits(object, "tpm")) {
            cs.mat <- matrix(plogis(betas[, 1]), nrow(Z), p, TRUE)
            cs.mat + (1 - cs.mat) * probs(Z %*% t(betas[, 2:3]))
        } else {
            probs(Z %*% t(betas))
        }
        Obs <- rowsum(obs * X, groups.ind) / rep(tapply(obs, groups.ind, sum), p)
        chi.square <- (Obs - pr)^2 / (pr * (1 - pr))
        colSums(chi.square, na.rm = TRUE)
    }
    n <- nrow(object$X)
    p <- ncol(object$X)
    X <- object$patterns$X
    obs <- object$patterns$obs
    ablts <- factor.scores(object, resp.patterns = X)$score.dat$z1
    betas <- object$coefficients
    parms <- if (inherits(object, "tpm")) cbind(betas[, 2:3], plogis(betas[, 1])) else betas
    Tobs <- itmFit(X, ablts, betas, obs)
    if (!simulate.p.value) {
        df <- switch(class(object), "rasch" = 1, "ltm" = 2, "tpm" = 3)
        pvals <- pchisq(Tobs, df = df, lower.tail = FALSE)
    } else {
        T.boot <- matrix(0, p, B)
        for (b in 1:B) {
            X.new <- rmvlogis(n, parms, IRT = FALSE)
            object.new <- if (class(object) %in% c("rasch", "tpm")) {
                update(object, data = X.new)
            } else {
                update(object, formula = X.new ~ z1)
            }
            parms.new <- object.new$coefficients
            X.new <- object.new$patterns$X
            obs.new <- object.new$patterns$obs
            z1.new <- factor.scores(object.new, resp.patterns = X.new)$score.dat$z1
            T.boot[, b] <- itmFit(X.new, z1.new, parms.new, obs.new)
        }
        pvals <- (rowSums(T.boot >= rep(Tobs, B), na.rm = TRUE) + 1) / (B + 1)
    }
    names(Tobs) <- names(pvals) <- if (is.null(nams <- colnames(object$X))) paste("It", 1:p) else nams
    out <- list(Tobs = Tobs, p.values = pvals, G = G, simulate.p.value = simulate.p.value, B = B, call = object$call)
    class(out) <- "itemFit"
    out
}

