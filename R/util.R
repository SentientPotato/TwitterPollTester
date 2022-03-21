## This is the log of the normalizing constant of the Dirichlet distribution
log_multivariate_beta <- function(vec) {
    sum(lgamma(vec)) - lgamma(sum(vec))
}

## This calculates the Bayes Factor for the hypothesis that two two-level
## factors in a contingency table are not independent, using Equation 4.2
## in Gunel and Dickey (1974). Some aspects of this implementation were
## inspired by the implementation in the R package BayesFactor, though some
## design choices differ, and importantly, this version *only* works for 2x2
## tables (whereas the BayesFactor version generalizes to other table sizes),
## allowing some simplifications/optimizations
bayes_factor_contingency <- function(tab) {
    n = sum(tab)
    line1 = -log(1 + n/4) - lgamma(n + 3) + lgamma(3)
    line2 = sum(lgamma(tab + 1)) - log_multivariate_beta(colSums(tab) + 1)
    line3 = -log_multivariate_beta(rowSums(tab) + 1)
    return(exp(line1 + line2 + line3))
}

## These functions format p-values and Bayes factors,
## adding star or apostrophe annotations if requested
fmt_p <- function(p, star = TRUE) {
    ann <- strrep("*", star * sum(p < 0.05, p < 0.01, p <= 0.001))
    return(sprintf("%0.3f%s", p, ann))
}
fmt_BF <- function(BF, apos = TRUE) {
    ann <- strrep("*", apos * sum(BF >= 10, BF >= 30, BF >= 100))
    return(sprintf("%g%s", BF, ann))
}

## This function centers a string within a specified width,
## padding each side with space characters
center <- function(text, width) {
    text_width <- nchar(text)
    pad_base   <- (width - text_width) %/% 2
    left_pad   <- strrep(x = " ", times = pad_base)
    right_pad  <- strrep(x = " ", times = pad_base + (width - text_width) %% 2)
    return(paste0(left_pad, text, right_pad))
}
