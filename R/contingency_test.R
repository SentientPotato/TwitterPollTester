#' Test Independence of Factors in a 2x2 Contingency Table
#'
#' @details
#' There are a number of statistical techniques that may be used to test
#' whether two factors in a contingency table are independent or related.
#' The G-test is a frequentist test of the hypothesis that the factors are
#' related, appropriate when you have at least 1,000 observations; with a
#' small number of observations, Fisher's exact test is more accurate
#' (McDonald 2014, 68-85). For these test, you choose a significance level
#' before running the test, commonly 0.05. The test will return a p-value,
#' and if that value is below the chosen significance level, then you can
#' reject the null hypothesis that the factors are independent of each other.
#'
#' We can use Bayesian analysis and generate a Bayes Factor comparing the
#' hypotheses that the factors are related and that they're independent using
#' the procedure in Gunel and Dickey (1974). This Bayes Factor will tell you
#' how much more likely it is to have observed your data under the hypothesis
#' that the factors are related than under the hypothesis that they are
#' independent. Although there are not strict standards regarding the exact
#' treatment of specific Bayes factor values, generally a Bayes factor of 10
#' or greater is considered strong evidence of a hypothesis
#' (Lee and Wagenmakers 2014, 105).
#'
#' @param tab An integer matrix with two columns and two rows, where the rows
#'     represent a factor with two possible values and the columns represent
#'     another factor with two possible values, and each cell gives the number
#'     of observations that exhibit the factor values represented by that
#'     column and that row
#'
#' @return An object of class "ContingencyTableTests", consisting of a
#'     dataframe with three observations of two variables, "Quantity" and
#'     "Value". The "Quantity" variable names the quantity returned, and its
#'     observations will be "Fisher's Exact Text p-value", "G-test p-value",
#'     and "Bayes Factor". The "Value" variable gives the p-values and Bayes
#'     factor value for those tests. The result object will also have a
#'     "contingency_table" attribute whose value is the "tab" parameter.
#'
#' @references Gunel, Erdogan and James Dickey. 1974. "Bayes Factors for
#'     Independence in Contingency Tables". Biometrika 61(3): 545-557.
#'
#' Lee, Michael D. and Eric-Jan Wagenmakers. 2014. Bayesian Cognitive Modeling:
#'     A Practical Course. Cambridge University Press.
#'
#' McDonald, J.H. 2014. Handbook of Biological Statistics (3rd ed.).
#'     Sparky House Publishing, Baltimore, Maryland.
#'
#' @seealso [print.ContingencyTableTests()]
#'
#' @export
contingency_test <- function(tab) {
    ## Sanity checks
    if ( !is.matrix(tab) ) {
        stop("'tab' must be a matrix.")
    } else if ( !(is.numeric(tab) | is.integer(tab)) ) {
        stop("The entries in 'tab' must be integer or numeric values.")
    } else if ( nrow(tab) != 2 | ncol(tab) != 2 ) {
        stop("'tab' must have two rows and two columns.")
    }
    ## Statistical tests
    bf  <- bayes_factor_contingency(tab)
    fe  <- stats::fisher.test(tab)$p.value
    ex  <- outer(rowSums(tab), colSums(tab)) / sum(tab)
    gt  <- stats::pchisq(q = 2*sum(tab*log(tab/ex)), df = 1, lower.tail = FALSE)
    ## Put results in a dataframe
    nms <-  c("Fisher's Exact Test p-value", "G-test p-value", "Bayes Factor")
    res <- data.frame(Quantity = nms, Value = c(fe, gt, bf))
    ## Create the classed result object with the table as an attribute
    res <- structure(
        .Data = res,
        contingency_table = tab,
        class = c("ContingencyTableTests")
    )
    ## Return the results
    return(res)
}

#' Print the results of contingency_test()
#'
#' @param x A \code{ContingencyTableTests} object
#'     (see \code{\link{contingency_test}})
#' @param ... Arguments passed to or from other methods (currently unused)
#' @param simplify Should values under 0.001 be shown as < 0.001 rather than
#'     its exact value, and should values over 100 be shown as > 100?
#'     Default is \code{TRUE}.
#' @param stars Should stars be appended to p-values that would be treated as
#'     significant at various levels, and apostrophes appended to Bayes Factors
#'     that would be treated as at least strong evidence in favor of the
#'     hypothesis that the factors are related? The default is \code{TRUE}.
#'     (If \code{TRUE}, a legend as to what significance or evidence strength
#'     the glyphs signify will also be displayed).
#' @param show_table Should the contingency table be displayed in addition to
#'     the results of the statistical tests? Default is \code{TRUE}.
#' @param show_BF_guide Should a guide to the strength of evidence provided by
#'     different Bayes Factor values be displayed? Default is \code{FALSE}.
#'
#' @return Returns x invisibly
#'
#' @export
print.ContingencyTableTests <- function(
    x, ...,
    simplify = TRUE,
    stars = TRUE,
    show_table = TRUE,
    show_BF_guide = FALSE
) {
    ## Get console width and define a hrule of that length
    w <- getOption("width") - 1
    b <- strrep("-", w)
    ## Format results for console printing
    if ( simplify ) {
        cmp <- ifelse(x$Value > 100, ">", ifelse(x$Value < 0.001, "<", "="))
        val <- round(pmin(100, pmax(0.001, x$Value)), 3)
    } else {
        cmp <- rep("=", 3)
        val <- x$Value
    }
    val <- c(fmt_p(val[1], stars), fmt_p(val[2], stars), fmt_BF(val[3], stars))
    cat("\n")
    ## Show the contingency table?
    if ( show_table ) {
        cat(sprintf("%s\n%s\n%s\n\n", b, center("Contingency table", w), b))
        detailed_tab <- attr(x, "contingency_table")
        ## We'll add a Total row and column
        detailed_tab <- rbind(detailed_tab, "Total" = colSums(detailed_tab))
        detailed_tab <- cbind(detailed_tab, "Total" = rowSums(detailed_tab))
        print(detailed_tab)
        cat("\n\n")
    }
    ## Display the results of the statistical test
    cat(sprintf("%s\n%s\n%s\n\n", b, center("Test of independence", w), b))
    cat("Null hypothesis (H0): Factors are independent\n")
    cat("Alternative hypothesis (H1): Factors are related\n\n")
    nms <- gsub(" p-value", "", x$Quantity)
    nms <- c(left_pad(nms[1], 25), left_pad(nms[-1], 15))
    qs  <- sprintf("%s %s %s", c("p", "p", "BF"), cmp, val)
    qs  <- c(left_pad(qs[1], 25),  left_pad(qs[-1],  15))
    cat(nms, "\n")
    cat(qs, "\n\n")
    if ( stars ) {
        cat("Significant at alpha level\n")
        cat(paste(c("*", "**", "***"), c(0.05, 0.01, 0.001), " "), "\n\n")
        cat("Evidence the factors are related is\n")
        levs <- c("Strong", "Very strong", "Extreme")
        cat(paste(c("'", "''", "'''"), levs, " "), "\n\n\n")
    }
    ## Show a guide to interpreting Bayes factors?
    if ( show_BF_guide ) {
        txt <- "Guide to interpreting Bayes Factors"
        cat(sprintf("%s\n%s\n%s\n\n", b, center(txt, w), b))
        levs <- c("anecdotal", "moderate", "strong", "very strong", "extreme")
        levs <- c(rev(levs), "considered no", levs)
        hyps <- c(rep(" of H1", 5), "", rep(" of H0", 5))
        cmps <- c(">", rep("in", 4), "=", rep("in", 4), "<")
        vals <- c(100, "(30, 100]", "(10, 30]", "(3, 10]", "(1, 3]", 1)
        vals <- c(vals, "[1/3, 1)", "[1/10, 1/3)", "[1/30, 1/10)")
        vals <- c(vals, "[1/100, 1/30)", "1/100")
        rows <- sprintf("BF %s %s is %s evidence%s.\n", cmps, vals, levs, hyps)
        cat(rows, "\n", sep = "")
    }
    return(invisible(x))
}
