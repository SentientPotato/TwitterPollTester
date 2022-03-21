#' Conduct statistical tests on a Twitter poll
#'
#' @param tweet_id A character vector of length one giving the ID of the tweet
#'     containing the poll
#' @param sep A character vector of length one giving the separator between
#'     factors in the poll options; the default is ","
#' @param token A character vector of length one giving your Twitter developer
#'     account application's Bearer token; the default is
#'     \code{Sys.getenv("BEARER_TOKEN")}
#' @param show_poll_text Should the text of the tweet (which should include the
#'     poll question text) be displayed? The default is \code{TRUE}
#'
#' @return Returns the result of running \code{\link{contingency_test}()} on
#'     the 2x2 contingency test constructed from the Twitter poll's results
#'
#' @examples
#' \dontrun{
#' test_poll("1500904825739005953", sep = ";")
#' test_poll("1505591329530777603", sep = "/")
#' }
#'
#' @export
test_poll <- function(
    tweet_id,
    sep = ",",
    token = Sys.getenv("BEARER_TOKEN"),
    show_poll_text = TRUE
) {
    ## Construct the Twitter API call
    hdr <- c(Authorization = sprintf('Bearer %s', token))
    URL <- paste0("https://api.twitter.com/2/tweets/", tweet_id)
    opt <- list(poll.fields = "options", expansions = 'attachments.poll_ids')
    ## Retrieve the tweet information from Twitter
    twt <- httr::GET(url = URL, httr::add_headers(.headers = hdr), query = opt)
    dat <- jsonlite::fromJSON(httr::content(twt, as = "text"))
    ## Show the text of the poll?
    if ( show_poll_text ) {
        w <- getOption("width") - 1
        b <- strrep("-", w)
        cat(sprintf("\n%s\n%s\n%s\n\n", b, center("Poll text", w), b))
        txt <- unlist(strsplit(x = dat$data$text, split = "\n"))
        for ( elem in txt ) {
            cat(strwrap(x = elem), sep = "\n")
        }
        cat("\n")
    }
    ## Solve an R CMD check issue related to using non-standard evaluation
    ## (no visible binding for global variable ‘label’)
    label <- NULL; votes <- NULL
    ## Construct a contingency table from the tweet data
    tab <- dat$includes$polls$options[[1]] %>%
        dplyr::select(label, votes) %>%
        dplyr::mutate(factor1 = trimws(gsub(paste0(sep, ".+"), "", label))) %>%
        dplyr::mutate(factor2 = trimws(gsub(paste0(".+", sep), "", label))) %>%
        dplyr::select(-label) %>%
        tidyr::pivot_wider(names_from = "factor2", values_from = "votes") %>%
        tibble::column_to_rownames("factor1")
    return(contingency_test(as.matrix(tab)))
}
