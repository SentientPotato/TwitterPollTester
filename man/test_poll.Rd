% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test_poll.R
\name{test_poll}
\alias{test_poll}
\title{Conduct statistical tests on a Twitter poll}
\usage{
test_poll(
  tweet_id,
  sep = ",",
  token = Sys.getenv("BEARER_TOKEN"),
  show_poll_text = TRUE
)
}
\arguments{
\item{tweet_id}{A character vector of length one giving the ID of the tweet
containing the poll}

\item{sep}{A character vector of length one giving the separator between
factors in the poll options; the default is ","}

\item{token}{A character vector of length one giving your Twitter developer
account application's Bearer token; the default is
\code{Sys.getenv("BEARER_TOKEN")}}

\item{show_poll_text}{Should the text of the tweet (which should include the
poll question text) be displayed? The default is \code{TRUE}}
}
\value{
Returns the result of running \code{\link{contingency_test}()} on
the 2x2 contingency test constructed from the Twitter poll's results
}
\description{
Conduct statistical tests on a Twitter poll
}
\examples{
\dontrun{
test_poll("1500904825739005953", sep = ";")
test_poll("1505591329530777603", sep = "/")
}

}
