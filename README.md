
# TwitterPollTester

<!-- badges: start -->
<!-- badges: end -->

Twitter polls allow for four answer options, allowing users to
design a "2x2" poll, where two questions are actually asked, each with two
potential answers. Answers to such a poll can be arranged in a contingency
table, where the number of responses for each option are put in a cell of a
matrix with two columns (one for each potential answer to one question) and
two rows (one for each potential answer to the other question). There are
a number of statistical techniques that can be applied to such data to test
the hypothesis that answers to the two questions are related.
`TwitterPollTester` provides tools to fetch the poll responses from Twitter
and test whether answers to the two questions are independent or related.

## Installation

You can install the development version of `TwitterPollTester` from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("SentientPotato/TwitterPollTester")
```
