#' A wrapper around stubthat::stub that skips the test entirely if
#' stubthat is not installed. Use this to ensure that the package
#' can be republished on CRAN.
#'
#' This should be considered a temporary measure only, and should be
#' replaced with stronger mocks, say via the mockery or webmockr
#' packages.
#'
#' DSX-2196
stub <- function(...) {
  testthat::skip_if_not_installed("stubthat")
  library(stubthat)

  return(stubthat::stub(...))
}
