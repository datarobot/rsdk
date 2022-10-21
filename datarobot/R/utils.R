Unbox <- function(x) {
  if (length(x) == 1 && !is.list(x)) { jsonlite::unbox(x) }
  else { x }
}


fileToChar <- function(x) {
  readChar(x, file.info(x)$size, useBytes = TRUE)
}


#' Make sure that the object has all of the keys specified. Also tests that there
#' are not additional keys if \code{allowAdditional} is FALSE (default).
#'
#' @param obj object. A list, vector, or data.frame to check names.
#' @param keys character. A vector of names of keys to check.
#' @param allowAdditional logical. Should we allow there to be more keys than specified?
ExpectHasKeys <- function(obj, keys, allowAdditional = FALSE) {
  missingKeys <- setdiff(keys, names(obj))
  testthat::expect_equal(length(missingKeys), 0,
                         info = paste(paste0(missingKeys, collapse = ", "),
                                      " was not found."))
  if (identical(allowAdditional, FALSE)) {
    extraKeys <- setdiff(names(obj), keys)
    testthat::expect_equal(length(extraKeys), 0,
                           info = paste(paste0(extraKeys, collapse = ", "),
                                        " extra keys found."))
  }
}

#' @returns true, if the first list contains the second list (including duplicates)
#' https://stackoverflow.com/a/39350733/914510
Contains <- function(b, s) {
  dupS <- s[duplicated(s)]
  dupB <- b[duplicated(b)]
  lenS <- length(dupS)
  all(s %in% b) && lenS <= length(dupB) &&
    (if (lenS > 0) Contains(dupB, dupS) else 1)
}

nullToNA <- function(x, replacement = NA_real_) {
  # JSON response has a series of nested lists and jsonlite::fromJSON will
  # only convert null values to NA when they are found in a JSON array; not
  # in a list. So, do that here. x must be a list.
  x[sapply(x, is.null)] <- replacement
  x
}

#' Reorder the columns in a data.frame
#'
#' This function reorders columns in a data.frame without relying on dplyr or
#' data.table. You only need to specify the columns that should be moved; all
#' others will be slotted in the gaps. Invalid columns are ignored.
#' @param df data.frame with named columns.
#' @param vars integer. named vector where the names represent column names in
#'   df that should be moved. The value of each item is the new location of
#'   that column.
#' @returns A copy of the input data.frame, with columns rearranged per vars
#' @examples{
#'   df <- data.frame(Time=c(1,2), In=c(2,3), Out=c(3,4), Files=c(4,5))
#'   datarobot:::reorderColumns(df, c("In" = 3, "Time" = 4))
#' }
#'
reorderColumns <- function(df, vars) {
  # This function was directly copied from https://stackoverflow.com/a/37009127/914510,
  # untouched except for minor formatting adjustments.
  # stop if not a data.frame
  stopifnot(is.data.frame(df))

  # sort out inputs
  data.nms <- names(df)
  var.nr <- length(data.nms)
  var.nms <- names(vars)
  var.pos <- vars

  # sanity checks
  stopifnot(!any(duplicated(var.nms)),
            !any(duplicated(var.pos)))
  stopifnot(is.character(var.nms),
            is.numeric(var.pos))
  stopifnot(all(var.nms %in% data.nms))
  stopifnot(all(var.pos > 0),
            all(var.pos <= var.nr))

  # prepare output
  out.vec <- character(var.nr)
  out.vec[var.pos] <- var.nms
  out.vec[-var.pos] <- data.nms[!(data.nms %in% var.nms)]
  stopifnot(length(out.vec) == var.nr)

  # re-arrange vars by position
  df <- df[, out.vec]
  return(df)
}
