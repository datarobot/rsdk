#' Convert a function into a single string for DataRobot
#' @param functionToConvert function. The function to convert to a string.
#' @param dputFile character. Optional. A filepath to sink dput into.
Stringify <- function(functionToConvert, dputFile = tempfile()) {
  dput(functionToConvert, file = dputFile)
  charVector <- readLines(dputFile)
  outString <- paste(charVector, collapse = "\n")
  return(outString)
}
