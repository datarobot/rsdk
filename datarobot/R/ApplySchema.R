#' Apply a schema to DataRobot objects (lists, frames)
#' @param inList object. The DataRobot object to apply the schema to.
#' @param schema list. The schema to apply.
#' @param mask list. Search the schema and only apply values that match this with grep.
#'    Defaults to NULL, or no masking.
ApplySchema <- function(inList, schema, mask = NULL) {
  if (!is.null(mask)) {
    schema <- c(schema, names(inList)[grep(mask, names(inList))])
  }
  elements <- names(inList)[names(inList) %in% schema]
  if (is.data.frame(inList)) {
    if (nrow(inList) > 0) {
      outList <- inList[, elements]
    } else {
      outList <- data.frame(matrix(ncol = length(elements), nrow = 0))
      names(outList) <- elements
    }
    outList
  } else {
    inList[elements]
  }
}
