# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
#' Apply a schema to DataRobot objects (lists, frames).
#'
#' Attention developers! Do not apply this function to DataRobot objects UNLESS you have a specific
#' need to constrain the shape of an object! This function will hide information from the end user
#' and causes heartache when an endpoint is extended with new information, as we have to go in and
#' update the R client schema as well.
#'
#' Generally speaking, assume that the user should receive all data from the API. Assume that the
#' user knows exactly what will be returned from the API.
#'
#' @param inList object. The DataRobot object to apply the schema to.
#' @param schema list. The schema to apply.
ApplySchema <- function(inList, schema) {
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
