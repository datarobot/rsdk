# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.


#' Convert a blueprint chart into graphviz DOT format
#'
#' @param blueprintChart list. The list returned by \code{GetBlueprintChart} function.
#' @return Character string representation of chart in graphviz DOT language.
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "5996f820af07fc605e81ead4"
#' model <- GetModel(projectId, modelId)
#' blueprintId <- model$blueprintId
#' blueprintChart <- GetBlueprintChart(projectId, blueprintId)
#' BlueprintChartToGraphviz(blueprintChart)
#' }
#' @export
BlueprintChartToGraphviz <- function(blueprintChart) {
  digraph <- 'digraph "Blueprint Chart" {'
  digraph <- paste(digraph, "graph [rankdir=LR]", sep = "\n")
  for (node in blueprintChart$nodes) {
    addLine <- paste(node$id, ' [label="', node$label, '"]', sep = "")
    digraph <- paste(digraph, addLine, sep = "\n")
  }
  for (edgeN in seq(nrow(blueprintChart$edges))) {
    addLine <- paste(blueprintChart$edges[edgeN, 1], " -> ",
      blueprintChart$edges[edgeN, 2],
      sep = ""
    )
    digraph <- paste(digraph, addLine, sep = "\n")
  }
  paste(digraph, "\n}")
}


as.dataRobotBlueprint <- function(inList) {
  outList <- inList
  idIndex <- which(names(outList) == "id")
  names(outList)[idIndex] <- "blueprintId"
  return(outList)
}
