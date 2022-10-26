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

#' @name ListBlueprints
#' @details Retrieve the list of available blueprints for a project
#'
#' This function returns the list of available blueprints
#' for a specified modeling project, as an S3 object of class
#' listOfBlueprints; see Value.
#'
#' @inheritParams DeleteProject
#' @return An S3 object of class 'listOfBlueprints', a list
#' with one element for each recommended blueprint in the
#' associated project. For more information see GetBlueprint()
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' ListBlueprints(projectId)
#' }
#' @export
#' @include blueprints_apiWrapper.R
ListBlueprints


#' @name GetBlueprint
#' @details Retrieve a blueprint
#'
#' @inheritParams DeleteProject
#' @param blueprintId character. Id of blueprint to retrieve.
#' @return List with the following four components:
#' \describe{
#'   \item{projectId}{Character string giving the unique DataRobot project identifier}
#'   \item{processes}{List of character strings, identifying any preprocessing steps included in the
#'   blueprint}
#'   \item{blueprintId}{Character string giving the unique DataRobot blueprint identifier}
#'   \item{modelType}{Character string, specifying the type of model the blueprint builds}
#'   \item{blueprintCategory}{Character string. Describes the category of the blueprint
#'   and the kind of model it produces.}
#' }
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "5996f820af07fc605e81ead4"
#' model <- GetModel(projectId, modelId)
#' blueprintId <- model$blueprintId
#' GetBlueprint(projectId, blueprintId)
#' }
#' @export
#' @include blueprints_apiWrapper.R
GetBlueprint

#' @name GetBlueprintChart
#' @details Retrieve a blueprint chart
#'
#' A Blueprint chart can be used to understand data flow in blueprint.
#'
#' @inheritParams GetBlueprint
#' @return List with the following two components:
#' \itemize{
#'   \item nodes. list each element contains information about one node
#'      of a blueprint : id and label.
#'   \item edges. Two column matrix, identifying blueprint nodes connections.
#' }
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "5996f820af07fc605e81ead4"
#' model <- GetModel(projectId, modelId)
#' blueprintId <- model$blueprintId
#' GetBlueprintChart(projectId, blueprintId)
#' }
#' @export
#' @include blueprints_apiWrapper.R
GetBlueprintChart


#' @name GetModelBlueprintChart
#' @details Retrieve a model blueprint chart
#'
#' A model blueprint is a "pruned down" blueprint representing what was actually run for the model.
#' This is solely the branches of the blueprint that were executed based on the featurelist.
#'
#' @inheritParams GetModel
#' @return List with the following two components:
#' \itemize{
#'   \item nodes. list each element contains information about one node
#'      of a blueprint : id and label.
#'   \item edges. Two column matrix, identifying blueprint nodes connections.
#' }
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "5996f820af07fc605e81ead4"
#' GetModelBlueprintChart(projectId, modelId)
#' }
#' @export
#' @include blueprints_apiWrapper.R
GetModelBlueprintChart

#' @name GetBlueprintDocumentation
#' @details Get documentation for tasks used in the blueprint
#'
#' @inheritParams GetBlueprint
#' @return list with following components
#' \describe{
#'   \item{task}{Character string name of the task described in document}
#'   \item{description}{Character string task description}
#'   \item{title}{Character string title of document}
#'   \item{parameters}{List of parameters that task can received in human-readable
#'   format with following components: name, type, description}
#'   \item{links}{List of external lines used in document with following components: name, url}
#'   \item{references}{List of references used in document with following components: name, url}
#' }
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "5996f820af07fc605e81ead4"
#' model <- GetModel(projectId, modelId)
#' blueprintId <- model$blueprintId
#' GetBlueprintDocumentation(projectId, blueprintId)
#' }
#' @export
#' @include blueprints_apiWrapper.R
GetBlueprintDocumentation


#' @name GetModelBlueprintDocumentation
#' @details Get documentation for tasks used in the model blueprint
#'
#' A model blueprint is a "pruned down" blueprint representing what was actually run for the model.
#' This is solely the branches of the blueprint that were executed based on the featurelist.
#'
#' @inheritParams GetModelBlueprintChart
#' @return list with following components
#' \describe{
#'   \item{task}{Character string name of the task described in document}
#'   \item{description}{Character string task description}
#'   \item{title}{Character string title of document}
#'   \item{parameters}{List of parameters that task can received in human-readable
#'   format with following components: name, type, description}
#'   \item{links}{List of external links used in document with following components: name, url}
#'   \item{references}{List of references used in document with following components: name, url}
#' }
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "5996f820af07fc605e81ead4"
#' GetModelBlueprintDocumentation(projectId, modelId)
#' }
#' @export
#' @include blueprints_apiWrapper.R
GetModelBlueprintDocumentation
