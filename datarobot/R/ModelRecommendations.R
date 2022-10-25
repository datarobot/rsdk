# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.

#' Retrieve a model recommendation from DataRobot for your project.
#'
#' Model recommendations are only generated when you run full Autopilot. One of them
#' (the most accurate individual, non-blender model) will be prepared for deployment.
#' In the preparation process, DataRobot will: (1) calculate feature impact for the selected
#' model and use it to generate a reduced feature list, (2) retrain the selected model on the
#' reduced featurelist, (3) will replace the recommended model with the new model if
#' performance is improved on the reduced featurelist, (4) will retrain the model on a higher
#' sample size, and (5) will replace the recommended model with the higher sample size model if
#' it is more accurate.
#'
#' @inheritParams DeleteProject
#' @param type character. The type of recommendation to retrieve. See
#'   \code{RecommendedModelType} for available options. Defaults to
#'   \code{RecommendedModelType$FastAccurate}.
#' @return A list containing information about the recommended model:
#' \itemize{
#'   \item modelId character. The model ID of the recommended model.
#'   \item projectId character. The project ID of the project the recommendations were made for.
#'   \item recommendationType character. The type of recommendation being made.
#' }
#' @examples
#' \dontrun{
#' projectId <- "5984b4d7100d2b31c1166529"
#' GetModelRecommendation(projectId)
#' }
#' @export
GetModelRecommendation <- function(project, type = RecommendedModelType$FastAccurate) {
  projectId <- ValidateProject(project)
  recs <- ListModelRecommendations(project)
  rec <- Find(function(r) identical(r$recommendationType, type), recs)
  if (length(rec) == 0) {
    stop("A recommendation for type ", type, " was not found.")
  } else {
    rec
  }
}

as.dataRobotModelRecommendation <- function(inList) {
  outList <- inList
  class(outList) <- "dataRobotModelRecommendation"
  outList
}


#' Retrieve the model object that DataRobot recommends for your project.
#'
#' See \code{GetModelRecommendation} for details.
#'
#' @inheritParams DeleteProject
#' @param type character. The type of recommendation to retrieve. See
#'   \code{RecommendedModelType} for available options. Defaults to
#'   \code{RecommendedModelType$FastAccurate}.
#' @return The model object corresponding with that recommendation
#' @examples
#' \dontrun{
#' projectId <- "5984b4d7100d2b31c1166529"
#' GetRecommendedModel(projectId)
#' }
#' @export
GetRecommendedModel <- function(project, type = RecommendedModelType$FastAccurate) {
  projectId <- ValidateProject(project)
  rec <- GetModelRecommendation(project, type = type)
  GetModel(project, rec$modelId)
}
