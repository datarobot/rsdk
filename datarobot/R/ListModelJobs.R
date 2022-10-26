# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.

as.dataRobotModelJob <- function(inList) {
  idIndex <- which(names(inList) == "id")
  names(inList)[idIndex] <- "modelJobId"
  outList <- inList
}

#' @name ListModelJobs
#' @details Retrieve status of Autopilot modeling jobs that are not complete
#'
#' This function requests information on DataRobot Autopilot modeling
#' tasks that are not complete, for one of three reasons: the task is
#' running and has not yet completed; the task is queued and has not
#' yet been started; or, the task has terminated due to an error.
#'
#' The jobStatus variable specifies which of the three groups of
#' modeling tasks is of interest. Specifically, if jobStatus has the
#' value 'inprogress', the request returns information about modeling
#' tasks that are running but not yet complete; if jobStatus has the
#' value 'queue', the request returns information about modeling tasks
#' that are scheduled to run but have not yet started; if jobStatus
#' has the value 'error', the request returns information about modeling
#' tasks that have terminated due to an error. By default, jobStatus is
#' NULL, which means jobs with status "inprogress" or "queue" are returned,
#' but not those with status "error".
#'
#' @inheritParams GetPredictJobs
#' @return A list of lists with one element for each modeling task
#' in the group being queried; if there are no tasks in the class
#' being queried, an empty list is returned. If the group is not empty,
#' a list is returned with the following nine elements:
#' \itemize{
#'   \item status. Prediction job status; an element of JobStatus, e.g. JobStatus$Queue.
#'   \item processes. List of character vectors describing any preprocessing applied.
#'   \item projectId. Character string giving the unique identifier for the project.
#'   \item modelId character. The unique identifier for the related model.
#'   \item samplePct. Numeric: the percentage of the dataset used for model building.
#'   \item modelType. Character string specifying the model type.
#'   \item modelCategory. Character string: what kind of model this is - 'prime' for DataRobot
#'     Prime models, 'blend' for blender models, and 'model' for other models.
#'   \item featurelistId. Character string: id of the featurelist used in fitting the model.
#'   \item blueprintId. Character string: id of the DataRobot blueprint on which the model is based.
#'   \item modelJobId. Character: id of the job.
#'   \item isBlocked logical. If TRUE, the job is blocked (cannot be executed) until its
#'     dependencies are resolved.
#' }
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' ListModelJobs(projectId)
#' }
#' @export
#' @include models_apiWrapper.R
ListModelJobs

#' @name GetModelJob
#' @details Request information about a single model job
#'
#' @inheritParams DeleteProject
#' @param modelJobId Character string specifying the job id
#' @return list with following elements:
#' \itemize{
#'   \item status character. Model job status; an element of \code{JobStatus}, e.g.
#'     \code{JobStatus$Queue.}
#'   \item processes list. List of character vectors describing any preprocessing applied.
#'   \item projectId character. The unique identifier for the project.
#'   \item modelId character. The unique identifier for the related model.
#'   \item samplePct numeric. The percentage of the dataset used for model building.
#'   \item trainingRowCount. Integer. The number of rows of the project dataset used in training
#'     the model.
#'   \item modelType character. string specifying the model this job builds.
#'   \item modelCategory character. What kind of model this is - \code{prime} for DataRobot Prime
#'     models, /code{blend} for blender models, and /code{model} for other models.
#'   \item featurelistId character. Id of the featurelist used in fitting the model.
#'   \item blueprintId character. Id of the DataRobot blueprint on which the model is based.
#'   \item modelJobId character. Id of the job.
#'   \item isBlocked logical. If TRUE, the job is blocked (cannot be executed) until its
#'     dependencies are resolved.
#' }
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' initialJobs <- ListModelJobs(project)
#' job <- initialJobs[[1]]
#' modelJobId <- job$modelJobId
#' GetModelJob(projectId, modelJobId)
#' }
#' @export
#' @include models_apiWrapper.R
GetModelJob
