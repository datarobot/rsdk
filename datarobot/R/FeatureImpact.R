#' Request Feature Impact to be computed.
#'
#' This adds a Feature Impact job to the project queue.

#' @param model character. The model for which you want to compute Feature Impact, e.g.
#'    from the list of models returned by \code{ListModels(project)}.
#' @return A job ID (character)
#' @examples
#' \dontrun{
#'   model <- ListModels(project)[[1]]
#'   featureImpactJobId <- RequestFeatureImpact(model)
#'   featureImpact <- GetFeatureImpactForJobId(project, featureImpactJobId)
#' }
#' @export
RequestFeatureImpact <- function(model) {
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "models", modelId, "featureImpact")
  rawResponse <- DataRobotPOST(routeString, returnRawResponse = TRUE)
  JobIdFromResponse(rawResponse)
}

FeatureImpactFromResponseList <- function(response) {
  if (!isTRUE(response$ranRedundancyDetection)) {
    warning("Redundancy detection was not run when calculating feature impact.")
  }
  featureImpactDF <- response$featureImpacts
  expectedKeys <- c("impactNormalized", "impactUnnormalized", "featureName", "redundantWith")
  missingKeys <- setdiff(expectedKeys, names(featureImpactDF))
  if (length(missingKeys) > 0) {
    stop(sprintf("Expected keys were missing from Feature Impact data received: %s",
         missingKeys))
  }
  as.dataRobotFeatureImpact(featureImpactDF)
}

#' Retrieve completed Feature Impact results given a model
#'
#' This will only succeed if the Feature Impact computation has completed.
#'
#' Feature Impact is computed for each column by creating new data with that column randomly
#' permuted (but the others left unchanged), and seeing how the error metric score for the
#' predictions is affected. The 'impactUnnormalized' is how much worse the error metric score is
#' when making predictions on this modified data. The 'impactNormalized' is normalized so that the
#' largest value is 1. In both cases, larger values indicate more important features. Elsewhere this
#' technique is sometimes called 'Permutation Importance'.
#'
#' Feature impact also runs redundancy detection, which detects if some features are redundant with
#' higher importance features. Note that some types of projects, like multiclass, do not run
#' redundancy detection. This function will generate a warning if redundancy detection was not run.
#'
#' @param model character. The model for which you want to retrieve Feature Impact.
#' @return
#' A data frame with the following columns:
#' \itemize{
#'   \item featureName character. The name of the feature.
#'   \item impactNormalized numeric. The normalized impact score (largest value is 1).
#'   \item impactUnnormalized numeric. The unnormalized impact score.
#'   \item redundantWith character. A feature that makes this feature redundant, or \code{NA}
#'     if the feature is not redundant.
#'   }
#' @examples
#' \dontrun{
#'   model <- ListModels(project)[[1]]
#'   featureImpactJobId <- RequestFeatureImpact(model)
#'   # Note: This will only work after the feature impact job has completed. Use
#'   #       GetFeatureImpactFromJobId to automatically wait for the job.\
#'   featureImpact <- GetFeatureImpactForModel(model)
#' }
#' @export
GetFeatureImpactForModel <- function(model) {
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "models", modelId, "featureImpact")
  response <- DataRobotGET(routeString)
  FeatureImpactFromResponseList(response)
}

#' Retrieve completed Feature Impact results given a job ID
#'
#' This will wait for the Feature Impact job to be completed (giving an error if the job is not a
#' Feature Impact job and an error if the job errors).
#'
#' @param project character. The project the Feature Impact is part of.
#' @param jobId character. The ID of the job (e.g. as returned from RequestFeatureImpact)
#' @param maxWait integer. The maximum time (in seconds) to wait for the model job to complete
#' @return
#' A data frame with the following columns:
#' \itemize{
#'   \item featureName character. The name of the feature.
#'   \item impactNormalized numeric. The normalized impact score (largest value is 1).
#'   \item impactUnnormalized numeric. The unnormalized impact score.
#'   \item redundantWith character. A feature that makes this feature redundant, or \code{NA}
#'     if the feature is not redundant.
#'   }
#' @examples
#' \dontrun{
#'   model <- ListModels(project)[[1]]
#'   featureImpactJobId <- RequestFeatureImpact(model)
#'   featureImpact <- GetFeatureImpactForJobId(project, featureImpactJobId)
#' }
#' @export
GetFeatureImpactForJobId <- function(project, jobId, maxWait = 600) {
  # Gets generic job, including link to completed resource (as completedUrl) if available.
  # For now, this is just an internal function used to support GetFeatureImpactResults
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "jobs", jobId)
  job <- DataRobotGET(routeString, followLocation = FALSE)
  if (job$jobType != JobType$FeatureImpact) {
    stop(sprintf("Job %s is of type: %s. Can only get Feature Impact for jobs of type: %s.",
                 jobId, job$jobType, JobType$FeatureImpact))
  }
  featureImpactResponse <- WaitForAsyncReturn(routeString, maxWait = maxWait,
                                              failureStatuses = JobFailureStatuses)
  FeatureImpactFromResponseList(featureImpactResponse)
}


#' Get the feature impact for a model, requesting the feature impact if it is not already
#' available.
#'
#' Feature Impact is computed for each column by creating new data with that column randomly
#' permuted (but the others left unchanged), and seeing how the error metric score for the
#' predictions is affected. The 'impactUnnormalized' is how much worse the error metric score is
#' when making predictions on this modified data. The 'impactNormalized' is normalized so that the
#' largest value is 1. In both cases, larger values indicate more important features. Elsewhere this
#' technique is sometimes called 'Permutation Importance'.
#'
#' Note that \code{GetFeatureImpact} will block for the duration of feature impact calculation. If
#' you would prefer not to block the call, use \code{RequestFeatureImpact} to generate an async
#' request for feature impact and then use \code{GetFeatureImpactForModel} or
#' \code{GetFeatureImpactForJobId} to get the feature impact when it has been calculated.
#' \code{GetFeatureImpactForJobId} will also block until the request is complete, whereas
#' \code{GetFeatureImpactForModel} will error if the job is not complete yet.
#'
#' @inheritParams RequestFeatureImpact
#' @export
GetFeatureImpact <- function(model) {
  tryCatch({
    featureImpactJobId <- RequestFeatureImpact(model)
    GetFeatureImpactForJobId(model$projectId, featureImpactJobId)
  }, error = function(e) { # If error in calculating feature impact...
    GetFeatureImpactForModel(model)
  })
}


as.dataRobotFeatureImpact <- function(inList) {
  elements <- c("featureName",
                "name",
                "impactNormalized",
                "impactUnnormalized",
                "redundantWith")
  ApplySchema(inList, elements)
}
