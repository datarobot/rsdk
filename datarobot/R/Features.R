#' Returns the list of features (i.e., variables) on which a specified model is based
#'
#' This function returns the list of features (typically, response variable
#' and raw covariates) used in building the model specified by model, an S3
#' object of class 'dataRobotModel'.
#'
#' @inheritParams DeleteModel
#' @return A character vector of feature names, with one component for
#' each model feature.
#' @examples
#' \dontrun{
#'   modelId <- "5996f820af07fc605e81ead4"
#'   ListModelFeatures(modelId)
#' }
#' @family feature functions
#' @export
ListModelFeatures <- function(model) {
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "models", modelId, "features")
  featurelist <- DataRobotGET(routeString)
  featurelist$featureNames
}


#' Details about all features for this project
#'
#' @inheritParams DeleteProject
#' @inherit as.dataRobotFeatureInfo return
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   ListFeatureInfo(projectId)
#' }
#' @family feature functions
#' @export
ListFeatureInfo <- function(project) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "features")
  lapply(DataRobotGET(routeString, simplifyDataFrame = FALSE),
         as.dataRobotFeatureInfo)
}


#' Details about a feature
#'
#' @inheritParams DeleteProject
#' @param featureName Name of the feature to retrieve. Note: DataRobot renames some features, so
#' the feature name may not be the one from your original data. You can use ListFeatureInfo to list
#' the features and check the name.
#' @inherit as.dataRobotFeatureInfo return
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   GetFeatureInfo(projectId, "myFeature")
#' }
#' @family feature functions
#' @export
GetFeatureInfo <- function(project, featureName) {
  projectId <- ValidateProject(project)
  featureForUrl <- if (is.character(featureName)) URLencode(enc2utf8(featureName)) else featureName
  routeString <- UrlJoin("projects", projectId, "features", featureForUrl)
  # simplifyDataFrame because feature$keySummary should be a DF
  as.dataRobotFeatureInfo(DataRobotGET(routeString, simplifyDataFrame = TRUE))
}

#' Information on a data feature.
#'
#' @param inList list. See return value below for expected elements.
#' @return A named list which contains:
#' \itemize{
#'   \item id numeric. feature id. Note that throughout the API, features are specified using
#'     their names, not this ID.
#'   \item name character. The name of the feature.
#'   \item featureType character. Feature type: 'Numeric', 'Categorical', etc.
#'   \item importance numeric. numeric measure of the strength of relationship between the
#'     feature and target (independent of any model or other features).
#'   \item lowInformation logical. Whether the feature has too few values to be informative.
#'   \item uniqueCount numeric. The number of unique values in the feature.
#'   \item naCount numeric. The number of missing values in the feature.
#'   \item dateFormat character. The format of the feature if it is date-time feature.
#'   \item projectId character. Character id of the project the feature belongs to.
#'   \item max. The maximum value in the dataset, formatted in the same format as the data.
#'   \item min. The minimum value in the dataset, formatted in the same format as the data.
#'   \item mean. The arithmetic mean of the dataset, formatted in the same format as the data.
#'   \item median. The median of the dataset, formatted in the same format as the data.
#'   \item stdDev. The standard deviation of the dataset, formatted in the same format as the data.
#'   \item timeSeriesEligible logical. Whether this feature can be used as the datetime partition
#'     column in a time series project.
#'   \item timeSeriesEligibilityReason character. Why the feature is ineligible for the
#'     datetime partition column in a time series project, "suitable" when it is eligible.
#'   \item crossSeriesEligible logical. Whether the cross series group by column is
#'     eligible for cross-series modeling. Will be NULL if no cross series group by column
#'     is used.
#'   \item crossSeriesEligibilityReason character. The type of cross series eligibility
#'     (or ineligibility).
#'   \item timeStep numeric. For time-series eligible features, a positive integer determining
#'     the interval at which windows can be specified. If used as the datetime partition column
#'     on a time series project, the feature derivation and forecast windows must start and end
#'     at an integer multiple of this value. NULL for features that are not time series eligible.
#'   \item timeUnit character. For time series eligible features, the time unit covered by a
#'     single time step, e.g. "HOUR", or NULL for features that are not time series eligible.
#'   \item targetLeakage character. Whether a feature is considered to have target leakage or not.
#'     A value of "SKIPPED_DETECTION" indicates that target leakage detection was not run on
#'     the feature.
#'   \item keySummary data.frame. Optional. Descriptive statistics for this feature, iff
#'     it is a summarized categorical feature. This data.frame contains:
#'     \itemize{
#'       \item key. The name of the key.
#'       \item summary. Descriptive statistics for this key, including:
#'       \itemize{
#'         \item max. The maximum value in the dataset.
#'         \item min. The minimum value in the dataset.
#'         \item mean. The arithmetic mean of the dataset.
#'         \item median. The median of the dataset.
#'         \item stdDev. The standard deviation of the dataset.
#'         \item pctRows. The percentage of rows (from the EDA sample) in which this key occurs.
#'       }
#'     }
#' }
#'
#' @family feature functions
as.dataRobotFeatureInfo <- function(inList) {
  elements <- c("id",
                "name",
                "featureType",
                "importance",
                "lowInformation",
                "uniqueCount",
                "naCount",
                "dateFormat",
                "projectId",
                "max",
                "min",
                "mean",
                "median",
                "stdDev",
                "timeSeriesEligible",
                "timeSeriesEligibilityReason",
                "crossSeriesEligible",
                "crossSeriesEligibilityReason",
                "timeStep",
                "timeUnit",
                "targetLeakage",
                "keySummary")
  outList <- ApplySchema(inList, elements)
  outList$keySummary <- as.featureKeySummary(outList$keySummary)
  class(outList) <- "dataRobotFeatureInfo"
  outList
}

as.featureKeySummary <- function(inList) {
  outList <- ApplySchema(inList, c("key", "summary"))
  descriptiveStatisticsElements <- c("min",
                                     "max",
                                     "median",
                                     "mean",
                                     "stdDev",
                                     "pctRows")
  outList$summary <- ApplySchema(outList$summary, descriptiveStatisticsElements)
  outList
}

#' Retrieve a feature from the creation URL
#'
#' If feature creation times out, the error message includes a URL corresponding to the
#' creation task. That URL can be passed to this function (which will return the feature
#' details when finished) to resume waiting for feature creation.
#'
#' @param asyncUrl character. The temporary status URL.
#' @param maxWait integer. Optional. The maximum time to wait (in seconds) for
#'   project creation before aborting.
#' @export
FeatureFromAsyncUrl <- function(asyncUrl, maxWait = 600) {
  timeoutMessage <-
    paste(sprintf("Feature creation did not complete before timeout (%ss).", maxWait),
          "To query its status and (if complete) retrieve the completed feature info, use:\n  ",
          sprintf("%s('%s')", "FeatureFromAsyncUrl", asyncUrl))
  tryCatch(WaitForAsyncReturn(asyncUrl,
                              addUrl = FALSE,
                              maxWait = maxWait,
                             failureStatuses = "ERROR"),
                  AsyncTimeout = function(e) stop(timeoutMessage))
}


#' Retrieve histogram plot data for a specific feature
#'
#' A histogram is a popular way of visual representation of a feature values
#' distribution in a series of bins. For categorical features every bin represents
#' exactly one of feature values plus the number of occurrences of that value.
#' For numeric features every bin represents a range of values (low end inclusive,
#' high end exclusive) plus the total number of occurrences of all values in this range.
#' In addition to that, with every bin for categorical and numeric features there is also
#' included a target feature average for values in that bin (though it can be missing
#' if the feature is deemed uninformative, if the project target has not been selected
#' yet using \code{SetTarget}, or if the project is a multiclass project).
#'
#' @inheritParams GetFeatureInfo
#' @param binLimit integer. Optional. Desired max number of histogram bins. The default is 60.
#' @return list containing:
#'   \itemize{
#'     \item count numeric. The number of values in this bin's range. If a project is using weights,
#'       the value is equal to the sum of weights of all feature values in the bin's range.
#'     \item target numeric. Average of the target feature for values in this bin. It may be NULL
#'       if the feature is deemed uninformative, if the target has not yet been set
#'       (see \code{SetTarget}), or if the project is multiclass.
#'     \item label character. The value of the feature if categorical, otherwise the low end of the
#'       bin range such that the difference between two consecutive bin labels is the length of the
#'       bin.
#'   }
#' @export
GetFeatureHistogram <- function(project, featureName, binLimit = NULL) {
  projectId <- ValidateProject(project)
  featureForUrl <- if (is.character(featureName)) URLencode(enc2utf8(featureName)) else featureName
  routeString <- UrlJoin("projects", projectId, "featureHistograms", featureForUrl)
  query <- list()
  query$binLimit <- binLimit
  as.dataRobotFeatureHistogram(DataRobotGET(routeString,
                                            simplifyDataFrame = FALSE,
                                            query = query)$plot)
}

as.dataRobotFeatureHistogram <- function(inList) {
  outList <- lapply(inList, ApplySchema, schema = c("label", "count", "target"))
  lapply(outList, function(lst) {
                    lapply(lst, function(item) {
                                  if (is.list(item)) { NULL } else { item }
                                }) }) }


# Create derived features
CreateDerivedFeatureFunctionMaker <- function(variableType) {
  featureRequester <- function(project, parentName, name = NULL, dateExtraction = NULL,
                               replacement = NULL, maxWait = 600) {
    projectId <- ValidateProject(project)
    routeString <- UrlJoin("projects", projectId, "typeTransformFeatures")
    if (is.null(name)) name <- sprintf("%s (%s)", parentName, variableType)
    body <- list(name = name, parentName = parentName, variableType = variableType,
                 replacement = replacement, dateExtraction = dateExtraction)
    body <- Filter(Negate(is.null), body)  # Drop NULL values
    creationRequestResponse <- DataRobotPOST(routeString, body = body,
                                             returnRawResponse = TRUE, encode = "json")
    as.dataRobotFeatureInfo(FeatureFromAsyncUrl(
      GetRedirectFromResponse(creationRequestResponse), maxWait = maxWait))
  }
  featureRequester
}

#' @name CreateDerivedFeatures
#' @rdname CreateDerivedFeatures
#'
#' @title Derived Features
#'
#' @description These functions request that new features be created as transformations of existing
#' features and wait for the new feature to be created.
#'
#' @inheritParams DeleteProject
#' @param parentName The name of the parent feature.
#' @param name The name of the new feature.
#' @param dateExtraction dateExtraction: The value to extract from the date column:
#'   'year', 'yearDay', 'month', 'monthDay', 'week', or 'weekDay'. Required for transformation of a
#'   date column. Otherwise must not be provided.
#' @param replacement The replacement in case of a failed transformation. Optional.
#' @param maxWait The maximum time (in seconds) to wait for feature creation.
#'
#' @return Details for the created feature; same schema as the object returned from GetFeatureInfo.
NULL

#' @rdname CreateDerivedFeatures
#' @export
CreateDerivedFeatureAsCategorical <- CreateDerivedFeatureFunctionMaker("categorical")

#' @rdname CreateDerivedFeatures
#' @export
CreateDerivedFeatureAsText <- CreateDerivedFeatureFunctionMaker("text")

#' @rdname CreateDerivedFeatures
#' @export
CreateDerivedFeatureAsNumeric <- CreateDerivedFeatureFunctionMaker("numeric")

#' @rdname CreateDerivedFeatures
#' @export
CreateDerivedFeatureIntAsCategorical <- CreateDerivedFeatureFunctionMaker("categoricalInt")



#' Create new features by transforming the type of an existing ones.
#'
#' Supports feature transformations, including:
#' \itemize{
#'    \item text to categorical
#'    \item text to numeric
#'    \item categorical to text
#'    \item categorical to numeric
#'    \item numeric to categorical
#' }
#' @inheritParams GetProject
#' @param parentNames character. Character vector of variable names to be transformed.
#' @param variableType character. The new type that the columns should be converted to.
#'   See \code{VariableTransformTypes}.
#' @param prefix character. Optional. The string to preface all the transformed features.
#'   Either \code{prefix} or \code{suffix} or both must be provided.
#' @param suffix character. Optional. The string that will be appended at the end to all
#'  the transformed features. Either \code{prefix} or \code{suffix} or both must be provided.
#' @param maxWait integer. Optional. The maximum amount of time (in seconds) to wait for
#'  DataRobot to finish processing the new column before providing a timeout error.
#' @return a list of all the features, after transformation. See \code{GetFeaturelist}
#'  for details.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   BatchFeaturesTypeTransform(projectId,
#'                              parentNames = c("var1", "var2"),
#'                              variableType = VariableTransformTypes$Categorical,
#'                              suffix = "_transformed")
#' }
#' @export
BatchFeaturesTypeTransform <- function(project, parentNames, variableType, prefix = NULL,
                                       suffix = NULL, maxWait = 600) {
  project <- ValidateProject(project)
  if (!is.character(parentNames)) {
    stop(sQuote("parentNames"), " must be a character vector.")
  }
  ValidateParameterIn(variableType, VariableTransformTypes, allowNULL = FALSE)
  payload <- list(parentNames = as.list(parentNames),
                  variableType = variableType)
  if (!is.null(prefix)) { payload$prefix <- prefix }
  if (!is.null(suffix)) { payload$suffix <- suffix }
  routeString <- UrlJoin("projects", project, "batchTypeTransformFeatures")
  postResponse <- DataRobotPOST(routeString,
                                body = lapply(payload, Unbox),
                                returnRawResponse = TRUE,
                                encode = "json")
  WaitForAsyncReturn(GetRedirectFromResponse(postResponse),
                     addUrl = FALSE,
                     maxWait = maxWait,
                     failureStatuses = "ERROR")
  ListFeatureInfo(project)
}
