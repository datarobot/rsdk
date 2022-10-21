#' Get pairwise feature association statistics for a project's informative features
#'
#' @inheritParams GetProject
#' @param associationType character. The type of association, must be either "association"
#'  or "correlation".
#' @param metric character. The specified association metric, must be one of "mutualInfo",
#'  "cramersV", "spearman", "pearson", or "tau".
#' @return A list with two items:
#' \itemize{
#'   \item features data.frame. A data.frame containing the following info for each feature:
#'     \itemize{
#'       \item alphabeticSortIndex integer. A number representing the alphabetical order of this
#'         feature compared to the other features in this dataset.
#'       \item feature character. The name of the feature.
#'       \item importanceSortIndex integer. A number ranking the importance of this feature compared
#'         to the other features in this dataset.
#'       \item strengthSortIndex integer. A number ranking the strength of this feature compared to
#'         the other features in this dataset.
#'     }
#'   \item strengths data.frame. A data.frame of pairwise strength data, with the following info:
#'     \itemize{
#'       \item feature1 character. The name of the first feature.
#'       \item feature2 character. The name of the second feature.
#'       \item statistic numeric. Feature association statistics for `feature1` and `feature2`.
#'     }
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   GetFeatureAssociationMatrix(projectId)
#' }
#' @export
GetFeatureAssociationMatrix <- function(project, associationType, metric) {
  project <- ValidateProject(project)
  routeString <- UrlJoin("projects", project, "featureAssociationMatrix")
  params <- list(metric = metric, type = associationType)
  as.DataRobotFeatureAssociationMatrix(DataRobotGET(routeString, query = params))
}

as.DataRobotFeatureAssociationMatrix <- function(inList) {
  outList <- ApplySchema(inList, c("strengths", "features"))
  outList$strengths <- ApplySchema(outList$strengths, c("feature1", "feature2", "statistic"))
  outList$features <- ApplySchema(outList$features, c("feature", "alphabeticSortIndex",
                                                      "importanceSortIndex", "strengthSortIndex"))
  outList
}


#' Get a sample of the actual values used to measure the association between a pair of features.
#'
#' @inheritParams GetProject
#' @param feature1 character. The name of the first feature of interest.
#' @param feature2 character. The name of the second feature of interest.
#' @return A list with the following info:
#' \itemize{
#'    \item features list. The names of `feature1` and `feature2`.
#'    \item types list. The type of `feature1` and `feature2`. Will be "C" for categorical and
#'       "N" for numeric.
#'    \item values data.frame. The values of the feature associations and the relative frequency
#'      of the data points in the sample.
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   GetFeatureAssociationMatrix(projectId, "SepalWidth", "SepalLength")
#' }
#' @export
GetFeatureAssociationMatrixDetails <- function(project, feature1, feature2) {
  project <- ValidateProject(project)
  routeString <- UrlJoin("projects", project, "featureAssociationMatrixDetails")
  params <- list(feature1 = feature1, feature2 = feature2)
  result <- DataRobotGET(routeString, query = params, simplifyDataFrame = FALSE)
  as.DataRobotFeatureAssociationMatrixDetails(result)
}

as.DataRobotFeatureAssociationMatrixDetails <- function(inList) {
  outList <- ApplySchema(inList, c("features", "types", "values"))
  outList$values <- as.data.frame(outList$values)
  colnames(outList$values) <- c("feature1", "feature2", "relativeFreq")
  outList
}
