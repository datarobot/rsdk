# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
as.DataRobotFeatureAssociationMatrixDetails <- function(inList) {
  outList <- inList
  outList$values <- as.data.frame(outList$values)
  colnames(outList$values) <- c("feature1", "feature2", "relativeFreq")
  outList
}

#' @name GetFeatureAssociationMatrix
#' @details Get pairwise feature association statistics for a project's informative features
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
#' projectId <- "59a5af20c80891534e3c2bde"
#' GetFeatureAssociationMatrix(projectId)
#' }
#' @export
#' @include insights_apiWrapper.R
GetFeatureAssociationMatrix

#' @name GetFeatureAssociationMatrixDetails
#' @details Get a sample of the actual values used to measure the association between a pair of features.
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
#' projectId <- "59a5af20c80891534e3c2bde"
#' GetFeatureAssociationMatrix(projectId, "SepalWidth", "SepalLength")
#' }
#' @export
#' @include insights_apiWrapper.R
GetFeatureAssociationMatrixDetails
