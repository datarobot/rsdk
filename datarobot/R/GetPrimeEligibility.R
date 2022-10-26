# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.

as.dataRobotPrimeEligibility <- function(inList) {
  outList <- inList
  outList$messageId <- NULL
  outList
}

#' @name GetPrimeEligibility
#' @details Check if model can be approximated with DataRobot Prime
#'
#' @inheritParams DeleteProject
#' @param modelId character. Unique alphanumeric identifier for the model of interest.
#' @return list with two members:
#' \itemize{
#'   \item canMakePrime logical. TRUE if model can be approximated using DataRobot Prime,
#'     FALSE if model can not be approximated.
#'   \item message character. Provides information why model may not be approximated with
#'     DataRobot Prime.
#' }
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "5996f820af07fc605e81ead4"
#' GetPrimeEligibility(projectId, modelId)
#' }
#' @export
#' @include models_apiWrapper.R
GetPrimeEligibility
