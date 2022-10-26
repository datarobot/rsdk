# Copyright 2021-2022 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
#' @name CreatePrimeCode
#' @details Create and validate the downloadable code for the ruleset associated with this model
#'
#' @inheritParams DeleteProject
#' @param primeModelId character. Id returned by GetPrimeModel(s) functions.
#' @param language character. Programming language to use for downloadable code (see PrimeLanguage).
#' @return job Id
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "5996f820af07fc605e81ead4"
#' CreatePrimeCode(projectId, modelId, "Python")
#' }
#' @export
#' @include models_apiWrapper.R
CreatePrimeCode
