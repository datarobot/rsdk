# Copyright 2021-2022 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
#' @name RequestPrimeModel
#' @details Request training for a DataRobot Prime model using a specified ruleset
#'
#' Training a model using a ruleset is a necessary prerequisite for being able to download the code
#' for a ruleset.
#'
#' @inheritParams DeleteProject
#' @param ruleset list. A list specifying ruleset parameters (see GetRulesets)
#' @return job Id
#' @examples
#' \dontrun{
#' projectId <- "59a5af20c80891534e3c2bde"
#' modelId <- "5996f820af07fc605e81ead4"
#' rulesets <- GetRulesets(projectId, modelId)
#' ruleset <- rulesets[[1]]
#' RequestPrimeModel(projectId, ruleset)
#' }
#' @export
#' @include models_apiWrapper.R
RequestPrimeModel
