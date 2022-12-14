# Copyright 2021-2022 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.

# Public API
#
# DataRobot's Public facing API
#
# The version of the OpenAPI document: 2.29.0
# Contact: api-maintainer@datarobot.com
# Generated by: https://openapi-generator.tech

#' @docType class
#' @title PairwiseStatisticsResponse
#'
#' @description PairwiseStatisticsResponse Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field data  list( \link{PairwiseStatisticsItem} ) Statistic values.
#'
#' @field featureName  character Feature name.
#'
#' @field projectId  character Project Id.
#'
#' @field statisticType  character Pairwise statistic type.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
PairwiseStatisticsResponse <- R6::R6Class(
  "PairwiseStatisticsResponse",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`data` = NULL, `featureName` = NULL, `projectId` = NULL, `statisticType` = NULL) {
      if (!is.null(`data`)) {
        stopifnot(is.vector(`data`), sapply(`data`, R6::is.R6))
      }
      if (!is.null(`featureName`)) {
        stopifnot(is.character(`featureName`), length(`featureName`) == 1)
      }
      if (!is.null(`projectId`)) {
        stopifnot(is.character(`projectId`), length(`projectId`) == 1)
      }
      if (!is.null(`statisticType`)) {
        stopifnot(is.character(`statisticType`), length(`statisticType`) == 1)
      }
    }
  ),
  public = list(
    `data` = NULL,
    `featureName` = NULL,
    `projectId` = NULL,
    `statisticType` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param data Statistic values.
    #' @param featureName Feature name.
    #' @param projectId Project Id.
    #' @param statisticType Pairwise statistic type.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`data` = NULL, `featureName` = NULL, `projectId` = NULL, `statisticType` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`data`, `featureName`, `projectId`, `statisticType`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(data, featureName, projectId, statisticType)
      }
      self$`data` <- `data`
      self$`featureName` <- `featureName`
      self$`projectId` <- `projectId`
      self$`statisticType` <- `statisticType`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(data = self$`data`, featureName = self$`featureName`, projectId = self$`projectId`, statisticType = self$`statisticType`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`data`)) {
          sprintf(
            '"data":
            [%s]
      ',
            paste(sapply(self$`data`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`featureName`)) {
          sprintf(
            '"featureName":
            "%s"
                  ',
            self$`featureName`
          )
        },
        if (!is.null(self$`projectId`)) {
          sprintf(
            '"projectId":
            "%s"
                  ',
            self$`projectId`
          )
        },
        if (!is.null(self$`statisticType`)) {
          sprintf(
            '"statisticType":
            "%s"
                  ',
            self$`statisticType`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param PairwiseStatisticsResponseJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(PairwiseStatisticsResponseJson, validateParams = FALSE) {
      PairwiseStatisticsResponseObject <- jsonlite::fromJSON(PairwiseStatisticsResponseJson)
      self$`data` <- ApiClient$new()$deserializeObj(PairwiseStatisticsResponseObject$`data`, "array[PairwiseStatisticsItem]", loadNamespace("datarobot.apicore"))
      self$`featureName` <- PairwiseStatisticsResponseObject$`featureName`
      self$`projectId` <- PairwiseStatisticsResponseObject$`projectId`
      self$`statisticType` <- PairwiseStatisticsResponseObject$`statisticType`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
