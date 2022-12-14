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
#' @title DeploymentFeatureDriftResponse
#'
#' @description DeploymentFeatureDriftResponse Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field count  integer [optional] The number of items returned on this page.
#'
#' @field data  list( \link{DeploymentFeatureDriftObject} ) An array [DriftObject], each in the form described below
#'
#' @field metric  character [optional] Metric used to calculate drift score.
#'
#' @field modelId  character The id of the model for which the features drift is being retrieved.
#'
#' @field next_  character [optional] A URL pointing to the next page (if null, there is no next page)
#'
#' @field period  \link{TimeRange}
#'
#' @field previous  character [optional] A URL pointing to the previous page (if null, there is no previous page)
#'
#' @field segmentAttribute  character [optional] The name of the segment on which segment analysis is being performed.
#'
#' @field segmentValue  character [optional] The value of the &#x60;segmentAttribute&#x60; to segment on.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
DeploymentFeatureDriftResponse <- R6::R6Class(
  "DeploymentFeatureDriftResponse",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`count` = NULL, `data` = NULL, `metric` = NULL, `modelId` = NULL, `next_` = NULL, `period` = NULL, `previous` = NULL, `segmentAttribute` = NULL, `segmentValue` = NULL) {
      if (!is.null(`data`)) {
        stopifnot(is.vector(`data`), sapply(`data`, R6::is.R6))
      }
      if (!is.null(`modelId`)) {
        stopifnot(is.character(`modelId`), length(`modelId`) == 1)
      }
      if (!is.null(`period`)) {
        stopifnot(R6::is.R6(`period`))
      }
      if (!is.null(`count`)) {
        stopifnot(is.numeric(`count`), length(`count`) == 1)
      }
      if (!is.null(`metric`)) {
        stopifnot(is.character(`metric`), length(`metric`) == 1)
      }
      if (!is.null(`next_`)) {
        stopifnot(is.character(`next_`), length(`next_`) == 1)
      }
      if (!is.null(`previous`)) {
        stopifnot(is.character(`previous`), length(`previous`) == 1)
      }
      if (!is.null(`segmentAttribute`)) {
        stopifnot(is.character(`segmentAttribute`), length(`segmentAttribute`) == 1)
      }
      if (!is.null(`segmentValue`)) {
        stopifnot(is.character(`segmentValue`), length(`segmentValue`) == 1)
      }
    }
  ),
  public = list(
    `count` = NULL,
    `data` = NULL,
    `metric` = NULL,
    `modelId` = NULL,
    `next_` = NULL,
    `period` = NULL,
    `previous` = NULL,
    `segmentAttribute` = NULL,
    `segmentValue` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param count The number of items returned on this page.
    #' @param data An array [DriftObject], each in the form described below
    #' @param metric Metric used to calculate drift score.
    #' @param modelId The id of the model for which the features drift is being retrieved.
    #' @param next_ A URL pointing to the next page (if null, there is no next page)
    #' @param period
    #' @param previous A URL pointing to the previous page (if null, there is no previous page)
    #' @param segmentAttribute The name of the segment on which segment analysis is being performed.
    #' @param segmentValue The value of the &#x60;segmentAttribute&#x60; to segment on.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`data` = NULL, `modelId` = NULL, `period` = NULL, `count` = NULL, `metric` = NULL, `next_` = NULL, `previous` = NULL, `segmentAttribute` = NULL, `segmentValue` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`data`, `modelId`, `period`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(count, data, metric, modelId, next_, period, previous, segmentAttribute, segmentValue)
      }
      self$`count` <- `count`
      self$`data` <- `data`
      self$`metric` <- `metric`
      self$`modelId` <- `modelId`
      self$`next_` <- `next_`
      self$`period` <- `period`
      self$`previous` <- `previous`
      self$`segmentAttribute` <- `segmentAttribute`
      self$`segmentValue` <- `segmentValue`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(count = self$`count`, data = self$`data`, metric = self$`metric`, modelId = self$`modelId`, next_ = self$`next_`, period = self$`period`, previous = self$`previous`, segmentAttribute = self$`segmentAttribute`, segmentValue = self$`segmentValue`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`count`)) {
          sprintf(
            '"count":
            %d
                  ',
            self$`count`
          )
        },
        if (!is.null(self$`data`)) {
          sprintf(
            '"data":
            [%s]
      ',
            paste(sapply(self$`data`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`metric`)) {
          sprintf(
            '"metric":
            "%s"
                  ',
            self$`metric`
          )
        },
        if (!is.null(self$`modelId`)) {
          sprintf(
            '"modelId":
            "%s"
                  ',
            self$`modelId`
          )
        },
        if (!is.null(self$`next_`)) {
          sprintf(
            '"next":
            "%s"
                  ',
            self$`next_`
          )
        },
        if (!is.null(self$`period`)) {
          sprintf(
            '"period":
            %s
      ',
            jsonlite::toJSON(self$`period`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`previous`)) {
          sprintf(
            '"previous":
            "%s"
                  ',
            self$`previous`
          )
        },
        if (!is.null(self$`segmentAttribute`)) {
          sprintf(
            '"segmentAttribute":
            "%s"
                  ',
            self$`segmentAttribute`
          )
        },
        if (!is.null(self$`segmentValue`)) {
          sprintf(
            '"segmentValue":
            "%s"
                  ',
            self$`segmentValue`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param DeploymentFeatureDriftResponseJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(DeploymentFeatureDriftResponseJson, validateParams = FALSE) {
      DeploymentFeatureDriftResponseObject <- jsonlite::fromJSON(DeploymentFeatureDriftResponseJson)
      self$`count` <- DeploymentFeatureDriftResponseObject$`count`
      self$`data` <- ApiClient$new()$deserializeObj(DeploymentFeatureDriftResponseObject$`data`, "array[DeploymentFeatureDriftObject]", loadNamespace("datarobot.apicore"))
      self$`metric` <- DeploymentFeatureDriftResponseObject$`metric`
      self$`modelId` <- DeploymentFeatureDriftResponseObject$`modelId`
      self$`next_` <- DeploymentFeatureDriftResponseObject$`next`
      self$`period` <- TimeRange$new()$fromJSON(jsonlite::toJSON(DeploymentFeatureDriftResponseObject$period, auto_unbox = TRUE, digits = NA, null = "null"))
      self$`previous` <- DeploymentFeatureDriftResponseObject$`previous`
      self$`segmentAttribute` <- DeploymentFeatureDriftResponseObject$`segmentAttribute`
      self$`segmentValue` <- DeploymentFeatureDriftResponseObject$`segmentValue`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
