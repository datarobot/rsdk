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
#' @title FrozenModelCreate
#'
#' @description FrozenModelCreate Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field modelId  character the ID of an existing model to use as a source of training parameters.
#'
#' @field nClusters  integer [optional] The number of clusters to use in the specified unsupervised clustering model. Only valid in unsupervised clustering projects.
#'
#' @field samplePct  numeric [optional] the percentage of the dataset to use with the model. Only one of &#x60;samplePct&#x60; and &#x60;trainingRowCount&#x60; should be specified. The specified percentage should be between 0.0 and 100.0.
#'
#' @field trainingRowCount  integer [optional] the integer number of rows of the dataset to use with the model. Only one of &#x60;samplePct&#x60; and &#x60;trainingRowCount&#x60; should be specified.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
FrozenModelCreate <- R6::R6Class(
  "FrozenModelCreate",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`modelId` = NULL, `nClusters` = NULL, `samplePct` = NULL, `trainingRowCount` = NULL) {
      if (!is.null(`modelId`)) {
        stopifnot(is.character(`modelId`), length(`modelId`) == 1)
      }
      if (!is.null(`nClusters`)) {
        stopifnot(is.numeric(`nClusters`), length(`nClusters`) == 1)
      }
      if (!is.null(`samplePct`)) {
        stopifnot(is.numeric(`samplePct`), length(`samplePct`) == 1)
      }
      if (!is.null(`trainingRowCount`)) {
        stopifnot(is.numeric(`trainingRowCount`), length(`trainingRowCount`) == 1)
      }
    }
  ),
  public = list(
    `modelId` = NULL,
    `nClusters` = NULL,
    `samplePct` = NULL,
    `trainingRowCount` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param modelId the ID of an existing model to use as a source of training parameters.
    #' @param nClusters The number of clusters to use in the specified unsupervised clustering model. Only valid in unsupervised clustering projects.
    #' @param samplePct the percentage of the dataset to use with the model. Only one of &#x60;samplePct&#x60; and &#x60;trainingRowCount&#x60; should be specified. The specified percentage should be between 0.0 and 100.0.
    #' @param trainingRowCount the integer number of rows of the dataset to use with the model. Only one of &#x60;samplePct&#x60; and &#x60;trainingRowCount&#x60; should be specified.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`modelId` = NULL, `nClusters` = NULL, `samplePct` = NULL, `trainingRowCount` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`modelId`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(modelId, nClusters, samplePct, trainingRowCount)
      }
      self$`modelId` <- `modelId`
      self$`nClusters` <- `nClusters`
      self$`samplePct` <- `samplePct`
      self$`trainingRowCount` <- `trainingRowCount`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(modelId = self$`modelId`, nClusters = self$`nClusters`, samplePct = self$`samplePct`, trainingRowCount = self$`trainingRowCount`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`modelId`)) {
          sprintf(
            '"modelId":
            "%s"
                  ',
            self$`modelId`
          )
        },
        if (!is.null(self$`nClusters`)) {
          sprintf(
            '"nClusters":
            %d
                  ',
            self$`nClusters`
          )
        },
        if (!is.null(self$`samplePct`)) {
          sprintf(
            '"samplePct":
            %d
                  ',
            self$`samplePct`
          )
        },
        if (!is.null(self$`trainingRowCount`)) {
          sprintf(
            '"trainingRowCount":
            %d
                  ',
            self$`trainingRowCount`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param FrozenModelCreateJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(FrozenModelCreateJson, validateParams = FALSE) {
      FrozenModelCreateObject <- jsonlite::fromJSON(FrozenModelCreateJson)
      self$`modelId` <- FrozenModelCreateObject$`modelId`
      self$`nClusters` <- FrozenModelCreateObject$`nClusters`
      self$`samplePct` <- FrozenModelCreateObject$`samplePct`
      self$`trainingRowCount` <- FrozenModelCreateObject$`trainingRowCount`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
