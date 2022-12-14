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
#' @title FeatureDiscoveryRecipeSQLsExport
#'
#' @description FeatureDiscoveryRecipeSQLsExport Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field modelId  character [optional] Model ID to export recipe for
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
FeatureDiscoveryRecipeSQLsExport <- R6::R6Class(
  "FeatureDiscoveryRecipeSQLsExport",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`modelId` = NULL) {
      if (!is.null(`modelId`)) {
        stopifnot(is.character(`modelId`), length(`modelId`) == 1)
      }
    }
  ),
  public = list(
    `modelId` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param modelId Model ID to export recipe for
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`modelId` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(modelId)
      }
      self$`modelId` <- `modelId`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(modelId = self$`modelId`))
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
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param FeatureDiscoveryRecipeSQLsExportJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(FeatureDiscoveryRecipeSQLsExportJson, validateParams = FALSE) {
      FeatureDiscoveryRecipeSQLsExportObject <- jsonlite::fromJSON(FeatureDiscoveryRecipeSQLsExportJson)
      self$`modelId` <- FeatureDiscoveryRecipeSQLsExportObject$`modelId`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
