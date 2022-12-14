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
#' @title FeatureImpactCreatePayload
#'
#' @description FeatureImpactCreatePayload Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field rowCount  integer [optional] The sample size to use for Feature Impact computation. It is possible to re-compute Feature Impact with a different row count.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
FeatureImpactCreatePayload <- R6::R6Class(
  "FeatureImpactCreatePayload",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`rowCount` = NULL) {
      if (!is.null(`rowCount`)) {
        stopifnot(is.numeric(`rowCount`), length(`rowCount`) == 1)
      }
    }
  ),
  public = list(
    `rowCount` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param rowCount The sample size to use for Feature Impact computation. It is possible to re-compute Feature Impact with a different row count.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`rowCount` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(rowCount)
      }
      self$`rowCount` <- `rowCount`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(rowCount = self$`rowCount`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`rowCount`)) {
          sprintf(
            '"rowCount":
            %d
                  ',
            self$`rowCount`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param FeatureImpactCreatePayloadJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(FeatureImpactCreatePayloadJson, validateParams = FALSE) {
      FeatureImpactCreatePayloadObject <- jsonlite::fromJSON(FeatureImpactCreatePayloadJson)
      self$`rowCount` <- FeatureImpactCreatePayloadObject$`rowCount`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
