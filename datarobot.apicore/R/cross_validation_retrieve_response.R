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
#' @title CrossValidationRetrieveResponse
#'
#' @description CrossValidationRetrieveResponse Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field cvScores  object A dictionary &#x60;cvScores&#x60; with sub-dictionary keyed by &#x60;partition_id&#x60;, each &#x60;partition_id&#x60; is itself a dictionary keyed by &#x60;metric_name&#x60; where the value is the reading for that particular metric for the partition_id.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
CrossValidationRetrieveResponse <- R6::R6Class(
  "CrossValidationRetrieveResponse",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`cvScores` = NULL) {
      if (!is.null(`cvScores`)) {
      }
    }
  ),
  public = list(
    `cvScores` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param cvScores A dictionary &#x60;cvScores&#x60; with sub-dictionary keyed by &#x60;partition_id&#x60;, each &#x60;partition_id&#x60; is itself a dictionary keyed by &#x60;metric_name&#x60; where the value is the reading for that particular metric for the partition_id.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`cvScores` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`cvScores`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(cvScores)
      }
      self$`cvScores` <- `cvScores`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(cvScores = self$`cvScores`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`cvScores`)) {
          sprintf(
            '"cvScores":
            "%s"
                  ',
            self$`cvScores`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param CrossValidationRetrieveResponseJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(CrossValidationRetrieveResponseJson, validateParams = FALSE) {
      CrossValidationRetrieveResponseObject <- jsonlite::fromJSON(CrossValidationRetrieveResponseJson)
      self$`cvScores` <- CrossValidationRetrieveResponseObject$`cvScores`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
