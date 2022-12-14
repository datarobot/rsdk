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
#' @title PairwiseStatisticsLabelConfiguration
#'
#' @description PairwiseStatisticsLabelConfiguration Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field label  character Label name.
#'
#' @field relevance  integer [optional] Relevance value of the label.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
PairwiseStatisticsLabelConfiguration <- R6::R6Class(
  "PairwiseStatisticsLabelConfiguration",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`label` = NULL, `relevance` = NULL) {
      if (!is.null(`label`)) {
        stopifnot(is.character(`label`), length(`label`) == 1)
      }
      if (!is.null(`relevance`)) {
        stopifnot(is.numeric(`relevance`), length(`relevance`) == 1)
      }
    }
  ),
  public = list(
    `label` = NULL,
    `relevance` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param label Label name.
    #' @param relevance Relevance value of the label.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`label` = NULL, `relevance` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`label`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(label, relevance)
      }
      self$`label` <- `label`
      self$`relevance` <- `relevance`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(label = self$`label`, relevance = self$`relevance`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`label`)) {
          sprintf(
            '"label":
            "%s"
                  ',
            self$`label`
          )
        },
        if (!is.null(self$`relevance`)) {
          sprintf(
            '"relevance":
            %d
                  ',
            self$`relevance`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param PairwiseStatisticsLabelConfigurationJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(PairwiseStatisticsLabelConfigurationJson, validateParams = FALSE) {
      PairwiseStatisticsLabelConfigurationObject <- jsonlite::fromJSON(PairwiseStatisticsLabelConfigurationJson)
      self$`label` <- PairwiseStatisticsLabelConfigurationObject$`label`
      self$`relevance` <- PairwiseStatisticsLabelConfigurationObject$`relevance`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
