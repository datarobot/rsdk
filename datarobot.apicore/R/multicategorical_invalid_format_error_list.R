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
#' @title MulticategoricalInvalidFormatErrorList
#'
#' @description MulticategoricalInvalidFormatErrorList Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field error  character Error type.
#'
#' @field feature  character Feature name.
#'
#' @field rowData  character Content of the row containing format error.
#'
#' @field rowIndex  integer Row index of the row containing format error.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
MulticategoricalInvalidFormatErrorList <- R6::R6Class(
  "MulticategoricalInvalidFormatErrorList",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`error` = NULL, `feature` = NULL, `rowData` = NULL, `rowIndex` = NULL) {
      if (!is.null(`error`)) {
        stopifnot(is.character(`error`), length(`error`) == 1)
      }
      if (!is.null(`feature`)) {
        stopifnot(is.character(`feature`), length(`feature`) == 1)
      }
      if (!is.null(`rowData`)) {
        stopifnot(is.character(`rowData`), length(`rowData`) == 1)
      }
      if (!is.null(`rowIndex`)) {
        stopifnot(is.numeric(`rowIndex`), length(`rowIndex`) == 1)
      }
    }
  ),
  public = list(
    `error` = NULL,
    `feature` = NULL,
    `rowData` = NULL,
    `rowIndex` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param error Error type.
    #' @param feature Feature name.
    #' @param rowData Content of the row containing format error.
    #' @param rowIndex Row index of the row containing format error.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`error` = NULL, `feature` = NULL, `rowData` = NULL, `rowIndex` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`error`, `feature`, `rowData`, `rowIndex`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(error, feature, rowData, rowIndex)
      }
      self$`error` <- `error`
      self$`feature` <- `feature`
      self$`rowData` <- `rowData`
      self$`rowIndex` <- `rowIndex`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(error = self$`error`, feature = self$`feature`, rowData = self$`rowData`, rowIndex = self$`rowIndex`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`error`)) {
          sprintf(
            '"error":
            "%s"
                  ',
            self$`error`
          )
        },
        if (!is.null(self$`feature`)) {
          sprintf(
            '"feature":
            "%s"
                  ',
            self$`feature`
          )
        },
        if (!is.null(self$`rowData`)) {
          sprintf(
            '"rowData":
            "%s"
                  ',
            self$`rowData`
          )
        },
        if (!is.null(self$`rowIndex`)) {
          sprintf(
            '"rowIndex":
            %d
                  ',
            self$`rowIndex`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param MulticategoricalInvalidFormatErrorListJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(MulticategoricalInvalidFormatErrorListJson, validateParams = FALSE) {
      MulticategoricalInvalidFormatErrorListObject <- jsonlite::fromJSON(MulticategoricalInvalidFormatErrorListJson)
      self$`error` <- MulticategoricalInvalidFormatErrorListObject$`error`
      self$`feature` <- MulticategoricalInvalidFormatErrorListObject$`feature`
      self$`rowData` <- MulticategoricalInvalidFormatErrorListObject$`rowData`
      self$`rowIndex` <- MulticategoricalInvalidFormatErrorListObject$`rowIndex`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
