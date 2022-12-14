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
#' @title BinResponse
#'
#' @description BinResponse Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field avgPredicted  numeric Average prediction of the model in the bin. Null if there are no entries in the bin.
#'
#' @field endDate  character ISO-formatted datetime of the end of the bin (exclusive).
#'
#' @field frequency  integer Number of the rows in the bin.
#'
#' @field maxPredicted  numeric Maximum prediction of the model in the bin. Null if there are no entries in the bin.
#'
#' @field startDate  character ISO-formatted datetime of the start of the bin (inclusive).
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
BinResponse <- R6::R6Class(
  "BinResponse",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`avgPredicted` = NULL, `endDate` = NULL, `frequency` = NULL, `maxPredicted` = NULL, `startDate` = NULL) {
      if (!is.null(`avgPredicted`)) {
      }
      if (!is.null(`endDate`)) {
        stopifnot(inherits(`endDate`, "POSIXt"))
      }
      if (!is.null(`frequency`)) {
        stopifnot(is.numeric(`frequency`), length(`frequency`) == 1)
      }
      if (!is.null(`maxPredicted`)) {
      }
      if (!is.null(`startDate`)) {
        stopifnot(inherits(`startDate`, "POSIXt"))
      }
    }
  ),
  public = list(
    `avgPredicted` = NULL,
    `endDate` = NULL,
    `frequency` = NULL,
    `maxPredicted` = NULL,
    `startDate` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param avgPredicted Average prediction of the model in the bin. Null if there are no entries in the bin.
    #' @param endDate ISO-formatted datetime of the end of the bin (exclusive).
    #' @param frequency Number of the rows in the bin.
    #' @param maxPredicted Maximum prediction of the model in the bin. Null if there are no entries in the bin.
    #' @param startDate ISO-formatted datetime of the start of the bin (inclusive).
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`avgPredicted` = NULL, `endDate` = NULL, `frequency` = NULL, `maxPredicted` = NULL, `startDate` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`avgPredicted`, `endDate`, `frequency`, `maxPredicted`, `startDate`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(avgPredicted, endDate, frequency, maxPredicted, startDate)
      }
      self$`avgPredicted` <- `avgPredicted`
      self$`endDate` <- `endDate`
      self$`frequency` <- `frequency`
      self$`maxPredicted` <- `maxPredicted`
      self$`startDate` <- `startDate`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(avgPredicted = self$`avgPredicted`, endDate = self$`endDate`, frequency = self$`frequency`, maxPredicted = self$`maxPredicted`, startDate = self$`startDate`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`avgPredicted`)) {
          sprintf(
            '"avgPredicted":
            %d
                  ',
            self$`avgPredicted`
          )
        },
        if (!is.null(self$`endDate`)) {
          sprintf(
            '"endDate":
            "%s"
                  ',
            format(self$`endDate`, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
          )
        },
        if (!is.null(self$`frequency`)) {
          sprintf(
            '"frequency":
            %d
                  ',
            self$`frequency`
          )
        },
        if (!is.null(self$`maxPredicted`)) {
          sprintf(
            '"maxPredicted":
            %d
                  ',
            self$`maxPredicted`
          )
        },
        if (!is.null(self$`startDate`)) {
          sprintf(
            '"startDate":
            "%s"
                  ',
            format(self$`startDate`, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param BinResponseJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(BinResponseJson, validateParams = FALSE) {
      BinResponseObject <- jsonlite::fromJSON(BinResponseJson)
      self$`avgPredicted` <- BinResponseObject$`avgPredicted`
      self$`endDate` <- ParseRFC3339Timestamp(BinResponseObject$`endDate`)
      self$`frequency` <- BinResponseObject$`frequency`
      self$`maxPredicted` <- BinResponseObject$`maxPredicted`
      self$`startDate` <- ParseRFC3339Timestamp(BinResponseObject$`startDate`)

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
