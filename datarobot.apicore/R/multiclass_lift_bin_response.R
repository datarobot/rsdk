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
#' @title MulticlassLiftBinResponse
#'
#' @description MulticlassLiftBinResponse Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field bins  list( \link{LiftBinResponse} ) The lift chart data for that source, as specified below.
#'
#' @field targetClass  character Target class for the lift chart.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
MulticlassLiftBinResponse <- R6::R6Class(
  "MulticlassLiftBinResponse",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`bins` = NULL, `targetClass` = NULL) {
      if (!is.null(`bins`)) {
        stopifnot(is.vector(`bins`), sapply(`bins`, R6::is.R6))
      }
      if (!is.null(`targetClass`)) {
        stopifnot(is.character(`targetClass`), length(`targetClass`) == 1)
      }
    }
  ),
  public = list(
    `bins` = NULL,
    `targetClass` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param bins The lift chart data for that source, as specified below.
    #' @param targetClass Target class for the lift chart.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`bins` = NULL, `targetClass` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`bins`, `targetClass`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(bins, targetClass)
      }
      self$`bins` <- `bins`
      self$`targetClass` <- `targetClass`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(bins = self$`bins`, targetClass = self$`targetClass`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`bins`)) {
          sprintf(
            '"bins":
            [%s]
      ',
            paste(sapply(self$`bins`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`targetClass`)) {
          sprintf(
            '"targetClass":
            "%s"
                  ',
            self$`targetClass`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param MulticlassLiftBinResponseJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(MulticlassLiftBinResponseJson, validateParams = FALSE) {
      MulticlassLiftBinResponseObject <- jsonlite::fromJSON(MulticlassLiftBinResponseJson)
      self$`bins` <- ApiClient$new()$deserializeObj(MulticlassLiftBinResponseObject$`bins`, "array[LiftBinResponse]", loadNamespace("datarobot.apicore"))
      self$`targetClass` <- MulticlassLiftBinResponseObject$`targetClass`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
