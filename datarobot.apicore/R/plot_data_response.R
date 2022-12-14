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
#' @title PlotDataResponse
#'
#' @description PlotDataResponse Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field actual  numeric The actual value of the target variable for the specified row.
#'
#' @field predicted  numeric The predicted value of the target by the solution for the specified row.
#'
#' @field row  integer The row number from the raw source data. Used as the X axis for the plot when rendered in the web application.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
PlotDataResponse <- R6::R6Class(
  "PlotDataResponse",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`actual` = NULL, `predicted` = NULL, `row` = NULL) {
      if (!is.null(`actual`)) {
      }
      if (!is.null(`predicted`)) {
      }
      if (!is.null(`row`)) {
        stopifnot(is.numeric(`row`), length(`row`) == 1)
      }
    }
  ),
  public = list(
    `actual` = NULL,
    `predicted` = NULL,
    `row` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param actual The actual value of the target variable for the specified row.
    #' @param predicted The predicted value of the target by the solution for the specified row.
    #' @param row The row number from the raw source data. Used as the X axis for the plot when rendered in the web application.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`actual` = NULL, `predicted` = NULL, `row` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`actual`, `predicted`, `row`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(actual, predicted, row)
      }
      self$`actual` <- `actual`
      self$`predicted` <- `predicted`
      self$`row` <- `row`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(actual = self$`actual`, predicted = self$`predicted`, row = self$`row`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`actual`)) {
          sprintf(
            '"actual":
            %d
                  ',
            self$`actual`
          )
        },
        if (!is.null(self$`predicted`)) {
          sprintf(
            '"predicted":
            %d
                  ',
            self$`predicted`
          )
        },
        if (!is.null(self$`row`)) {
          sprintf(
            '"row":
            %d
                  ',
            self$`row`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param PlotDataResponseJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(PlotDataResponseJson, validateParams = FALSE) {
      PlotDataResponseObject <- jsonlite::fromJSON(PlotDataResponseJson)
      self$`actual` <- PlotDataResponseObject$`actual`
      self$`predicted` <- PlotDataResponseObject$`predicted`
      self$`row` <- PlotDataResponseObject$`row`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
