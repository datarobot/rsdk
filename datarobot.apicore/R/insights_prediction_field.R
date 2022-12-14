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
#' @title InsightsPredictionField
#'
#' @description InsightsPredictionField Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field labels  list( character ) List of predicted label names corresponding to values
#'
#' @field values  list( \link{OneOfnumberarray} ) Predicted value or probability of the class identified by the label
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
InsightsPredictionField <- R6::R6Class(
  "InsightsPredictionField",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`labels` = NULL, `values` = NULL) {
      if (!is.null(`labels`)) {
        stopifnot(is.vector(`labels`), sapply(`labels`, is.character))
      }
      if (!is.null(`values`)) {
        .setPrimitiveProperty(typeList = list("numeric", "array"), propertyData = values)
      }
    }
  ),
  public = list(
    `labels` = NULL,
    `values` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param labels List of predicted label names corresponding to values
    #' @param values Predicted value or probability of the class identified by the label
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`labels` = NULL, `values` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`labels`, `values`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(labels, values)
      }
      self$`labels` <- `labels`
      self$`values` <- sapply(`values`, function(item) .setPrimitiveProperty(typeList = list("numeric", "array"), propertyData = item))
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(labels = self$`labels`, values = self$`values`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`labels`)) {
          sprintf(
            '"labels":
            [%s]
                  ',
            paste(unlist(lapply(self$`labels`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`values`)) {
          sprintf(
            '"values":
            [%s]
      ',
            paste(sapply(self$`values`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param InsightsPredictionFieldJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(InsightsPredictionFieldJson, validateParams = FALSE) {
      InsightsPredictionFieldObject <- jsonlite::fromJSON(InsightsPredictionFieldJson)
      self$`labels` <- ApiClient$new()$deserializeObj(InsightsPredictionFieldObject$`labels`, "array[character]", loadNamespace("datarobot.apicore"))
      self$`values` <- ApiClient$new()$deserializeObj(InsightsPredictionFieldObject$`values`, "array[OneOfnumberarray]", loadNamespace("datarobot.apicore"))

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
