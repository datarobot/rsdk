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
#' @title FrequentValuesResponse
#'
#' @description FrequentValuesResponse Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field frequentValues  list( \link{FrequentValueData} ) List of frequent value and data quality information
#'
#' @field name  character Name of the feature
#'
#' @field numRows  integer Number of rows in the sample used to determine frequent values
#'
#' @field projectId  character Project Id
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
FrequentValuesResponse <- R6::R6Class(
  "FrequentValuesResponse",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`frequentValues` = NULL, `name` = NULL, `numRows` = NULL, `projectId` = NULL) {
      if (!is.null(`frequentValues`)) {
        stopifnot(is.vector(`frequentValues`), sapply(`frequentValues`, R6::is.R6))
      }
      if (!is.null(`name`)) {
        stopifnot(is.character(`name`), length(`name`) == 1)
      }
      if (!is.null(`numRows`)) {
        stopifnot(is.numeric(`numRows`), length(`numRows`) == 1)
      }
      if (!is.null(`projectId`)) {
        stopifnot(is.character(`projectId`), length(`projectId`) == 1)
      }
    }
  ),
  public = list(
    `frequentValues` = NULL,
    `name` = NULL,
    `numRows` = NULL,
    `projectId` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param frequentValues List of frequent value and data quality information
    #' @param name Name of the feature
    #' @param numRows Number of rows in the sample used to determine frequent values
    #' @param projectId Project Id
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`frequentValues` = NULL, `name` = NULL, `numRows` = NULL, `projectId` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`frequentValues`, `name`, `numRows`, `projectId`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(frequentValues, name, numRows, projectId)
      }
      self$`frequentValues` <- `frequentValues`
      self$`name` <- `name`
      self$`numRows` <- `numRows`
      self$`projectId` <- `projectId`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(frequentValues = self$`frequentValues`, name = self$`name`, numRows = self$`numRows`, projectId = self$`projectId`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`frequentValues`)) {
          sprintf(
            '"frequentValues":
            [%s]
      ',
            paste(sapply(self$`frequentValues`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`name`)) {
          sprintf(
            '"name":
            "%s"
                  ',
            self$`name`
          )
        },
        if (!is.null(self$`numRows`)) {
          sprintf(
            '"numRows":
            %d
                  ',
            self$`numRows`
          )
        },
        if (!is.null(self$`projectId`)) {
          sprintf(
            '"projectId":
            "%s"
                  ',
            self$`projectId`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param FrequentValuesResponseJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(FrequentValuesResponseJson, validateParams = FALSE) {
      FrequentValuesResponseObject <- jsonlite::fromJSON(FrequentValuesResponseJson)
      self$`frequentValues` <- ApiClient$new()$deserializeObj(FrequentValuesResponseObject$`frequentValues`, "array[FrequentValueData]", loadNamespace("datarobot.apicore"))
      self$`name` <- FrequentValuesResponseObject$`name`
      self$`numRows` <- FrequentValuesResponseObject$`numRows`
      self$`projectId` <- FrequentValuesResponseObject$`projectId`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
