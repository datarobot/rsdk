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
#' @title MulticlassLiftData
#'
#' @description MulticlassLiftData Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field classBins  list( \link{MulticlassLiftDataClassBins} ) List of lift chart data for each target class.
#'
#' @field datasetId  character The dataset id of dataset which was used to compute Lift chart.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
MulticlassLiftData <- R6::R6Class(
  "MulticlassLiftData",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`classBins` = NULL, `datasetId` = NULL) {
      if (!is.null(`classBins`)) {
        stopifnot(is.vector(`classBins`), sapply(`classBins`, R6::is.R6))
      }
      if (!is.null(`datasetId`)) {
        stopifnot(is.character(`datasetId`), length(`datasetId`) == 1)
      }
    }
  ),
  public = list(
    `classBins` = NULL,
    `datasetId` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param classBins List of lift chart data for each target class.
    #' @param datasetId The dataset id of dataset which was used to compute Lift chart.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`classBins` = NULL, `datasetId` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`classBins`, `datasetId`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(classBins, datasetId)
      }
      self$`classBins` <- `classBins`
      self$`datasetId` <- `datasetId`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(classBins = self$`classBins`, datasetId = self$`datasetId`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`classBins`)) {
          sprintf(
            '"classBins":
            [%s]
      ',
            paste(sapply(self$`classBins`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`datasetId`)) {
          sprintf(
            '"datasetId":
            "%s"
                  ',
            self$`datasetId`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param MulticlassLiftDataJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(MulticlassLiftDataJson, validateParams = FALSE) {
      MulticlassLiftDataObject <- jsonlite::fromJSON(MulticlassLiftDataJson)
      self$`classBins` <- ApiClient$new()$deserializeObj(MulticlassLiftDataObject$`classBins`, "array[MulticlassLiftDataClassBins]", loadNamespace("datarobot.apicore"))
      self$`datasetId` <- MulticlassLiftDataObject$`datasetId`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
