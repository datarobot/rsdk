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
#' @title BulkDatasetAction
#'
#' @description BulkDatasetAction Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field datasetIds  list( character ) The dataset IDs to execute the bulk action on.
#'
#' @field payload  \link{OneOfBulkCatalogDeletePayloadBulkCatalogAppendTagsPayloadBulkCatalogSharePayload} indicate which action to run and with what parameters.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
BulkDatasetAction <- R6::R6Class(
  "BulkDatasetAction",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`datasetIds` = NULL, `payload` = NULL) {
      if (!is.null(`datasetIds`)) {
        stopifnot(is.vector(`datasetIds`), sapply(`datasetIds`, is.character))
      }
      if (!is.null(`payload`)) {
        .setComplexProperty(typeList = list(BulkCatalogDeletePayload, BulkCatalogAppendTagsPayload, BulkCatalogSharePayload), propertyData = payload)
      }
    }
  ),
  public = list(
    `datasetIds` = NULL,
    `payload` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param datasetIds The dataset IDs to execute the bulk action on.
    #' @param payload indicate which action to run and with what parameters.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`datasetIds` = NULL, `payload` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`datasetIds`, `payload`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(datasetIds, payload)
      }
      self$`datasetIds` <- `datasetIds`
      self$`payload` <- .setComplexProperty(typeList = list(BulkCatalogDeletePayload, BulkCatalogAppendTagsPayload, BulkCatalogSharePayload), propertyData = payload)
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(datasetIds = self$`datasetIds`, payload = self$`payload`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`datasetIds`)) {
          sprintf(
            '"datasetIds":
            [%s]
                  ',
            paste(unlist(lapply(self$`datasetIds`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`payload`)) {
          sprintf(
            '"payload":
            %s
      ',
            jsonlite::toJSON(self$`payload`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param BulkDatasetActionJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(BulkDatasetActionJson, validateParams = FALSE) {
      BulkDatasetActionObject <- jsonlite::fromJSON(BulkDatasetActionJson)
      self$`datasetIds` <- ApiClient$new()$deserializeObj(BulkDatasetActionObject$`datasetIds`, "array[character]", loadNamespace("datarobot.apicore"))
      self$`payload` <- .setComplexProperty(typeList = list(BulkCatalogDeletePayload, BulkCatalogAppendTagsPayload, BulkCatalogSharePayload), propertyData = BulkDatasetActionObject$payload)

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
