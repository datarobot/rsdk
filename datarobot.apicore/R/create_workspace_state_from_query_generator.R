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
#' @title CreateWorkspaceStateFromQueryGenerator
#'
#' @description CreateWorkspaceStateFromQueryGenerator Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field datasetId  character [optional] The ID of the dataset.
#'
#' @field datasetVersionId  character [optional] The ID of the dataset version.
#'
#' @field queryGeneratorId  character The ID of the query generator.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
CreateWorkspaceStateFromQueryGenerator <- R6::R6Class(
  "CreateWorkspaceStateFromQueryGenerator",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`datasetId` = NULL, `datasetVersionId` = NULL, `queryGeneratorId` = NULL) {
      if (!is.null(`queryGeneratorId`)) {
        stopifnot(is.character(`queryGeneratorId`), length(`queryGeneratorId`) == 1)
      }
      if (!is.null(`datasetId`)) {
        stopifnot(is.character(`datasetId`), length(`datasetId`) == 1)
      }
      if (!is.null(`datasetVersionId`)) {
        stopifnot(is.character(`datasetVersionId`), length(`datasetVersionId`) == 1)
      }
    }
  ),
  public = list(
    `datasetId` = NULL,
    `datasetVersionId` = NULL,
    `queryGeneratorId` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param datasetId The ID of the dataset.
    #' @param datasetVersionId The ID of the dataset version.
    #' @param queryGeneratorId The ID of the query generator.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`queryGeneratorId` = NULL, `datasetId` = NULL, `datasetVersionId` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`queryGeneratorId`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(datasetId, datasetVersionId, queryGeneratorId)
      }
      self$`datasetId` <- `datasetId`
      self$`datasetVersionId` <- `datasetVersionId`
      self$`queryGeneratorId` <- `queryGeneratorId`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(datasetId = self$`datasetId`, datasetVersionId = self$`datasetVersionId`, queryGeneratorId = self$`queryGeneratorId`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`datasetId`)) {
          sprintf(
            '"datasetId":
            "%s"
                  ',
            self$`datasetId`
          )
        },
        if (!is.null(self$`datasetVersionId`)) {
          sprintf(
            '"datasetVersionId":
            "%s"
                  ',
            self$`datasetVersionId`
          )
        },
        if (!is.null(self$`queryGeneratorId`)) {
          sprintf(
            '"queryGeneratorId":
            "%s"
                  ',
            self$`queryGeneratorId`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param CreateWorkspaceStateFromQueryGeneratorJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(CreateWorkspaceStateFromQueryGeneratorJson, validateParams = FALSE) {
      CreateWorkspaceStateFromQueryGeneratorObject <- jsonlite::fromJSON(CreateWorkspaceStateFromQueryGeneratorJson)
      self$`datasetId` <- CreateWorkspaceStateFromQueryGeneratorObject$`datasetId`
      self$`datasetVersionId` <- CreateWorkspaceStateFromQueryGeneratorObject$`datasetVersionId`
      self$`queryGeneratorId` <- CreateWorkspaceStateFromQueryGeneratorObject$`queryGeneratorId`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
