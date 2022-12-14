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
#' @title BaseDependencyBuildMetadataResponse
#'
#' @description BaseDependencyBuildMetadataResponse Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field buildEnd  character The ISO-8601 encoded time when this build completed.
#'
#' @field buildLogLocation  character The URL to download the build logs from this build.
#'
#' @field buildStart  character The ISO-8601 encoded time when this build started.
#'
#' @field buildStatus  character The current status of the dependency build.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
BaseDependencyBuildMetadataResponse <- R6::R6Class(
  "BaseDependencyBuildMetadataResponse",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`buildEnd` = NULL, `buildLogLocation` = NULL, `buildStart` = NULL, `buildStatus` = NULL) {
      if (!is.null(`buildEnd`)) {
        stopifnot(is.character(`buildEnd`), length(`buildEnd`) == 1)
      }
      if (!is.null(`buildLogLocation`)) {
        stopifnot(is.character(`buildLogLocation`), length(`buildLogLocation`) == 1)
      }
      if (!is.null(`buildStart`)) {
        stopifnot(is.character(`buildStart`), length(`buildStart`) == 1)
      }
      if (!is.null(`buildStatus`)) {
        stopifnot(is.character(`buildStatus`), length(`buildStatus`) == 1)
      }
    }
  ),
  public = list(
    `buildEnd` = NULL,
    `buildLogLocation` = NULL,
    `buildStart` = NULL,
    `buildStatus` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param buildEnd The ISO-8601 encoded time when this build completed.
    #' @param buildLogLocation The URL to download the build logs from this build.
    #' @param buildStart The ISO-8601 encoded time when this build started.
    #' @param buildStatus The current status of the dependency build.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`buildEnd` = NULL, `buildLogLocation` = NULL, `buildStart` = NULL, `buildStatus` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`buildEnd`, `buildLogLocation`, `buildStart`, `buildStatus`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(buildEnd, buildLogLocation, buildStart, buildStatus)
      }
      self$`buildEnd` <- `buildEnd`
      self$`buildLogLocation` <- `buildLogLocation`
      self$`buildStart` <- `buildStart`
      self$`buildStatus` <- `buildStatus`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(buildEnd = self$`buildEnd`, buildLogLocation = self$`buildLogLocation`, buildStart = self$`buildStart`, buildStatus = self$`buildStatus`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`buildEnd`)) {
          sprintf(
            '"buildEnd":
            "%s"
                  ',
            self$`buildEnd`
          )
        },
        if (!is.null(self$`buildLogLocation`)) {
          sprintf(
            '"buildLogLocation":
            "%s"
                  ',
            self$`buildLogLocation`
          )
        },
        if (!is.null(self$`buildStart`)) {
          sprintf(
            '"buildStart":
            "%s"
                  ',
            self$`buildStart`
          )
        },
        if (!is.null(self$`buildStatus`)) {
          sprintf(
            '"buildStatus":
            "%s"
                  ',
            self$`buildStatus`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param BaseDependencyBuildMetadataResponseJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(BaseDependencyBuildMetadataResponseJson, validateParams = FALSE) {
      BaseDependencyBuildMetadataResponseObject <- jsonlite::fromJSON(BaseDependencyBuildMetadataResponseJson)
      self$`buildEnd` <- BaseDependencyBuildMetadataResponseObject$`buildEnd`
      self$`buildLogLocation` <- BaseDependencyBuildMetadataResponseObject$`buildLogLocation`
      self$`buildStart` <- BaseDependencyBuildMetadataResponseObject$`buildStart`
      self$`buildStatus` <- BaseDependencyBuildMetadataResponseObject$`buildStatus`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
