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
#' @title VersionRetrieveResponse
#'
#' @description VersionRetrieveResponse Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field major  integer The major version number.
#'
#' @field minor  integer The minor version number.
#'
#' @field versionString  character The full version string.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
VersionRetrieveResponse <- R6::R6Class(
  "VersionRetrieveResponse",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`major` = NULL, `minor` = NULL, `versionString` = NULL) {
      if (!is.null(`major`)) {
        stopifnot(is.numeric(`major`), length(`major`) == 1)
      }
      if (!is.null(`minor`)) {
        stopifnot(is.numeric(`minor`), length(`minor`) == 1)
      }
      if (!is.null(`versionString`)) {
        stopifnot(is.character(`versionString`), length(`versionString`) == 1)
      }
    }
  ),
  public = list(
    `major` = NULL,
    `minor` = NULL,
    `versionString` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param major The major version number.
    #' @param minor The minor version number.
    #' @param versionString The full version string.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`major` = NULL, `minor` = NULL, `versionString` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`major`, `minor`, `versionString`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(major, minor, versionString)
      }
      self$`major` <- `major`
      self$`minor` <- `minor`
      self$`versionString` <- `versionString`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(major = self$`major`, minor = self$`minor`, versionString = self$`versionString`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`major`)) {
          sprintf(
            '"major":
            %d
                  ',
            self$`major`
          )
        },
        if (!is.null(self$`minor`)) {
          sprintf(
            '"minor":
            %d
                  ',
            self$`minor`
          )
        },
        if (!is.null(self$`versionString`)) {
          sprintf(
            '"versionString":
            "%s"
                  ',
            self$`versionString`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param VersionRetrieveResponseJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(VersionRetrieveResponseJson, validateParams = FALSE) {
      VersionRetrieveResponseObject <- jsonlite::fromJSON(VersionRetrieveResponseJson)
      self$`major` <- VersionRetrieveResponseObject$`major`
      self$`minor` <- VersionRetrieveResponseObject$`minor`
      self$`versionString` <- VersionRetrieveResponseObject$`versionString`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
