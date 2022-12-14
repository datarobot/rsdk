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
#' @title SamlMetadataFile
#'
#' @description SamlMetadataFile Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field fileName  character Path to IdP metadata file.
#'
#' @field value  character IdP metadata.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
SamlMetadataFile <- R6::R6Class(
  "SamlMetadataFile",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`fileName` = NULL, `value` = NULL) {
      if (!is.null(`fileName`)) {
        stopifnot(is.character(`fileName`), length(`fileName`) == 1)
      }
      if (!is.null(`value`)) {
        stopifnot(is.character(`value`), length(`value`) == 1)
      }
    }
  ),
  public = list(
    `fileName` = NULL,
    `value` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param fileName Path to IdP metadata file.
    #' @param value IdP metadata.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`fileName` = NULL, `value` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`fileName`, `value`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(fileName, value)
      }
      self$`fileName` <- `fileName`
      self$`value` <- `value`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(fileName = self$`fileName`, value = self$`value`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`fileName`)) {
          sprintf(
            '"fileName":
            "%s"
                  ',
            self$`fileName`
          )
        },
        if (!is.null(self$`value`)) {
          sprintf(
            '"value":
            "%s"
                  ',
            self$`value`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param SamlMetadataFileJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(SamlMetadataFileJson, validateParams = FALSE) {
      SamlMetadataFileObject <- jsonlite::fromJSON(SamlMetadataFileJson)
      self$`fileName` <- SamlMetadataFileObject$`fileName`
      self$`value` <- SamlMetadataFileObject$`value`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
