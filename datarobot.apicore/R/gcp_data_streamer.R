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
#' @title GCPDataStreamer
#'
#' @description GCPDataStreamer Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field credentialId  \link{OneOfstringstring} [optional] Either the populated value of the field or [redacted] due to permission settings
#'
#' @field format  character [optional] Type of input file format
#'
#' @field type  character Type name for this intake type
#'
#' @field url  character URL for the CSV file
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
GCPDataStreamer <- R6::R6Class(
  "GCPDataStreamer",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`credentialId` = NULL, `format` = NULL, `type` = NULL, `url` = NULL) {
      if (!is.null(`type`)) {
        stopifnot(is.character(`type`), length(`type`) == 1)
      }
      if (!is.null(`url`)) {
        stopifnot(is.character(`url`), length(`url`) == 1)
      }
      if (!is.null(`credentialId`)) {
        .setPrimitiveProperty(typeList = list("character"), propertyData = credentialId)
      }
      if (!is.null(`format`)) {
        stopifnot(is.character(`format`), length(`format`) == 1)
      }
    }
  ),
  public = list(
    `credentialId` = NULL,
    `format` = NULL,
    `type` = NULL,
    `url` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param credentialId Either the populated value of the field or [redacted] due to permission settings
    #' @param format Type of input file format
    #' @param type Type name for this intake type
    #' @param url URL for the CSV file
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`type` = NULL, `url` = NULL, `credentialId` = NULL, `format` = "csv", validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`type`, `url`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(credentialId, format, type, url)
      }
      self$`credentialId` <- .setPrimitiveProperty(typeList = list("character"), propertyData = credentialId)
      self$`format` <- `format`
      self$`type` <- `type`
      self$`url` <- `url`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(credentialId = self$`credentialId`, format = self$`format`, type = self$`type`, url = self$`url`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`credentialId`)) {
          sprintf(
            '"credentialId":
            %s
      ',
            self$`credentialId`
          )
        },
        if (!is.null(self$`format`)) {
          sprintf(
            '"format":
            "%s"
                  ',
            self$`format`
          )
        },
        if (!is.null(self$`type`)) {
          sprintf(
            '"type":
            "%s"
                  ',
            self$`type`
          )
        },
        if (!is.null(self$`url`)) {
          sprintf(
            '"url":
            "%s"
                  ',
            self$`url`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param GCPDataStreamerJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(GCPDataStreamerJson, validateParams = FALSE) {
      GCPDataStreamerObject <- jsonlite::fromJSON(GCPDataStreamerJson)
      self$`credentialId` <- .setPrimitiveProperty(typeList = list("character"), propertyData = GCPDataStreamerObject$credentialId)
      self$`format` <- GCPDataStreamerObject$`format`
      self$`type` <- GCPDataStreamerObject$`type`
      self$`url` <- GCPDataStreamerObject$`url`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
