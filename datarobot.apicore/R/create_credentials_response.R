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
#' @title CreateCredentialsResponse
#'
#' @description CreateCredentialsResponse Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field creationDate  character ISO-8601 formatted date/time when these credentials were created.
#'
#' @field credentialId  character ID of these credentials.
#'
#' @field credentialType  character [optional] Type of credentials.
#'
#' @field description  character [optional] Description of these credentials.
#'
#' @field name  character Name of these credentials.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
CreateCredentialsResponse <- R6::R6Class(
  "CreateCredentialsResponse",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`creationDate` = NULL, `credentialId` = NULL, `credentialType` = NULL, `description` = NULL, `name` = NULL) {
      if (!is.null(`creationDate`)) {
        stopifnot(inherits(`creationDate`, "POSIXt"))
      }
      if (!is.null(`credentialId`)) {
        stopifnot(is.character(`credentialId`), length(`credentialId`) == 1)
      }
      if (!is.null(`name`)) {
        stopifnot(is.character(`name`), length(`name`) == 1)
      }
      if (!is.null(`credentialType`)) {
        stopifnot(is.character(`credentialType`), length(`credentialType`) == 1)
      }
      if (!is.null(`description`)) {
        stopifnot(is.character(`description`), length(`description`) == 1)
      }
    }
  ),
  public = list(
    `creationDate` = NULL,
    `credentialId` = NULL,
    `credentialType` = NULL,
    `description` = NULL,
    `name` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param creationDate ISO-8601 formatted date/time when these credentials were created.
    #' @param credentialId ID of these credentials.
    #' @param credentialType Type of credentials.
    #' @param description Description of these credentials.
    #' @param name Name of these credentials.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`creationDate` = NULL, `credentialId` = NULL, `name` = NULL, `credentialType` = "basic", `description` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`creationDate`, `credentialId`, `name`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(creationDate, credentialId, credentialType, description, name)
      }
      self$`creationDate` <- `creationDate`
      self$`credentialId` <- `credentialId`
      self$`credentialType` <- `credentialType`
      self$`description` <- `description`
      self$`name` <- `name`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(creationDate = self$`creationDate`, credentialId = self$`credentialId`, credentialType = self$`credentialType`, description = self$`description`, name = self$`name`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`creationDate`)) {
          sprintf(
            '"creationDate":
            "%s"
                  ',
            format(self$`creationDate`, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
          )
        },
        if (!is.null(self$`credentialId`)) {
          sprintf(
            '"credentialId":
            "%s"
                  ',
            self$`credentialId`
          )
        },
        if (!is.null(self$`credentialType`)) {
          sprintf(
            '"credentialType":
            "%s"
                  ',
            self$`credentialType`
          )
        },
        if (!is.null(self$`description`)) {
          sprintf(
            '"description":
            "%s"
                  ',
            self$`description`
          )
        },
        if (!is.null(self$`name`)) {
          sprintf(
            '"name":
            "%s"
                  ',
            self$`name`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param CreateCredentialsResponseJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(CreateCredentialsResponseJson, validateParams = FALSE) {
      CreateCredentialsResponseObject <- jsonlite::fromJSON(CreateCredentialsResponseJson)
      self$`creationDate` <- ParseRFC3339Timestamp(CreateCredentialsResponseObject$`creationDate`)
      self$`credentialId` <- CreateCredentialsResponseObject$`credentialId`
      self$`credentialType` <- CreateCredentialsResponseObject$`credentialType`
      self$`description` <- CreateCredentialsResponseObject$`description`
      self$`name` <- CreateCredentialsResponseObject$`name`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
