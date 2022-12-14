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
#' @title DeploymentOwnerResponse
#'
#' @description DeploymentOwnerResponse Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field email  character Email address of the owner.
#'
#' @field firstName  character First name of the owner.
#'
#' @field id  character ID of the owner.
#'
#' @field lastName  character Last name of the owner.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
DeploymentOwnerResponse <- R6::R6Class(
  "DeploymentOwnerResponse",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`email` = NULL, `firstName` = NULL, `id` = NULL, `lastName` = NULL) {
      if (!is.null(`email`)) {
        stopifnot(is.character(`email`), length(`email`) == 1)
      }
      if (!is.null(`firstName`)) {
        stopifnot(is.character(`firstName`), length(`firstName`) == 1)
      }
      if (!is.null(`id`)) {
        stopifnot(is.character(`id`), length(`id`) == 1)
      }
      if (!is.null(`lastName`)) {
        stopifnot(is.character(`lastName`), length(`lastName`) == 1)
      }
    }
  ),
  public = list(
    `email` = NULL,
    `firstName` = NULL,
    `id` = NULL,
    `lastName` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param email Email address of the owner.
    #' @param firstName First name of the owner.
    #' @param id ID of the owner.
    #' @param lastName Last name of the owner.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`email` = NULL, `firstName` = NULL, `id` = NULL, `lastName` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`email`, `firstName`, `id`, `lastName`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(email, firstName, id, lastName)
      }
      self$`email` <- `email`
      self$`firstName` <- `firstName`
      self$`id` <- `id`
      self$`lastName` <- `lastName`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(email = self$`email`, firstName = self$`firstName`, id = self$`id`, lastName = self$`lastName`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`email`)) {
          sprintf(
            '"email":
            "%s"
                  ',
            self$`email`
          )
        },
        if (!is.null(self$`firstName`)) {
          sprintf(
            '"firstName":
            "%s"
                  ',
            self$`firstName`
          )
        },
        if (!is.null(self$`id`)) {
          sprintf(
            '"id":
            "%s"
                  ',
            self$`id`
          )
        },
        if (!is.null(self$`lastName`)) {
          sprintf(
            '"lastName":
            "%s"
                  ',
            self$`lastName`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param DeploymentOwnerResponseJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(DeploymentOwnerResponseJson, validateParams = FALSE) {
      DeploymentOwnerResponseObject <- jsonlite::fromJSON(DeploymentOwnerResponseJson)
      self$`email` <- DeploymentOwnerResponseObject$`email`
      self$`firstName` <- DeploymentOwnerResponseObject$`firstName`
      self$`id` <- DeploymentOwnerResponseObject$`id`
      self$`lastName` <- DeploymentOwnerResponseObject$`lastName`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
