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
#' @title CustomModelAccessControlResponse
#'
#' @description CustomModelAccessControlResponse Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field canShare  character Whether this user can share this custom model
#'
#' @field role  character This users role.
#'
#' @field userId  character This user&#39;s userId.
#'
#' @field username  character The username for this user&#39;s entry.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
CustomModelAccessControlResponse <- R6::R6Class(
  "CustomModelAccessControlResponse",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`canShare` = NULL, `role` = NULL, `userId` = NULL, `username` = NULL) {
      if (!is.null(`canShare`)) {
        stopifnot(is.logical(`canShare`), length(`canShare`) == 1)
      }
      if (!is.null(`role`)) {
        stopifnot(is.character(`role`), length(`role`) == 1)
      }
      if (!is.null(`userId`)) {
        stopifnot(is.character(`userId`), length(`userId`) == 1)
      }
      if (!is.null(`username`)) {
        stopifnot(is.character(`username`), length(`username`) == 1)
      }
    }
  ),
  public = list(
    `canShare` = NULL,
    `role` = NULL,
    `userId` = NULL,
    `username` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param canShare Whether this user can share this custom model
    #' @param role This users role.
    #' @param userId This user&#39;s userId.
    #' @param username The username for this user&#39;s entry.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`canShare` = NULL, `role` = NULL, `userId` = NULL, `username` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`canShare`, `role`, `userId`, `username`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(canShare, role, userId, username)
      }
      self$`canShare` <- `canShare`
      self$`role` <- `role`
      self$`userId` <- `userId`
      self$`username` <- `username`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(canShare = self$`canShare`, role = self$`role`, userId = self$`userId`, username = self$`username`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`canShare`)) {
          sprintf(
            '"canShare":
            %s
                  ',
            tolower(self$`canShare`)
          )
        },
        if (!is.null(self$`role`)) {
          sprintf(
            '"role":
            "%s"
                  ',
            self$`role`
          )
        },
        if (!is.null(self$`userId`)) {
          sprintf(
            '"userId":
            "%s"
                  ',
            self$`userId`
          )
        },
        if (!is.null(self$`username`)) {
          sprintf(
            '"username":
            "%s"
                  ',
            self$`username`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param CustomModelAccessControlResponseJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(CustomModelAccessControlResponseJson, validateParams = FALSE) {
      CustomModelAccessControlResponseObject <- jsonlite::fromJSON(CustomModelAccessControlResponseJson)
      self$`canShare` <- CustomModelAccessControlResponseObject$`canShare`
      self$`role` <- CustomModelAccessControlResponseObject$`role`
      self$`userId` <- CustomModelAccessControlResponseObject$`userId`
      self$`username` <- CustomModelAccessControlResponseObject$`username`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
