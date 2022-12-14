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
#' @title UserNotificationRelatedUserResponse
#'
#' @description UserNotificationRelatedUserResponse Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field fullName  character [optional] User&#39;s full name.
#'
#' @field gravatarHash  character [optional] User&#39;s gravatar hash.
#'
#' @field inactive  character [optional] True if the user was deleted.
#'
#' @field uid  character The ID of the user
#'
#' @field username  character [optional] Username of the user.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
UserNotificationRelatedUserResponse <- R6::R6Class(
  "UserNotificationRelatedUserResponse",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`fullName` = NULL, `gravatarHash` = NULL, `inactive` = NULL, `uid` = NULL, `username` = NULL) {
      if (!is.null(`uid`)) {
        stopifnot(is.character(`uid`), length(`uid`) == 1)
      }
      if (!is.null(`fullName`)) {
        stopifnot(is.character(`fullName`), length(`fullName`) == 1)
      }
      if (!is.null(`gravatarHash`)) {
        stopifnot(is.character(`gravatarHash`), length(`gravatarHash`) == 1)
      }
      if (!is.null(`inactive`)) {
        stopifnot(is.logical(`inactive`), length(`inactive`) == 1)
      }
      if (!is.null(`username`)) {
        stopifnot(is.character(`username`), length(`username`) == 1)
      }
    }
  ),
  public = list(
    `fullName` = NULL,
    `gravatarHash` = NULL,
    `inactive` = NULL,
    `uid` = NULL,
    `username` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param fullName User&#39;s full name.
    #' @param gravatarHash User&#39;s gravatar hash.
    #' @param inactive True if the user was deleted.
    #' @param uid The ID of the user
    #' @param username Username of the user.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`uid` = NULL, `fullName` = NULL, `gravatarHash` = NULL, `inactive` = NULL, `username` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`uid`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(fullName, gravatarHash, inactive, uid, username)
      }
      self$`fullName` <- `fullName`
      self$`gravatarHash` <- `gravatarHash`
      self$`inactive` <- `inactive`
      self$`uid` <- `uid`
      self$`username` <- `username`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(fullName = self$`fullName`, gravatarHash = self$`gravatarHash`, inactive = self$`inactive`, uid = self$`uid`, username = self$`username`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`fullName`)) {
          sprintf(
            '"fullName":
            "%s"
                  ',
            self$`fullName`
          )
        },
        if (!is.null(self$`gravatarHash`)) {
          sprintf(
            '"gravatarHash":
            "%s"
                  ',
            self$`gravatarHash`
          )
        },
        if (!is.null(self$`inactive`)) {
          sprintf(
            '"inactive":
            %s
                  ',
            tolower(self$`inactive`)
          )
        },
        if (!is.null(self$`uid`)) {
          sprintf(
            '"uid":
            "%s"
                  ',
            self$`uid`
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
    #' @param UserNotificationRelatedUserResponseJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(UserNotificationRelatedUserResponseJson, validateParams = FALSE) {
      UserNotificationRelatedUserResponseObject <- jsonlite::fromJSON(UserNotificationRelatedUserResponseJson)
      self$`fullName` <- UserNotificationRelatedUserResponseObject$`fullName`
      self$`gravatarHash` <- UserNotificationRelatedUserResponseObject$`gravatarHash`
      self$`inactive` <- UserNotificationRelatedUserResponseObject$`inactive`
      self$`uid` <- UserNotificationRelatedUserResponseObject$`uid`
      self$`username` <- UserNotificationRelatedUserResponseObject$`username`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
