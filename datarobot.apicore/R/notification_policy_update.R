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
#' @title NotificationPolicyUpdate
#'
#' @description NotificationPolicyUpdate Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field active  character [optional] Defines if the notification policy is active or not.
#'
#' @field channelId  character [optional] The id of the notification channel to be used to send the notification.
#'
#' @field eventGroup  character [optional] The group of the event that trigger the notification.
#'
#' @field eventType  character [optional] The type of the event that triggers the notification.
#'
#' @field name  character [optional] The name of the notification policy.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
NotificationPolicyUpdate <- R6::R6Class(
  "NotificationPolicyUpdate",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`active` = NULL, `channelId` = NULL, `eventGroup` = NULL, `eventType` = NULL, `name` = NULL) {
      if (!is.null(`active`)) {
        stopifnot(is.logical(`active`), length(`active`) == 1)
      }
      if (!is.null(`channelId`)) {
        stopifnot(is.character(`channelId`), length(`channelId`) == 1)
      }
      if (!is.null(`eventGroup`)) {
        stopifnot(is.character(`eventGroup`), length(`eventGroup`) == 1)
      }
      if (!is.null(`eventType`)) {
        stopifnot(is.character(`eventType`), length(`eventType`) == 1)
      }
      if (!is.null(`name`)) {
        stopifnot(is.character(`name`), length(`name`) == 1)
      }
    }
  ),
  public = list(
    `active` = NULL,
    `channelId` = NULL,
    `eventGroup` = NULL,
    `eventType` = NULL,
    `name` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param active Defines if the notification policy is active or not.
    #' @param channelId The id of the notification channel to be used to send the notification.
    #' @param eventGroup The group of the event that trigger the notification.
    #' @param eventType The type of the event that triggers the notification.
    #' @param name The name of the notification policy.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`active` = NULL, `channelId` = NULL, `eventGroup` = NULL, `eventType` = NULL, `name` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(active, channelId, eventGroup, eventType, name)
      }
      self$`active` <- `active`
      self$`channelId` <- `channelId`
      self$`eventGroup` <- `eventGroup`
      self$`eventType` <- `eventType`
      self$`name` <- `name`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(active = self$`active`, channelId = self$`channelId`, eventGroup = self$`eventGroup`, eventType = self$`eventType`, name = self$`name`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`active`)) {
          sprintf(
            '"active":
            %s
                  ',
            tolower(self$`active`)
          )
        },
        if (!is.null(self$`channelId`)) {
          sprintf(
            '"channelId":
            "%s"
                  ',
            self$`channelId`
          )
        },
        if (!is.null(self$`eventGroup`)) {
          sprintf(
            '"eventGroup":
            "%s"
                  ',
            self$`eventGroup`
          )
        },
        if (!is.null(self$`eventType`)) {
          sprintf(
            '"eventType":
            "%s"
                  ',
            self$`eventType`
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
    #' @param NotificationPolicyUpdateJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(NotificationPolicyUpdateJson, validateParams = FALSE) {
      NotificationPolicyUpdateObject <- jsonlite::fromJSON(NotificationPolicyUpdateJson)
      self$`active` <- NotificationPolicyUpdateObject$`active`
      self$`channelId` <- NotificationPolicyUpdateObject$`channelId`
      self$`eventGroup` <- NotificationPolicyUpdateObject$`eventGroup`
      self$`eventType` <- NotificationPolicyUpdateObject$`eventType`
      self$`name` <- NotificationPolicyUpdateObject$`name`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
