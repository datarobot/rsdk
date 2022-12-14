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
#' @title NotificationPolicyMuteCreate
#'
#' @description NotificationPolicyMuteCreate Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field entityId  character The id of the entity to mute the notification for
#'
#' @field policyId  character The id of the policy to mute notification for
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
NotificationPolicyMuteCreate <- R6::R6Class(
  "NotificationPolicyMuteCreate",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`entityId` = NULL, `policyId` = NULL) {
      if (!is.null(`entityId`)) {
        stopifnot(is.character(`entityId`), length(`entityId`) == 1)
      }
      if (!is.null(`policyId`)) {
        stopifnot(is.character(`policyId`), length(`policyId`) == 1)
      }
    }
  ),
  public = list(
    `entityId` = NULL,
    `policyId` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param entityId The id of the entity to mute the notification for
    #' @param policyId The id of the policy to mute notification for
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`entityId` = NULL, `policyId` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`entityId`, `policyId`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(entityId, policyId)
      }
      self$`entityId` <- `entityId`
      self$`policyId` <- `policyId`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(entityId = self$`entityId`, policyId = self$`policyId`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`entityId`)) {
          sprintf(
            '"entityId":
            "%s"
                  ',
            self$`entityId`
          )
        },
        if (!is.null(self$`policyId`)) {
          sprintf(
            '"policyId":
            "%s"
                  ',
            self$`policyId`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param NotificationPolicyMuteCreateJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(NotificationPolicyMuteCreateJson, validateParams = FALSE) {
      NotificationPolicyMuteCreateObject <- jsonlite::fromJSON(NotificationPolicyMuteCreateJson)
      self$`entityId` <- NotificationPolicyMuteCreateObject$`entityId`
      self$`policyId` <- NotificationPolicyMuteCreateObject$`policyId`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
