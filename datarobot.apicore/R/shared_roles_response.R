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
#' @title SharedRolesResponse
#'
#' @description SharedRolesResponse Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field canShare  character True if this user can share with other users
#'
#' @field canUseData  character True if the user can view, download and process data (use to create projects, predictions, etc)
#'
#' @field id  character The ID of the recipient organization, group or user.
#'
#' @field name  character The name of the recipient organization, group or user.
#'
#' @field role  character The role of the org/group/user on this dataset or \&quot;NO_ROLE\&quot; for removing access when used with route to modify access.
#'
#' @field shareRecipientType  character It describes the recipient type.
#'
#' @field userFullName  character [optional] If the recipient type is a user, the full name of the user if available.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
SharedRolesResponse <- R6::R6Class(
  "SharedRolesResponse",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`canShare` = NULL, `canUseData` = NULL, `id` = NULL, `name` = NULL, `role` = NULL, `shareRecipientType` = NULL, `userFullName` = NULL) {
      if (!is.null(`canShare`)) {
        stopifnot(is.logical(`canShare`), length(`canShare`) == 1)
      }
      if (!is.null(`canUseData`)) {
        stopifnot(is.logical(`canUseData`), length(`canUseData`) == 1)
      }
      if (!is.null(`id`)) {
        stopifnot(is.character(`id`), length(`id`) == 1)
      }
      if (!is.null(`name`)) {
        stopifnot(is.character(`name`), length(`name`) == 1)
      }
      if (!is.null(`role`)) {
        stopifnot(is.character(`role`), length(`role`) == 1)
      }
      if (!is.null(`shareRecipientType`)) {
        stopifnot(is.character(`shareRecipientType`), length(`shareRecipientType`) == 1)
      }
      if (!is.null(`userFullName`)) {
        stopifnot(is.character(`userFullName`), length(`userFullName`) == 1)
      }
    }
  ),
  public = list(
    `canShare` = NULL,
    `canUseData` = NULL,
    `id` = NULL,
    `name` = NULL,
    `role` = NULL,
    `shareRecipientType` = NULL,
    `userFullName` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param canShare True if this user can share with other users
    #' @param canUseData True if the user can view, download and process data (use to create projects, predictions, etc)
    #' @param id The ID of the recipient organization, group or user.
    #' @param name The name of the recipient organization, group or user.
    #' @param role The role of the org/group/user on this dataset or \&quot;NO_ROLE\&quot; for removing access when used with route to modify access.
    #' @param shareRecipientType It describes the recipient type.
    #' @param userFullName If the recipient type is a user, the full name of the user if available.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`canShare` = NULL, `canUseData` = NULL, `id` = NULL, `name` = NULL, `role` = NULL, `shareRecipientType` = NULL, `userFullName` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`canShare`, `canUseData`, `id`, `name`, `role`, `shareRecipientType`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(canShare, canUseData, id, name, role, shareRecipientType, userFullName)
      }
      self$`canShare` <- `canShare`
      self$`canUseData` <- `canUseData`
      self$`id` <- `id`
      self$`name` <- `name`
      self$`role` <- `role`
      self$`shareRecipientType` <- `shareRecipientType`
      self$`userFullName` <- `userFullName`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(canShare = self$`canShare`, canUseData = self$`canUseData`, id = self$`id`, name = self$`name`, role = self$`role`, shareRecipientType = self$`shareRecipientType`, userFullName = self$`userFullName`))
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
        if (!is.null(self$`canUseData`)) {
          sprintf(
            '"canUseData":
            %s
                  ',
            tolower(self$`canUseData`)
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
        if (!is.null(self$`name`)) {
          sprintf(
            '"name":
            "%s"
                  ',
            self$`name`
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
        if (!is.null(self$`shareRecipientType`)) {
          sprintf(
            '"shareRecipientType":
            "%s"
                  ',
            self$`shareRecipientType`
          )
        },
        if (!is.null(self$`userFullName`)) {
          sprintf(
            '"userFullName":
            "%s"
                  ',
            self$`userFullName`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param SharedRolesResponseJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(SharedRolesResponseJson, validateParams = FALSE) {
      SharedRolesResponseObject <- jsonlite::fromJSON(SharedRolesResponseJson)
      self$`canShare` <- SharedRolesResponseObject$`canShare`
      self$`canUseData` <- SharedRolesResponseObject$`canUseData`
      self$`id` <- SharedRolesResponseObject$`id`
      self$`name` <- SharedRolesResponseObject$`name`
      self$`role` <- SharedRolesResponseObject$`role`
      self$`shareRecipientType` <- SharedRolesResponseObject$`shareRecipientType`
      self$`userFullName` <- SharedRolesResponseObject$`userFullName`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
