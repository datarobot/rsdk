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
#' @title DatasetAccessControlResponse
#'
#' @description DatasetAccessControlResponse Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field canShare  character True if this user can share with other users
#'
#' @field canUseData  character True if the user can view, download and process data (use to create projects, predictions, etc)
#'
#' @field role  character The role of the user on this data source.
#'
#' @field userFullName  character The full name of a user with access to this dataset. If the full name is not available, username is returned instead.
#'
#' @field username  character &#x60;username&#x60; of a user with access to this dataset.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
DatasetAccessControlResponse <- R6::R6Class(
  "DatasetAccessControlResponse",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`canShare` = NULL, `canUseData` = NULL, `role` = NULL, `userFullName` = NULL, `username` = NULL) {
      if (!is.null(`canShare`)) {
        stopifnot(is.logical(`canShare`), length(`canShare`) == 1)
      }
      if (!is.null(`canUseData`)) {
        stopifnot(is.logical(`canUseData`), length(`canUseData`) == 1)
      }
      if (!is.null(`role`)) {
        stopifnot(is.character(`role`), length(`role`) == 1)
      }
      if (!is.null(`userFullName`)) {
        stopifnot(is.character(`userFullName`), length(`userFullName`) == 1)
      }
      if (!is.null(`username`)) {
        stopifnot(is.character(`username`), length(`username`) == 1)
      }
    }
  ),
  public = list(
    `canShare` = NULL,
    `canUseData` = NULL,
    `role` = NULL,
    `userFullName` = NULL,
    `username` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param canShare True if this user can share with other users
    #' @param canUseData True if the user can view, download and process data (use to create projects, predictions, etc)
    #' @param role The role of the user on this data source.
    #' @param userFullName The full name of a user with access to this dataset. If the full name is not available, username is returned instead.
    #' @param username &#x60;username&#x60; of a user with access to this dataset.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`canShare` = NULL, `canUseData` = NULL, `role` = NULL, `userFullName` = NULL, `username` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`canShare`, `canUseData`, `role`, `userFullName`, `username`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(canShare, canUseData, role, userFullName, username)
      }
      self$`canShare` <- `canShare`
      self$`canUseData` <- `canUseData`
      self$`role` <- `role`
      self$`userFullName` <- `userFullName`
      self$`username` <- `username`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(canShare = self$`canShare`, canUseData = self$`canUseData`, role = self$`role`, userFullName = self$`userFullName`, username = self$`username`))
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
        if (!is.null(self$`role`)) {
          sprintf(
            '"role":
            "%s"
                  ',
            self$`role`
          )
        },
        if (!is.null(self$`userFullName`)) {
          sprintf(
            '"userFullName":
            "%s"
                  ',
            self$`userFullName`
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
    #' @param DatasetAccessControlResponseJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(DatasetAccessControlResponseJson, validateParams = FALSE) {
      DatasetAccessControlResponseObject <- jsonlite::fromJSON(DatasetAccessControlResponseJson)
      self$`canShare` <- DatasetAccessControlResponseObject$`canShare`
      self$`canUseData` <- DatasetAccessControlResponseObject$`canUseData`
      self$`role` <- DatasetAccessControlResponseObject$`role`
      self$`userFullName` <- DatasetAccessControlResponseObject$`userFullName`
      self$`username` <- DatasetAccessControlResponseObject$`username`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
