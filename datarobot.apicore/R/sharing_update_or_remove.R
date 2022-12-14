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
#' @title SharingUpdateOrRemove
#'
#' @description SharingUpdateOrRemove Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field data  list( \link{UpdateAccessControl} ) The role to set for the user.
#'
#' @field includeFeatureDiscoveryEntities  character [optional] Whether to share all the related entities.
#'
#' @field sendNotification  character [optional] Send an email notification.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
SharingUpdateOrRemove <- R6::R6Class(
  "SharingUpdateOrRemove",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`data` = NULL, `includeFeatureDiscoveryEntities` = NULL, `sendNotification` = NULL) {
      if (!is.null(`data`)) {
        stopifnot(is.vector(`data`), sapply(`data`, R6::is.R6))
      }
      if (!is.null(`includeFeatureDiscoveryEntities`)) {
        stopifnot(is.logical(`includeFeatureDiscoveryEntities`), length(`includeFeatureDiscoveryEntities`) == 1)
      }
      if (!is.null(`sendNotification`)) {
        stopifnot(is.logical(`sendNotification`), length(`sendNotification`) == 1)
      }
    }
  ),
  public = list(
    `data` = NULL,
    `includeFeatureDiscoveryEntities` = NULL,
    `sendNotification` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param data The role to set for the user.
    #' @param includeFeatureDiscoveryEntities Whether to share all the related entities.
    #' @param sendNotification Send an email notification.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`data` = NULL, `includeFeatureDiscoveryEntities` = FALSE, `sendNotification` = TRUE, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`data`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(data, includeFeatureDiscoveryEntities, sendNotification)
      }
      self$`data` <- `data`
      self$`includeFeatureDiscoveryEntities` <- `includeFeatureDiscoveryEntities`
      self$`sendNotification` <- `sendNotification`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(data = self$`data`, includeFeatureDiscoveryEntities = self$`includeFeatureDiscoveryEntities`, sendNotification = self$`sendNotification`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`data`)) {
          sprintf(
            '"data":
            [%s]
      ',
            paste(sapply(self$`data`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`includeFeatureDiscoveryEntities`)) {
          sprintf(
            '"includeFeatureDiscoveryEntities":
            %s
                  ',
            tolower(self$`includeFeatureDiscoveryEntities`)
          )
        },
        if (!is.null(self$`sendNotification`)) {
          sprintf(
            '"sendNotification":
            %s
                  ',
            tolower(self$`sendNotification`)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param SharingUpdateOrRemoveJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(SharingUpdateOrRemoveJson, validateParams = FALSE) {
      SharingUpdateOrRemoveObject <- jsonlite::fromJSON(SharingUpdateOrRemoveJson)
      self$`data` <- ApiClient$new()$deserializeObj(SharingUpdateOrRemoveObject$`data`, "array[UpdateAccessControl]", loadNamespace("datarobot.apicore"))
      self$`includeFeatureDiscoveryEntities` <- SharingUpdateOrRemoveObject$`includeFeatureDiscoveryEntities`
      self$`sendNotification` <- SharingUpdateOrRemoveObject$`sendNotification`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
