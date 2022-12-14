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
#' @title UpdateDriverRequest
#'
#' @description UpdateDriverRequest Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field baseNames  list( character ) [optional] Original file name(s) of the uploaded JAR file(s). If there are multiple JAR files required for the driver, each of the original file names should be present in the list in the same order as found in &#39;localJarUrls&#39;
#'
#' @field canonicalName  character [optional] User-friendly driver name.
#'
#' @field className  character [optional] Driver class name. For example &#39;com.amazon.redshift.jdbc.Driver&#39;)
#'
#' @field configurationId  character [optional] Driver configuration ID if it was provided during driver upload.
#'
#' @field localJarUrls  list( character ) [optional] File path(s) for the driver files.This path is returned in the response as &#39;local_url&#39; by the driverUpload route.If there are multiple JAR files required for the driver, each uploaded JAR must be present in this list. If specified, values will replace any previous settings.
#'
#' @field removeConfig  character [optional] Pass &#x60;True&#x60; to remove configuration currently associated with this driver. Note: must pass &#x60;canonicalName&#x60; and &#x60;className&#x60; if removing the configuration.
#'
#' @field skipConfigVerification  character [optional] Pass &#x60;True&#x60; to skip &#x60;jdbc_url&#x60; verification.
#'
#' @field version  character [optional] Driver version, which is used to construct the canonical name if driver configuration ID was provided during driver upload.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
UpdateDriverRequest <- R6::R6Class(
  "UpdateDriverRequest",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`baseNames` = NULL, `canonicalName` = NULL, `className` = NULL, `configurationId` = NULL, `localJarUrls` = NULL, `removeConfig` = NULL, `skipConfigVerification` = NULL, `version` = NULL) {
      if (!is.null(`baseNames`) && length(`baseNames`) > 0) {
        stopifnot(is.vector(`baseNames`), sapply(`baseNames`, is.character))
      }
      if (!is.null(`canonicalName`)) {
        stopifnot(is.character(`canonicalName`), length(`canonicalName`) == 1)
      }
      if (!is.null(`className`)) {
        stopifnot(is.character(`className`), length(`className`) == 1)
      }
      if (!is.null(`configurationId`)) {
        stopifnot(is.character(`configurationId`), length(`configurationId`) == 1)
      }
      if (!is.null(`localJarUrls`) && length(`localJarUrls`) > 0) {
        stopifnot(is.vector(`localJarUrls`), sapply(`localJarUrls`, is.character))
      }
      if (!is.null(`removeConfig`)) {
        stopifnot(is.logical(`removeConfig`), length(`removeConfig`) == 1)
      }
      if (!is.null(`skipConfigVerification`)) {
        stopifnot(is.logical(`skipConfigVerification`), length(`skipConfigVerification`) == 1)
      }
      if (!is.null(`version`)) {
        stopifnot(is.character(`version`), length(`version`) == 1)
      }
    }
  ),
  public = list(
    `baseNames` = NULL,
    `canonicalName` = NULL,
    `className` = NULL,
    `configurationId` = NULL,
    `localJarUrls` = NULL,
    `removeConfig` = NULL,
    `skipConfigVerification` = NULL,
    `version` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param baseNames Original file name(s) of the uploaded JAR file(s). If there are multiple JAR files required for the driver, each of the original file names should be present in the list in the same order as found in &#39;localJarUrls&#39;
    #' @param canonicalName User-friendly driver name.
    #' @param className Driver class name. For example &#39;com.amazon.redshift.jdbc.Driver&#39;)
    #' @param configurationId Driver configuration ID if it was provided during driver upload.
    #' @param localJarUrls File path(s) for the driver files.This path is returned in the response as &#39;local_url&#39; by the driverUpload route.If there are multiple JAR files required for the driver, each uploaded JAR must be present in this list. If specified, values will replace any previous settings.
    #' @param removeConfig Pass &#x60;True&#x60; to remove configuration currently associated with this driver. Note: must pass &#x60;canonicalName&#x60; and &#x60;className&#x60; if removing the configuration.
    #' @param skipConfigVerification Pass &#x60;True&#x60; to skip &#x60;jdbc_url&#x60; verification.
    #' @param version Driver version, which is used to construct the canonical name if driver configuration ID was provided during driver upload.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`baseNames` = NULL, `canonicalName` = NULL, `className` = NULL, `configurationId` = NULL, `localJarUrls` = NULL, `removeConfig` = NULL, `skipConfigVerification` = NULL, `version` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(baseNames, canonicalName, className, configurationId, localJarUrls, removeConfig, skipConfigVerification, version)
      }
      self$`baseNames` <- `baseNames`
      self$`canonicalName` <- `canonicalName`
      self$`className` <- `className`
      self$`configurationId` <- `configurationId`
      self$`localJarUrls` <- `localJarUrls`
      self$`removeConfig` <- `removeConfig`
      self$`skipConfigVerification` <- `skipConfigVerification`
      self$`version` <- `version`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(baseNames = self$`baseNames`, canonicalName = self$`canonicalName`, className = self$`className`, configurationId = self$`configurationId`, localJarUrls = self$`localJarUrls`, removeConfig = self$`removeConfig`, skipConfigVerification = self$`skipConfigVerification`, version = self$`version`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`baseNames`)) {
          sprintf(
            '"baseNames":
            [%s]
                  ',
            paste(unlist(lapply(self$`baseNames`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`canonicalName`)) {
          sprintf(
            '"canonicalName":
            "%s"
                  ',
            self$`canonicalName`
          )
        },
        if (!is.null(self$`className`)) {
          sprintf(
            '"className":
            "%s"
                  ',
            self$`className`
          )
        },
        if (!is.null(self$`configurationId`)) {
          sprintf(
            '"configurationId":
            "%s"
                  ',
            self$`configurationId`
          )
        },
        if (!is.null(self$`localJarUrls`)) {
          sprintf(
            '"localJarUrls":
            [%s]
                  ',
            paste(unlist(lapply(self$`localJarUrls`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`removeConfig`)) {
          sprintf(
            '"removeConfig":
            %s
                  ',
            tolower(self$`removeConfig`)
          )
        },
        if (!is.null(self$`skipConfigVerification`)) {
          sprintf(
            '"skipConfigVerification":
            %s
                  ',
            tolower(self$`skipConfigVerification`)
          )
        },
        if (!is.null(self$`version`)) {
          sprintf(
            '"version":
            "%s"
                  ',
            self$`version`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param UpdateDriverRequestJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(UpdateDriverRequestJson, validateParams = FALSE) {
      UpdateDriverRequestObject <- jsonlite::fromJSON(UpdateDriverRequestJson)
      self$`baseNames` <- ApiClient$new()$deserializeObj(UpdateDriverRequestObject$`baseNames`, "array[character]", loadNamespace("datarobot.apicore"))
      self$`canonicalName` <- UpdateDriverRequestObject$`canonicalName`
      self$`className` <- UpdateDriverRequestObject$`className`
      self$`configurationId` <- UpdateDriverRequestObject$`configurationId`
      self$`localJarUrls` <- ApiClient$new()$deserializeObj(UpdateDriverRequestObject$`localJarUrls`, "array[character]", loadNamespace("datarobot.apicore"))
      self$`removeConfig` <- UpdateDriverRequestObject$`removeConfig`
      self$`skipConfigVerification` <- UpdateDriverRequestObject$`skipConfigVerification`
      self$`version` <- UpdateDriverRequestObject$`version`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
