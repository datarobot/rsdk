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
#' @title DataSourceCreate
#'
#' @description DataSourceCreate Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field canonicalName  character Data source canonical name.
#'
#' @field params  \link{OneOfJDBCTableDataSourceJDBCQueryDataSourceDRConnectorV1DataSource} Data source configuration.
#'
#' @field type  character Data source type.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
DataSourceCreate <- R6::R6Class(
  "DataSourceCreate",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`canonicalName` = NULL, `params` = NULL, `type` = NULL) {
      if (!is.null(`canonicalName`)) {
        stopifnot(is.character(`canonicalName`), length(`canonicalName`) == 1)
      }
      if (!is.null(`params`)) {
        .setComplexProperty(typeList = list(JDBCTableDataSource, JDBCQueryDataSource, DRConnectorV1DataSource), propertyData = params)
      }
      if (!is.null(`type`)) {
        stopifnot(is.character(`type`), length(`type`) == 1)
      }
    }
  ),
  public = list(
    `canonicalName` = NULL,
    `params` = NULL,
    `type` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param canonicalName Data source canonical name.
    #' @param params Data source configuration.
    #' @param type Data source type.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`canonicalName` = NULL, `params` = NULL, `type` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`canonicalName`, `params`, `type`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(canonicalName, params, type)
      }
      self$`canonicalName` <- `canonicalName`
      self$`params` <- .setComplexProperty(typeList = list(JDBCTableDataSource, JDBCQueryDataSource, DRConnectorV1DataSource), propertyData = params)
      self$`type` <- `type`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(canonicalName = self$`canonicalName`, params = self$`params`, type = self$`type`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`canonicalName`)) {
          sprintf(
            '"canonicalName":
            "%s"
                  ',
            self$`canonicalName`
          )
        },
        if (!is.null(self$`params`)) {
          sprintf(
            '"params":
            %s
      ',
            jsonlite::toJSON(self$`params`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`type`)) {
          sprintf(
            '"type":
            "%s"
                  ',
            self$`type`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param DataSourceCreateJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(DataSourceCreateJson, validateParams = FALSE) {
      DataSourceCreateObject <- jsonlite::fromJSON(DataSourceCreateJson)
      self$`canonicalName` <- DataSourceCreateObject$`canonicalName`
      self$`params` <- .setComplexProperty(typeList = list(JDBCTableDataSource, JDBCQueryDataSource, DRConnectorV1DataSource), propertyData = DataSourceCreateObject$params)
      self$`type` <- DataSourceCreateObject$`type`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
