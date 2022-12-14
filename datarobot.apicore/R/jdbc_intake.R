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
#' @title JDBCIntake
#'
#' @description JDBCIntake Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field catalog  character [optional] The name of the specified database catalog to read input data from.
#'
#' @field credentialId  character [optional] The ID of the credential holding information about a user with read access to the JDBC data source.
#'
#' @field dataStoreId  character ID of the data store to connect to
#'
#' @field fetchSize  integer [optional] A user specified fetch size. Changing it can be used to balance throughput and memory usage. Deprecated and ignored since v2.21.
#'
#' @field query  character [optional] A self-supplied SELECT statement of the dataset you wish to score. Helpful for supplying a more fine-grained selection of data not achievable through specification of \&quot;table\&quot; and/or \&quot;schema\&quot; parameters exclusively.If this job is executed with a job definition, then template variables are available which will be substituted for timestamps: {{ current_run_timestamp }}, {{ last_completed_run_time }}, {{ last_scheduled_run_time }}, {{ next_scheduled_run_time }}, {{ current_run_time }}
#'
#' @field schema  character [optional] The name of the specified database schema to read input data from.
#'
#' @field table  character [optional] The name of the specified database table to read input data from.
#'
#' @field type  character Type name for this intake type
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
JDBCIntake <- R6::R6Class(
  "JDBCIntake",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`catalog` = NULL, `credentialId` = NULL, `dataStoreId` = NULL, `fetchSize` = NULL, `query` = NULL, `schema` = NULL, `table` = NULL, `type` = NULL) {
      if (!is.null(`dataStoreId`)) {
        stopifnot(is.character(`dataStoreId`), length(`dataStoreId`) == 1)
      }
      if (!is.null(`type`)) {
        stopifnot(is.character(`type`), length(`type`) == 1)
      }
      if (!is.null(`catalog`)) {
        stopifnot(is.character(`catalog`), length(`catalog`) == 1)
      }
      if (!is.null(`credentialId`)) {
        stopifnot(is.character(`credentialId`), length(`credentialId`) == 1)
      }
      if (!is.null(`fetchSize`)) {
        stopifnot(is.numeric(`fetchSize`), length(`fetchSize`) == 1)
      }
      if (!is.null(`query`)) {
        stopifnot(is.character(`query`), length(`query`) == 1)
      }
      if (!is.null(`schema`)) {
        stopifnot(is.character(`schema`), length(`schema`) == 1)
      }
      if (!is.null(`table`)) {
        stopifnot(is.character(`table`), length(`table`) == 1)
      }
    }
  ),
  public = list(
    `catalog` = NULL,
    `credentialId` = NULL,
    `dataStoreId` = NULL,
    `fetchSize` = NULL,
    `query` = NULL,
    `schema` = NULL,
    `table` = NULL,
    `type` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param catalog The name of the specified database catalog to read input data from.
    #' @param credentialId The ID of the credential holding information about a user with read access to the JDBC data source.
    #' @param dataStoreId ID of the data store to connect to
    #' @param fetchSize A user specified fetch size. Changing it can be used to balance throughput and memory usage. Deprecated and ignored since v2.21.
    #' @param query A self-supplied SELECT statement of the dataset you wish to score. Helpful for supplying a more fine-grained selection of data not achievable through specification of \&quot;table\&quot; and/or \&quot;schema\&quot; parameters exclusively.If this job is executed with a job definition, then template variables are available which will be substituted for timestamps: {{ current_run_timestamp }}, {{ last_completed_run_time }}, {{ last_scheduled_run_time }}, {{ next_scheduled_run_time }}, {{ current_run_time }}
    #' @param schema The name of the specified database schema to read input data from.
    #' @param table The name of the specified database table to read input data from.
    #' @param type Type name for this intake type
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`dataStoreId` = NULL, `type` = NULL, `catalog` = NULL, `credentialId` = NULL, `fetchSize` = NULL, `query` = NULL, `schema` = NULL, `table` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`dataStoreId`, `type`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(catalog, credentialId, dataStoreId, fetchSize, query, schema, table, type)
      }
      self$`catalog` <- `catalog`
      self$`credentialId` <- `credentialId`
      self$`dataStoreId` <- `dataStoreId`
      self$`fetchSize` <- `fetchSize`
      self$`query` <- `query`
      self$`schema` <- `schema`
      self$`table` <- `table`
      self$`type` <- `type`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(catalog = self$`catalog`, credentialId = self$`credentialId`, dataStoreId = self$`dataStoreId`, fetchSize = self$`fetchSize`, query = self$`query`, schema = self$`schema`, table = self$`table`, type = self$`type`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`catalog`)) {
          sprintf(
            '"catalog":
            "%s"
                  ',
            self$`catalog`
          )
        },
        if (!is.null(self$`credentialId`)) {
          sprintf(
            '"credentialId":
            "%s"
                  ',
            self$`credentialId`
          )
        },
        if (!is.null(self$`dataStoreId`)) {
          sprintf(
            '"dataStoreId":
            "%s"
                  ',
            self$`dataStoreId`
          )
        },
        if (!is.null(self$`fetchSize`)) {
          sprintf(
            '"fetchSize":
            %d
                  ',
            self$`fetchSize`
          )
        },
        if (!is.null(self$`query`)) {
          sprintf(
            '"query":
            "%s"
                  ',
            self$`query`
          )
        },
        if (!is.null(self$`schema`)) {
          sprintf(
            '"schema":
            "%s"
                  ',
            self$`schema`
          )
        },
        if (!is.null(self$`table`)) {
          sprintf(
            '"table":
            "%s"
                  ',
            self$`table`
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
    #' @param JDBCIntakeJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(JDBCIntakeJson, validateParams = FALSE) {
      JDBCIntakeObject <- jsonlite::fromJSON(JDBCIntakeJson)
      self$`catalog` <- JDBCIntakeObject$`catalog`
      self$`credentialId` <- JDBCIntakeObject$`credentialId`
      self$`dataStoreId` <- JDBCIntakeObject$`dataStoreId`
      self$`fetchSize` <- JDBCIntakeObject$`fetchSize`
      self$`query` <- JDBCIntakeObject$`query`
      self$`schema` <- JDBCIntakeObject$`schema`
      self$`table` <- JDBCIntakeObject$`table`
      self$`type` <- JDBCIntakeObject$`type`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
