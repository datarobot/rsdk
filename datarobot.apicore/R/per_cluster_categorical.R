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
#' @title PerClusterCategorical
#'
#' @description PerClusterCategorical Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field allOther  numeric [optional] A percentage of rows that do not have any of these values or categories.
#'
#' @field clusterName  character Cluster name.
#'
#' @field missingRowsPercent  numeric [optional] A percentage of all rows that have a missing value for this feature.
#'
#' @field perValueStatistics  list( \link{PerValueStatisticsListItem} ) Statistic value for feature values in all data or a cluster.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
PerClusterCategorical <- R6::R6Class(
  "PerClusterCategorical",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`allOther` = NULL, `clusterName` = NULL, `missingRowsPercent` = NULL, `perValueStatistics` = NULL) {
      if (!is.null(`clusterName`)) {
        stopifnot(is.character(`clusterName`), length(`clusterName`) == 1)
      }
      if (!is.null(`perValueStatistics`)) {
        stopifnot(is.vector(`perValueStatistics`), sapply(`perValueStatistics`, R6::is.R6))
      }
      if (!is.null(`allOther`)) {
        stopifnot(is.numeric(`allOther`), length(`allOther`) == 1)
      }
      if (!is.null(`missingRowsPercent`)) {
        stopifnot(is.numeric(`missingRowsPercent`), length(`missingRowsPercent`) == 1)
      }
    }
  ),
  public = list(
    `allOther` = NULL,
    `clusterName` = NULL,
    `missingRowsPercent` = NULL,
    `perValueStatistics` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param allOther A percentage of rows that do not have any of these values or categories.
    #' @param clusterName Cluster name.
    #' @param missingRowsPercent A percentage of all rows that have a missing value for this feature.
    #' @param perValueStatistics Statistic value for feature values in all data or a cluster.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`clusterName` = NULL, `perValueStatistics` = NULL, `allOther` = NULL, `missingRowsPercent` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`clusterName`, `perValueStatistics`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(allOther, clusterName, missingRowsPercent, perValueStatistics)
      }
      self$`allOther` <- `allOther`
      self$`clusterName` <- `clusterName`
      self$`missingRowsPercent` <- `missingRowsPercent`
      self$`perValueStatistics` <- `perValueStatistics`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(allOther = self$`allOther`, clusterName = self$`clusterName`, missingRowsPercent = self$`missingRowsPercent`, perValueStatistics = self$`perValueStatistics`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`allOther`)) {
          sprintf(
            '"allOther":
            %d
                  ',
            self$`allOther`
          )
        },
        if (!is.null(self$`clusterName`)) {
          sprintf(
            '"clusterName":
            "%s"
                  ',
            self$`clusterName`
          )
        },
        if (!is.null(self$`missingRowsPercent`)) {
          sprintf(
            '"missingRowsPercent":
            %d
                  ',
            self$`missingRowsPercent`
          )
        },
        if (!is.null(self$`perValueStatistics`)) {
          sprintf(
            '"perValueStatistics":
            [%s]
      ',
            paste(sapply(self$`perValueStatistics`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param PerClusterCategoricalJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(PerClusterCategoricalJson, validateParams = FALSE) {
      PerClusterCategoricalObject <- jsonlite::fromJSON(PerClusterCategoricalJson)
      self$`allOther` <- PerClusterCategoricalObject$`allOther`
      self$`clusterName` <- PerClusterCategoricalObject$`clusterName`
      self$`missingRowsPercent` <- PerClusterCategoricalObject$`missingRowsPercent`
      self$`perValueStatistics` <- ApiClient$new()$deserializeObj(PerClusterCategoricalObject$`perValueStatistics`, "array[PerValueStatisticsListItem]", loadNamespace("datarobot.apicore"))

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
