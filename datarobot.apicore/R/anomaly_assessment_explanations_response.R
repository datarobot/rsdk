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
#' @title AnomalyAssessmentExplanationsResponse
#'
#' @description AnomalyAssessmentExplanationsResponse Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field backtest  \link{OneOfintegerstring} The backtest of the record.
#'
#' @field count  integer The count of points.
#'
#' @field data  list( \link{DataPointResponse} ) Each is a &#x60;DataPoint&#x60; corresponding to a row in the specified range.
#'
#' @field endDate  character ISO-formatted last timestamp in the response. For example: &#x60;&#x60;2019-08-30T00:00:00.000000Z&#x60;&#x60;.
#'
#' @field modelId  character The model ID of the record.
#'
#' @field projectId  character The project ID of the record.
#'
#' @field recordId  character The ID of the anomaly assessment record.
#'
#' @field seriesId  character The series id of the record. Applicable in multiseries projects
#'
#' @field shapBaseValue  numeric shap base value
#'
#' @field source  character The source of the record
#'
#' @field startDate  character ISO-formatted first timestamp in the response. For example: &#x60;&#x60;2019-08-01T00:00:00.000000Z&#x60;&#x60;.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
AnomalyAssessmentExplanationsResponse <- R6::R6Class(
  "AnomalyAssessmentExplanationsResponse",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`backtest` = NULL, `count` = NULL, `data` = NULL, `endDate` = NULL, `modelId` = NULL, `projectId` = NULL, `recordId` = NULL, `seriesId` = NULL, `shapBaseValue` = NULL, `source` = NULL, `startDate` = NULL) {
      if (!is.null(`backtest`)) {
        .setPrimitiveProperty(typeList = list("numeric", "character"), propertyData = backtest)
      }
      if (!is.null(`count`)) {
        stopifnot(is.numeric(`count`), length(`count`) == 1)
      }
      if (!is.null(`data`)) {
        stopifnot(is.vector(`data`), sapply(`data`, R6::is.R6))
      }
      if (!is.null(`endDate`)) {
        stopifnot(inherits(`endDate`, "POSIXt"))
      }
      if (!is.null(`modelId`)) {
        stopifnot(is.character(`modelId`), length(`modelId`) == 1)
      }
      if (!is.null(`projectId`)) {
        stopifnot(is.character(`projectId`), length(`projectId`) == 1)
      }
      if (!is.null(`recordId`)) {
        stopifnot(is.character(`recordId`), length(`recordId`) == 1)
      }
      if (!is.null(`seriesId`)) {
        stopifnot(is.character(`seriesId`), length(`seriesId`) == 1)
      }
      if (!is.null(`shapBaseValue`)) {
      }
      if (!is.null(`source`)) {
        stopifnot(is.character(`source`), length(`source`) == 1)
      }
      if (!is.null(`startDate`)) {
        stopifnot(inherits(`startDate`, "POSIXt"))
      }
    }
  ),
  public = list(
    `backtest` = NULL,
    `count` = NULL,
    `data` = NULL,
    `endDate` = NULL,
    `modelId` = NULL,
    `projectId` = NULL,
    `recordId` = NULL,
    `seriesId` = NULL,
    `shapBaseValue` = NULL,
    `source` = NULL,
    `startDate` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param backtest The backtest of the record.
    #' @param count The count of points.
    #' @param data Each is a &#x60;DataPoint&#x60; corresponding to a row in the specified range.
    #' @param endDate ISO-formatted last timestamp in the response. For example: &#x60;&#x60;2019-08-30T00:00:00.000000Z&#x60;&#x60;.
    #' @param modelId The model ID of the record.
    #' @param projectId The project ID of the record.
    #' @param recordId The ID of the anomaly assessment record.
    #' @param seriesId The series id of the record. Applicable in multiseries projects
    #' @param shapBaseValue shap base value
    #' @param source The source of the record
    #' @param startDate ISO-formatted first timestamp in the response. For example: &#x60;&#x60;2019-08-01T00:00:00.000000Z&#x60;&#x60;.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`backtest` = NULL, `count` = NULL, `data` = NULL, `endDate` = NULL, `modelId` = NULL, `projectId` = NULL, `recordId` = NULL, `seriesId` = NULL, `shapBaseValue` = NULL, `source` = NULL, `startDate` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`backtest`, `count`, `data`, `endDate`, `modelId`, `projectId`, `recordId`, `seriesId`, `shapBaseValue`, `source`, `startDate`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(backtest, count, data, endDate, modelId, projectId, recordId, seriesId, shapBaseValue, source, startDate)
      }
      self$`backtest` <- .setPrimitiveProperty(typeList = list("numeric", "character"), propertyData = backtest)
      self$`count` <- `count`
      self$`data` <- `data`
      self$`endDate` <- `endDate`
      self$`modelId` <- `modelId`
      self$`projectId` <- `projectId`
      self$`recordId` <- `recordId`
      self$`seriesId` <- `seriesId`
      self$`shapBaseValue` <- `shapBaseValue`
      self$`source` <- `source`
      self$`startDate` <- `startDate`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(backtest = self$`backtest`, count = self$`count`, data = self$`data`, endDate = self$`endDate`, modelId = self$`modelId`, projectId = self$`projectId`, recordId = self$`recordId`, seriesId = self$`seriesId`, shapBaseValue = self$`shapBaseValue`, source = self$`source`, startDate = self$`startDate`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`backtest`)) {
          sprintf(
            '"backtest":
            %s
      ',
            self$`backtest`
          )
        },
        if (!is.null(self$`count`)) {
          sprintf(
            '"count":
            %d
                  ',
            self$`count`
          )
        },
        if (!is.null(self$`data`)) {
          sprintf(
            '"data":
            [%s]
      ',
            paste(sapply(self$`data`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`endDate`)) {
          sprintf(
            '"endDate":
            "%s"
                  ',
            format(self$`endDate`, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
          )
        },
        if (!is.null(self$`modelId`)) {
          sprintf(
            '"modelId":
            "%s"
                  ',
            self$`modelId`
          )
        },
        if (!is.null(self$`projectId`)) {
          sprintf(
            '"projectId":
            "%s"
                  ',
            self$`projectId`
          )
        },
        if (!is.null(self$`recordId`)) {
          sprintf(
            '"recordId":
            "%s"
                  ',
            self$`recordId`
          )
        },
        if (!is.null(self$`seriesId`)) {
          sprintf(
            '"seriesId":
            "%s"
                  ',
            self$`seriesId`
          )
        },
        if (!is.null(self$`shapBaseValue`)) {
          sprintf(
            '"shapBaseValue":
            %d
                  ',
            self$`shapBaseValue`
          )
        },
        if (!is.null(self$`source`)) {
          sprintf(
            '"source":
            "%s"
                  ',
            self$`source`
          )
        },
        if (!is.null(self$`startDate`)) {
          sprintf(
            '"startDate":
            "%s"
                  ',
            format(self$`startDate`, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param AnomalyAssessmentExplanationsResponseJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(AnomalyAssessmentExplanationsResponseJson, validateParams = FALSE) {
      AnomalyAssessmentExplanationsResponseObject <- jsonlite::fromJSON(AnomalyAssessmentExplanationsResponseJson)
      self$`backtest` <- .setPrimitiveProperty(typeList = list("numeric", "character"), propertyData = AnomalyAssessmentExplanationsResponseObject$backtest)
      self$`count` <- AnomalyAssessmentExplanationsResponseObject$`count`
      self$`data` <- ApiClient$new()$deserializeObj(AnomalyAssessmentExplanationsResponseObject$`data`, "array[DataPointResponse]", loadNamespace("datarobot.apicore"))
      self$`endDate` <- ParseRFC3339Timestamp(AnomalyAssessmentExplanationsResponseObject$`endDate`)
      self$`modelId` <- AnomalyAssessmentExplanationsResponseObject$`modelId`
      self$`projectId` <- AnomalyAssessmentExplanationsResponseObject$`projectId`
      self$`recordId` <- AnomalyAssessmentExplanationsResponseObject$`recordId`
      self$`seriesId` <- AnomalyAssessmentExplanationsResponseObject$`seriesId`
      self$`shapBaseValue` <- AnomalyAssessmentExplanationsResponseObject$`shapBaseValue`
      self$`source` <- AnomalyAssessmentExplanationsResponseObject$`source`
      self$`startDate` <- ParseRFC3339Timestamp(AnomalyAssessmentExplanationsResponseObject$`startDate`)

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
