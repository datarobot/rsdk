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
#' @title CombinedModelSegmentsResponse
#'
#' @description CombinedModelSegmentsResponse Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field autopilotDone  character Is autopilot done for the project.
#'
#' @field holdoutUnlocked  character Is holdout unlocked for the project.
#'
#' @field isFrozen  character Indicates whether the segment champion model is frozen, i.e. uses tuning parameters from a parent model
#'
#' @field modelAssignedBy  character Who assigned model as segment champion. Default is &#x60;&#x60;DataRobot&#x60;&#x60;.
#'
#' @field modelAwardTime  character Time when model was awarded as segment champion.
#'
#' @field modelCount  integer Count of trained models in project.
#'
#' @field modelIcon  list( integer ) The number for the icon representing the given champion model.
#'
#' @field modelId  character ID of segment champion model.
#'
#' @field modelMetrics  object The performance of the model according to various metrics, where each metric has validation, crossValidation, holdout, and training scores reported, or &#x60;&#x60;null&#x60;&#x60; if they have not been computed.
#'
#' @field modelType  character The description of the model type of the given champion model.
#'
#' @field projectId  character The ID of the project.
#'
#' @field projectPaused  character [optional] Is project paused right now.
#'
#' @field projectStage  character The current stage of the project, where modeling indicates that the target has been successfully set and modeling and predictions may proceed.
#'
#' @field projectStageDescription  character A description of the current stage of the project.
#'
#' @field projectStatusError  character [optional] Project status error message.
#'
#' @field rowCount  integer Count of rows in project&#39;s dataset.
#'
#' @field rowPercentage  numeric Percentage of rows in segment project&#39;s dataset comparing to original dataset.
#'
#' @field segment  character Segment name.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
CombinedModelSegmentsResponse <- R6::R6Class(
  "CombinedModelSegmentsResponse",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`autopilotDone` = NULL, `holdoutUnlocked` = NULL, `isFrozen` = NULL, `modelAssignedBy` = NULL, `modelAwardTime` = NULL, `modelCount` = NULL, `modelIcon` = NULL, `modelId` = NULL, `modelMetrics` = NULL, `modelType` = NULL, `projectId` = NULL, `projectPaused` = NULL, `projectStage` = NULL, `projectStageDescription` = NULL, `projectStatusError` = NULL, `rowCount` = NULL, `rowPercentage` = NULL, `segment` = NULL) {
      if (!is.null(`autopilotDone`)) {
        stopifnot(is.logical(`autopilotDone`), length(`autopilotDone`) == 1)
      }
      if (!is.null(`holdoutUnlocked`)) {
        stopifnot(is.logical(`holdoutUnlocked`), length(`holdoutUnlocked`) == 1)
      }
      if (!is.null(`isFrozen`)) {
        stopifnot(is.logical(`isFrozen`), length(`isFrozen`) == 1)
      }
      if (!is.null(`modelAssignedBy`)) {
        stopifnot(is.character(`modelAssignedBy`), length(`modelAssignedBy`) == 1)
      }
      if (!is.null(`modelAwardTime`)) {
        stopifnot(inherits(`modelAwardTime`, "POSIXt"))
      }
      if (!is.null(`modelCount`)) {
        stopifnot(is.numeric(`modelCount`), length(`modelCount`) == 1)
      }
      if (!is.null(`modelIcon`)) {
        stopifnot(is.vector(`modelIcon`), sapply(`modelIcon`, is.numeric))
      }
      if (!is.null(`modelId`)) {
        stopifnot(is.character(`modelId`), length(`modelId`) == 1)
      }
      if (!is.null(`modelMetrics`)) {
      }
      if (!is.null(`modelType`)) {
        stopifnot(is.character(`modelType`), length(`modelType`) == 1)
      }
      if (!is.null(`projectId`)) {
        stopifnot(is.character(`projectId`), length(`projectId`) == 1)
      }
      if (!is.null(`projectStage`)) {
        stopifnot(is.character(`projectStage`), length(`projectStage`) == 1)
      }
      if (!is.null(`projectStageDescription`)) {
        stopifnot(is.character(`projectStageDescription`), length(`projectStageDescription`) == 1)
      }
      if (!is.null(`rowCount`)) {
        stopifnot(is.numeric(`rowCount`), length(`rowCount`) == 1)
      }
      if (!is.null(`rowPercentage`)) {
      }
      if (!is.null(`segment`)) {
        stopifnot(is.character(`segment`), length(`segment`) == 1)
      }
      if (!is.null(`projectPaused`)) {
        stopifnot(is.logical(`projectPaused`), length(`projectPaused`) == 1)
      }
      if (!is.null(`projectStatusError`)) {
        stopifnot(is.character(`projectStatusError`), length(`projectStatusError`) == 1)
      }
    }
  ),
  public = list(
    `autopilotDone` = NULL,
    `holdoutUnlocked` = NULL,
    `isFrozen` = NULL,
    `modelAssignedBy` = NULL,
    `modelAwardTime` = NULL,
    `modelCount` = NULL,
    `modelIcon` = NULL,
    `modelId` = NULL,
    `modelMetrics` = NULL,
    `modelType` = NULL,
    `projectId` = NULL,
    `projectPaused` = NULL,
    `projectStage` = NULL,
    `projectStageDescription` = NULL,
    `projectStatusError` = NULL,
    `rowCount` = NULL,
    `rowPercentage` = NULL,
    `segment` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param autopilotDone Is autopilot done for the project.
    #' @param holdoutUnlocked Is holdout unlocked for the project.
    #' @param isFrozen Indicates whether the segment champion model is frozen, i.e. uses tuning parameters from a parent model
    #' @param modelAssignedBy Who assigned model as segment champion. Default is &#x60;&#x60;DataRobot&#x60;&#x60;.
    #' @param modelAwardTime Time when model was awarded as segment champion.
    #' @param modelCount Count of trained models in project.
    #' @param modelIcon The number for the icon representing the given champion model.
    #' @param modelId ID of segment champion model.
    #' @param modelMetrics The performance of the model according to various metrics, where each metric has validation, crossValidation, holdout, and training scores reported, or &#x60;&#x60;null&#x60;&#x60; if they have not been computed.
    #' @param modelType The description of the model type of the given champion model.
    #' @param projectId The ID of the project.
    #' @param projectPaused Is project paused right now.
    #' @param projectStage The current stage of the project, where modeling indicates that the target has been successfully set and modeling and predictions may proceed.
    #' @param projectStageDescription A description of the current stage of the project.
    #' @param projectStatusError Project status error message.
    #' @param rowCount Count of rows in project&#39;s dataset.
    #' @param rowPercentage Percentage of rows in segment project&#39;s dataset comparing to original dataset.
    #' @param segment Segment name.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`autopilotDone` = NULL, `holdoutUnlocked` = NULL, `isFrozen` = NULL, `modelAssignedBy` = NULL, `modelAwardTime` = NULL, `modelCount` = NULL, `modelIcon` = NULL, `modelId` = NULL, `modelMetrics` = NULL, `modelType` = NULL, `projectId` = NULL, `projectStage` = NULL, `projectStageDescription` = NULL, `rowCount` = NULL, `rowPercentage` = NULL, `segment` = NULL, `projectPaused` = NULL, `projectStatusError` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`autopilotDone`, `holdoutUnlocked`, `isFrozen`, `modelAssignedBy`, `modelAwardTime`, `modelCount`, `modelIcon`, `modelId`, `modelMetrics`, `modelType`, `projectId`, `projectStage`, `projectStageDescription`, `rowCount`, `rowPercentage`, `segment`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(autopilotDone, holdoutUnlocked, isFrozen, modelAssignedBy, modelAwardTime, modelCount, modelIcon, modelId, modelMetrics, modelType, projectId, projectPaused, projectStage, projectStageDescription, projectStatusError, rowCount, rowPercentage, segment)
      }
      self$`autopilotDone` <- `autopilotDone`
      self$`holdoutUnlocked` <- `holdoutUnlocked`
      self$`isFrozen` <- `isFrozen`
      self$`modelAssignedBy` <- `modelAssignedBy`
      self$`modelAwardTime` <- `modelAwardTime`
      self$`modelCount` <- `modelCount`
      self$`modelIcon` <- `modelIcon`
      self$`modelId` <- `modelId`
      self$`modelMetrics` <- `modelMetrics`
      self$`modelType` <- `modelType`
      self$`projectId` <- `projectId`
      self$`projectPaused` <- `projectPaused`
      self$`projectStage` <- `projectStage`
      self$`projectStageDescription` <- `projectStageDescription`
      self$`projectStatusError` <- `projectStatusError`
      self$`rowCount` <- `rowCount`
      self$`rowPercentage` <- `rowPercentage`
      self$`segment` <- `segment`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(autopilotDone = self$`autopilotDone`, holdoutUnlocked = self$`holdoutUnlocked`, isFrozen = self$`isFrozen`, modelAssignedBy = self$`modelAssignedBy`, modelAwardTime = self$`modelAwardTime`, modelCount = self$`modelCount`, modelIcon = self$`modelIcon`, modelId = self$`modelId`, modelMetrics = self$`modelMetrics`, modelType = self$`modelType`, projectId = self$`projectId`, projectPaused = self$`projectPaused`, projectStage = self$`projectStage`, projectStageDescription = self$`projectStageDescription`, projectStatusError = self$`projectStatusError`, rowCount = self$`rowCount`, rowPercentage = self$`rowPercentage`, segment = self$`segment`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`autopilotDone`)) {
          sprintf(
            '"autopilotDone":
            %s
                  ',
            tolower(self$`autopilotDone`)
          )
        },
        if (!is.null(self$`holdoutUnlocked`)) {
          sprintf(
            '"holdoutUnlocked":
            %s
                  ',
            tolower(self$`holdoutUnlocked`)
          )
        },
        if (!is.null(self$`isFrozen`)) {
          sprintf(
            '"isFrozen":
            %s
                  ',
            tolower(self$`isFrozen`)
          )
        },
        if (!is.null(self$`modelAssignedBy`)) {
          sprintf(
            '"modelAssignedBy":
            "%s"
                  ',
            self$`modelAssignedBy`
          )
        },
        if (!is.null(self$`modelAwardTime`)) {
          sprintf(
            '"modelAwardTime":
            "%s"
                  ',
            format(self$`modelAwardTime`, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
          )
        },
        if (!is.null(self$`modelCount`)) {
          sprintf(
            '"modelCount":
            %d
                  ',
            self$`modelCount`
          )
        },
        if (!is.null(self$`modelIcon`)) {
          sprintf(
            '"modelIcon":
            [%s]
                  ',
            paste(unlist(lapply(self$`modelIcon`, function(x) paste0('"', x, '"'))), collapse = ",")
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
        if (!is.null(self$`modelMetrics`)) {
          sprintf(
            '"modelMetrics":
            "%s"
                  ',
            self$`modelMetrics`
          )
        },
        if (!is.null(self$`modelType`)) {
          sprintf(
            '"modelType":
            "%s"
                  ',
            self$`modelType`
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
        if (!is.null(self$`projectPaused`)) {
          sprintf(
            '"projectPaused":
            %s
                  ',
            tolower(self$`projectPaused`)
          )
        },
        if (!is.null(self$`projectStage`)) {
          sprintf(
            '"projectStage":
            "%s"
                  ',
            self$`projectStage`
          )
        },
        if (!is.null(self$`projectStageDescription`)) {
          sprintf(
            '"projectStageDescription":
            "%s"
                  ',
            self$`projectStageDescription`
          )
        },
        if (!is.null(self$`projectStatusError`)) {
          sprintf(
            '"projectStatusError":
            "%s"
                  ',
            self$`projectStatusError`
          )
        },
        if (!is.null(self$`rowCount`)) {
          sprintf(
            '"rowCount":
            %d
                  ',
            self$`rowCount`
          )
        },
        if (!is.null(self$`rowPercentage`)) {
          sprintf(
            '"rowPercentage":
            %d
                  ',
            self$`rowPercentage`
          )
        },
        if (!is.null(self$`segment`)) {
          sprintf(
            '"segment":
            "%s"
                  ',
            self$`segment`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param CombinedModelSegmentsResponseJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(CombinedModelSegmentsResponseJson, validateParams = FALSE) {
      CombinedModelSegmentsResponseObject <- jsonlite::fromJSON(CombinedModelSegmentsResponseJson)
      self$`autopilotDone` <- CombinedModelSegmentsResponseObject$`autopilotDone`
      self$`holdoutUnlocked` <- CombinedModelSegmentsResponseObject$`holdoutUnlocked`
      self$`isFrozen` <- CombinedModelSegmentsResponseObject$`isFrozen`
      self$`modelAssignedBy` <- CombinedModelSegmentsResponseObject$`modelAssignedBy`
      self$`modelAwardTime` <- ParseRFC3339Timestamp(CombinedModelSegmentsResponseObject$`modelAwardTime`)
      self$`modelCount` <- CombinedModelSegmentsResponseObject$`modelCount`
      self$`modelIcon` <- ApiClient$new()$deserializeObj(CombinedModelSegmentsResponseObject$`modelIcon`, "array[integer]", loadNamespace("datarobot.apicore"))
      self$`modelId` <- CombinedModelSegmentsResponseObject$`modelId`
      self$`modelMetrics` <- CombinedModelSegmentsResponseObject$`modelMetrics`
      self$`modelType` <- CombinedModelSegmentsResponseObject$`modelType`
      self$`projectId` <- CombinedModelSegmentsResponseObject$`projectId`
      self$`projectPaused` <- CombinedModelSegmentsResponseObject$`projectPaused`
      self$`projectStage` <- CombinedModelSegmentsResponseObject$`projectStage`
      self$`projectStageDescription` <- CombinedModelSegmentsResponseObject$`projectStageDescription`
      self$`projectStatusError` <- CombinedModelSegmentsResponseObject$`projectStatusError`
      self$`rowCount` <- CombinedModelSegmentsResponseObject$`rowCount`
      self$`rowPercentage` <- CombinedModelSegmentsResponseObject$`rowPercentage`
      self$`segment` <- CombinedModelSegmentsResponseObject$`segment`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
