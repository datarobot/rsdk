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
#' @title RatingTableModelDetailsResponse
#'
#' @description RatingTableModelDetailsResponse Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field blueprintId  character the blueprint used to construct the model
#'
#' @field featurelistId  character the ID of the featurelist used by the model
#'
#' @field featurelistName  character the name of the featurelist used by the model.If null, the model was trained on multiple feature lists.
#'
#' @field id  character the ID of the model
#'
#' @field isFrozen  character indicating whether the model is frozen, i.e. uses tuning parameters from a parent model
#'
#' @field isStarred  character whether the model has been starred
#'
#' @field linkFunction  character the link function the final modeler uses in the blueprint. If no link function exists, returns null
#'
#' @field metrics  object the performance of the model according to various metrics, where each metric has validation, crossValidation, holdout, and training scores reported, or null if they have not been computed.
#'
#' @field modelCategory  character indicates what kind of model it is - will be &#x60;prime&#x60; for DataRobot Prime models, &#x60;blend&#x60; for blender models, and &#x60;model&#x60; for all other models
#'
#' @field modelFamily  character the family model belongs to, e.g. SVM, GMB, etc.
#'
#' @field modelNumber  integer The model number from the leaderboard.
#'
#' @field modelType  character identifies the model, e.g. &#x60;Nystroem Kernel SVM Regressor&#x60;
#'
#' @field monotonicDecreasingFeaturelistId  character the ID of the featurelist that defines the set of features with a monotonically decreasing relationship to the target. If null, no such constraints are enforced.
#'
#' @field monotonicIncreasingFeaturelistId  character the ID of the featurelist that defines the set of features with a monotonically increasing relationship to the target. If null, no such constraints are enforced.
#'
#' @field parentModelId  character if this model is frozen, this is the ID of the parent model. Otherwise Null.
#'
#' @field predictionThreshold  numeric threshold used for binary classification in predictions.
#'
#' @field predictionThresholdReadOnly  character indicates whether modification of a predictions threshold is forbidden. Since v2.22 threshold modification is allowed.
#'
#' @field processes  list( character ) list of processes used by the model
#'
#' @field projectId  character the ID of the project to which the model belongs
#'
#' @field ratingTableId  character The rating table ID
#'
#' @field samplePct  numeric the percentage of the dataset used in training the model
#'
#' @field supportsComposableMl  character indicates whether this model is supported in Composable ML.
#'
#' @field supportsMonotonicConstraints  character whether this model supports enforcing monotonic constraints
#'
#' @field trainingDuration  character the duration spanned by the dates in the partition column for the data used to train the model
#'
#' @field trainingEndDate  character the end date of the dates in the partition column for the data used to train the model
#'
#' @field trainingRowCount  integer The number of rows used to train the model.
#'
#' @field trainingStartDate  character the start date of the dates in the partition column for the data used to train the model
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
RatingTableModelDetailsResponse <- R6::R6Class(
  "RatingTableModelDetailsResponse",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`blueprintId` = NULL, `featurelistId` = NULL, `featurelistName` = NULL, `id` = NULL, `isFrozen` = NULL, `isStarred` = NULL, `linkFunction` = NULL, `metrics` = NULL, `modelCategory` = NULL, `modelFamily` = NULL, `modelNumber` = NULL, `modelType` = NULL, `monotonicDecreasingFeaturelistId` = NULL, `monotonicIncreasingFeaturelistId` = NULL, `parentModelId` = NULL, `predictionThreshold` = NULL, `predictionThresholdReadOnly` = NULL, `processes` = NULL, `projectId` = NULL, `ratingTableId` = NULL, `samplePct` = NULL, `supportsComposableMl` = NULL, `supportsMonotonicConstraints` = NULL, `trainingDuration` = NULL, `trainingEndDate` = NULL, `trainingRowCount` = NULL, `trainingStartDate` = NULL) {
      if (!is.null(`blueprintId`)) {
        stopifnot(is.character(`blueprintId`), length(`blueprintId`) == 1)
      }
      if (!is.null(`featurelistId`)) {
        stopifnot(is.character(`featurelistId`), length(`featurelistId`) == 1)
      }
      if (!is.null(`featurelistName`)) {
        stopifnot(is.character(`featurelistName`), length(`featurelistName`) == 1)
      }
      if (!is.null(`id`)) {
        stopifnot(is.character(`id`), length(`id`) == 1)
      }
      if (!is.null(`isFrozen`)) {
        stopifnot(is.logical(`isFrozen`), length(`isFrozen`) == 1)
      }
      if (!is.null(`isStarred`)) {
        stopifnot(is.logical(`isStarred`), length(`isStarred`) == 1)
      }
      if (!is.null(`linkFunction`)) {
        stopifnot(is.character(`linkFunction`), length(`linkFunction`) == 1)
      }
      if (!is.null(`metrics`)) {
      }
      if (!is.null(`modelCategory`)) {
        stopifnot(is.character(`modelCategory`), length(`modelCategory`) == 1)
      }
      if (!is.null(`modelFamily`)) {
        stopifnot(is.character(`modelFamily`), length(`modelFamily`) == 1)
      }
      if (!is.null(`modelNumber`)) {
        stopifnot(is.numeric(`modelNumber`), length(`modelNumber`) == 1)
      }
      if (!is.null(`modelType`)) {
        stopifnot(is.character(`modelType`), length(`modelType`) == 1)
      }
      if (!is.null(`monotonicDecreasingFeaturelistId`)) {
        stopifnot(is.character(`monotonicDecreasingFeaturelistId`), length(`monotonicDecreasingFeaturelistId`) == 1)
      }
      if (!is.null(`monotonicIncreasingFeaturelistId`)) {
        stopifnot(is.character(`monotonicIncreasingFeaturelistId`), length(`monotonicIncreasingFeaturelistId`) == 1)
      }
      if (!is.null(`parentModelId`)) {
        stopifnot(is.character(`parentModelId`), length(`parentModelId`) == 1)
      }
      if (!is.null(`predictionThreshold`)) {
      }
      if (!is.null(`predictionThresholdReadOnly`)) {
        stopifnot(is.logical(`predictionThresholdReadOnly`), length(`predictionThresholdReadOnly`) == 1)
      }
      if (!is.null(`processes`)) {
        stopifnot(is.vector(`processes`), sapply(`processes`, is.character))
      }
      if (!is.null(`projectId`)) {
        stopifnot(is.character(`projectId`), length(`projectId`) == 1)
      }
      if (!is.null(`ratingTableId`)) {
        stopifnot(is.character(`ratingTableId`), length(`ratingTableId`) == 1)
      }
      if (!is.null(`samplePct`)) {
      }
      if (!is.null(`supportsComposableMl`)) {
        stopifnot(is.logical(`supportsComposableMl`), length(`supportsComposableMl`) == 1)
      }
      if (!is.null(`supportsMonotonicConstraints`)) {
        stopifnot(is.logical(`supportsMonotonicConstraints`), length(`supportsMonotonicConstraints`) == 1)
      }
      if (!is.null(`trainingDuration`)) {
        stopifnot(is.character(`trainingDuration`), length(`trainingDuration`) == 1)
      }
      if (!is.null(`trainingEndDate`)) {
        stopifnot(inherits(`trainingEndDate`, "POSIXt"))
      }
      if (!is.null(`trainingRowCount`)) {
        stopifnot(is.numeric(`trainingRowCount`), length(`trainingRowCount`) == 1)
      }
      if (!is.null(`trainingStartDate`)) {
        stopifnot(inherits(`trainingStartDate`, "POSIXt"))
      }
    }
  ),
  public = list(
    `blueprintId` = NULL,
    `featurelistId` = NULL,
    `featurelistName` = NULL,
    `id` = NULL,
    `isFrozen` = NULL,
    `isStarred` = NULL,
    `linkFunction` = NULL,
    `metrics` = NULL,
    `modelCategory` = NULL,
    `modelFamily` = NULL,
    `modelNumber` = NULL,
    `modelType` = NULL,
    `monotonicDecreasingFeaturelistId` = NULL,
    `monotonicIncreasingFeaturelistId` = NULL,
    `parentModelId` = NULL,
    `predictionThreshold` = NULL,
    `predictionThresholdReadOnly` = NULL,
    `processes` = NULL,
    `projectId` = NULL,
    `ratingTableId` = NULL,
    `samplePct` = NULL,
    `supportsComposableMl` = NULL,
    `supportsMonotonicConstraints` = NULL,
    `trainingDuration` = NULL,
    `trainingEndDate` = NULL,
    `trainingRowCount` = NULL,
    `trainingStartDate` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param blueprintId the blueprint used to construct the model
    #' @param featurelistId the ID of the featurelist used by the model
    #' @param featurelistName the name of the featurelist used by the model.If null, the model was trained on multiple feature lists.
    #' @param id the ID of the model
    #' @param isFrozen indicating whether the model is frozen, i.e. uses tuning parameters from a parent model
    #' @param isStarred whether the model has been starred
    #' @param linkFunction the link function the final modeler uses in the blueprint. If no link function exists, returns null
    #' @param metrics the performance of the model according to various metrics, where each metric has validation, crossValidation, holdout, and training scores reported, or null if they have not been computed.
    #' @param modelCategory indicates what kind of model it is - will be &#x60;prime&#x60; for DataRobot Prime models, &#x60;blend&#x60; for blender models, and &#x60;model&#x60; for all other models
    #' @param modelFamily the family model belongs to, e.g. SVM, GMB, etc.
    #' @param modelNumber The model number from the leaderboard.
    #' @param modelType identifies the model, e.g. &#x60;Nystroem Kernel SVM Regressor&#x60;
    #' @param monotonicDecreasingFeaturelistId the ID of the featurelist that defines the set of features with a monotonically decreasing relationship to the target. If null, no such constraints are enforced.
    #' @param monotonicIncreasingFeaturelistId the ID of the featurelist that defines the set of features with a monotonically increasing relationship to the target. If null, no such constraints are enforced.
    #' @param parentModelId if this model is frozen, this is the ID of the parent model. Otherwise Null.
    #' @param predictionThreshold threshold used for binary classification in predictions.
    #' @param predictionThresholdReadOnly indicates whether modification of a predictions threshold is forbidden. Since v2.22 threshold modification is allowed.
    #' @param processes list of processes used by the model
    #' @param projectId the ID of the project to which the model belongs
    #' @param ratingTableId The rating table ID
    #' @param samplePct the percentage of the dataset used in training the model
    #' @param supportsComposableMl indicates whether this model is supported in Composable ML.
    #' @param supportsMonotonicConstraints whether this model supports enforcing monotonic constraints
    #' @param trainingDuration the duration spanned by the dates in the partition column for the data used to train the model
    #' @param trainingEndDate the end date of the dates in the partition column for the data used to train the model
    #' @param trainingRowCount The number of rows used to train the model.
    #' @param trainingStartDate the start date of the dates in the partition column for the data used to train the model
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`blueprintId` = NULL, `featurelistId` = NULL, `featurelistName` = NULL, `id` = NULL, `isFrozen` = NULL, `isStarred` = NULL, `linkFunction` = NULL, `metrics` = NULL, `modelCategory` = NULL, `modelFamily` = NULL, `modelNumber` = NULL, `modelType` = NULL, `monotonicDecreasingFeaturelistId` = NULL, `monotonicIncreasingFeaturelistId` = NULL, `parentModelId` = NULL, `predictionThreshold` = NULL, `predictionThresholdReadOnly` = NULL, `processes` = NULL, `projectId` = NULL, `ratingTableId` = NULL, `samplePct` = NULL, `supportsComposableMl` = NULL, `supportsMonotonicConstraints` = NULL, `trainingDuration` = NULL, `trainingEndDate` = NULL, `trainingRowCount` = NULL, `trainingStartDate` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`blueprintId`, `featurelistId`, `featurelistName`, `id`, `isFrozen`, `isStarred`, `linkFunction`, `metrics`, `modelCategory`, `modelFamily`, `modelNumber`, `modelType`, `monotonicDecreasingFeaturelistId`, `monotonicIncreasingFeaturelistId`, `parentModelId`, `predictionThreshold`, `predictionThresholdReadOnly`, `processes`, `projectId`, `ratingTableId`, `samplePct`, `supportsComposableMl`, `supportsMonotonicConstraints`, `trainingDuration`, `trainingEndDate`, `trainingRowCount`, `trainingStartDate`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(blueprintId, featurelistId, featurelistName, id, isFrozen, isStarred, linkFunction, metrics, modelCategory, modelFamily, modelNumber, modelType, monotonicDecreasingFeaturelistId, monotonicIncreasingFeaturelistId, parentModelId, predictionThreshold, predictionThresholdReadOnly, processes, projectId, ratingTableId, samplePct, supportsComposableMl, supportsMonotonicConstraints, trainingDuration, trainingEndDate, trainingRowCount, trainingStartDate)
      }
      self$`blueprintId` <- `blueprintId`
      self$`featurelistId` <- `featurelistId`
      self$`featurelistName` <- `featurelistName`
      self$`id` <- `id`
      self$`isFrozen` <- `isFrozen`
      self$`isStarred` <- `isStarred`
      self$`linkFunction` <- `linkFunction`
      self$`metrics` <- `metrics`
      self$`modelCategory` <- `modelCategory`
      self$`modelFamily` <- `modelFamily`
      self$`modelNumber` <- `modelNumber`
      self$`modelType` <- `modelType`
      self$`monotonicDecreasingFeaturelistId` <- `monotonicDecreasingFeaturelistId`
      self$`monotonicIncreasingFeaturelistId` <- `monotonicIncreasingFeaturelistId`
      self$`parentModelId` <- `parentModelId`
      self$`predictionThreshold` <- `predictionThreshold`
      self$`predictionThresholdReadOnly` <- `predictionThresholdReadOnly`
      self$`processes` <- `processes`
      self$`projectId` <- `projectId`
      self$`ratingTableId` <- `ratingTableId`
      self$`samplePct` <- `samplePct`
      self$`supportsComposableMl` <- `supportsComposableMl`
      self$`supportsMonotonicConstraints` <- `supportsMonotonicConstraints`
      self$`trainingDuration` <- `trainingDuration`
      self$`trainingEndDate` <- `trainingEndDate`
      self$`trainingRowCount` <- `trainingRowCount`
      self$`trainingStartDate` <- `trainingStartDate`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(blueprintId = self$`blueprintId`, featurelistId = self$`featurelistId`, featurelistName = self$`featurelistName`, id = self$`id`, isFrozen = self$`isFrozen`, isStarred = self$`isStarred`, linkFunction = self$`linkFunction`, metrics = self$`metrics`, modelCategory = self$`modelCategory`, modelFamily = self$`modelFamily`, modelNumber = self$`modelNumber`, modelType = self$`modelType`, monotonicDecreasingFeaturelistId = self$`monotonicDecreasingFeaturelistId`, monotonicIncreasingFeaturelistId = self$`monotonicIncreasingFeaturelistId`, parentModelId = self$`parentModelId`, predictionThreshold = self$`predictionThreshold`, predictionThresholdReadOnly = self$`predictionThresholdReadOnly`, processes = self$`processes`, projectId = self$`projectId`, ratingTableId = self$`ratingTableId`, samplePct = self$`samplePct`, supportsComposableMl = self$`supportsComposableMl`, supportsMonotonicConstraints = self$`supportsMonotonicConstraints`, trainingDuration = self$`trainingDuration`, trainingEndDate = self$`trainingEndDate`, trainingRowCount = self$`trainingRowCount`, trainingStartDate = self$`trainingStartDate`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`blueprintId`)) {
          sprintf(
            '"blueprintId":
            "%s"
                  ',
            self$`blueprintId`
          )
        },
        if (!is.null(self$`featurelistId`)) {
          sprintf(
            '"featurelistId":
            "%s"
                  ',
            self$`featurelistId`
          )
        },
        if (!is.null(self$`featurelistName`)) {
          sprintf(
            '"featurelistName":
            "%s"
                  ',
            self$`featurelistName`
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
        if (!is.null(self$`isFrozen`)) {
          sprintf(
            '"isFrozen":
            %s
                  ',
            tolower(self$`isFrozen`)
          )
        },
        if (!is.null(self$`isStarred`)) {
          sprintf(
            '"isStarred":
            %s
                  ',
            tolower(self$`isStarred`)
          )
        },
        if (!is.null(self$`linkFunction`)) {
          sprintf(
            '"linkFunction":
            "%s"
                  ',
            self$`linkFunction`
          )
        },
        if (!is.null(self$`metrics`)) {
          sprintf(
            '"metrics":
            "%s"
                  ',
            self$`metrics`
          )
        },
        if (!is.null(self$`modelCategory`)) {
          sprintf(
            '"modelCategory":
            "%s"
                  ',
            self$`modelCategory`
          )
        },
        if (!is.null(self$`modelFamily`)) {
          sprintf(
            '"modelFamily":
            "%s"
                  ',
            self$`modelFamily`
          )
        },
        if (!is.null(self$`modelNumber`)) {
          sprintf(
            '"modelNumber":
            %d
                  ',
            self$`modelNumber`
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
        if (!is.null(self$`monotonicDecreasingFeaturelistId`)) {
          sprintf(
            '"monotonicDecreasingFeaturelistId":
            "%s"
                  ',
            self$`monotonicDecreasingFeaturelistId`
          )
        },
        if (!is.null(self$`monotonicIncreasingFeaturelistId`)) {
          sprintf(
            '"monotonicIncreasingFeaturelistId":
            "%s"
                  ',
            self$`monotonicIncreasingFeaturelistId`
          )
        },
        if (!is.null(self$`parentModelId`)) {
          sprintf(
            '"parentModelId":
            "%s"
                  ',
            self$`parentModelId`
          )
        },
        if (!is.null(self$`predictionThreshold`)) {
          sprintf(
            '"predictionThreshold":
            %d
                  ',
            self$`predictionThreshold`
          )
        },
        if (!is.null(self$`predictionThresholdReadOnly`)) {
          sprintf(
            '"predictionThresholdReadOnly":
            %s
                  ',
            tolower(self$`predictionThresholdReadOnly`)
          )
        },
        if (!is.null(self$`processes`)) {
          sprintf(
            '"processes":
            [%s]
                  ',
            paste(unlist(lapply(self$`processes`, function(x) paste0('"', x, '"'))), collapse = ",")
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
        if (!is.null(self$`ratingTableId`)) {
          sprintf(
            '"ratingTableId":
            "%s"
                  ',
            self$`ratingTableId`
          )
        },
        if (!is.null(self$`samplePct`)) {
          sprintf(
            '"samplePct":
            %d
                  ',
            self$`samplePct`
          )
        },
        if (!is.null(self$`supportsComposableMl`)) {
          sprintf(
            '"supportsComposableMl":
            %s
                  ',
            tolower(self$`supportsComposableMl`)
          )
        },
        if (!is.null(self$`supportsMonotonicConstraints`)) {
          sprintf(
            '"supportsMonotonicConstraints":
            %s
                  ',
            tolower(self$`supportsMonotonicConstraints`)
          )
        },
        if (!is.null(self$`trainingDuration`)) {
          sprintf(
            '"trainingDuration":
            "%s"
                  ',
            self$`trainingDuration`
          )
        },
        if (!is.null(self$`trainingEndDate`)) {
          sprintf(
            '"trainingEndDate":
            "%s"
                  ',
            format(self$`trainingEndDate`, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
          )
        },
        if (!is.null(self$`trainingRowCount`)) {
          sprintf(
            '"trainingRowCount":
            %d
                  ',
            self$`trainingRowCount`
          )
        },
        if (!is.null(self$`trainingStartDate`)) {
          sprintf(
            '"trainingStartDate":
            "%s"
                  ',
            format(self$`trainingStartDate`, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param RatingTableModelDetailsResponseJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(RatingTableModelDetailsResponseJson, validateParams = FALSE) {
      RatingTableModelDetailsResponseObject <- jsonlite::fromJSON(RatingTableModelDetailsResponseJson)
      self$`blueprintId` <- RatingTableModelDetailsResponseObject$`blueprintId`
      self$`featurelistId` <- RatingTableModelDetailsResponseObject$`featurelistId`
      self$`featurelistName` <- RatingTableModelDetailsResponseObject$`featurelistName`
      self$`id` <- RatingTableModelDetailsResponseObject$`id`
      self$`isFrozen` <- RatingTableModelDetailsResponseObject$`isFrozen`
      self$`isStarred` <- RatingTableModelDetailsResponseObject$`isStarred`
      self$`linkFunction` <- RatingTableModelDetailsResponseObject$`linkFunction`
      self$`metrics` <- RatingTableModelDetailsResponseObject$`metrics`
      self$`modelCategory` <- RatingTableModelDetailsResponseObject$`modelCategory`
      self$`modelFamily` <- RatingTableModelDetailsResponseObject$`modelFamily`
      self$`modelNumber` <- RatingTableModelDetailsResponseObject$`modelNumber`
      self$`modelType` <- RatingTableModelDetailsResponseObject$`modelType`
      self$`monotonicDecreasingFeaturelistId` <- RatingTableModelDetailsResponseObject$`monotonicDecreasingFeaturelistId`
      self$`monotonicIncreasingFeaturelistId` <- RatingTableModelDetailsResponseObject$`monotonicIncreasingFeaturelistId`
      self$`parentModelId` <- RatingTableModelDetailsResponseObject$`parentModelId`
      self$`predictionThreshold` <- RatingTableModelDetailsResponseObject$`predictionThreshold`
      self$`predictionThresholdReadOnly` <- RatingTableModelDetailsResponseObject$`predictionThresholdReadOnly`
      self$`processes` <- ApiClient$new()$deserializeObj(RatingTableModelDetailsResponseObject$`processes`, "array[character]", loadNamespace("datarobot.apicore"))
      self$`projectId` <- RatingTableModelDetailsResponseObject$`projectId`
      self$`ratingTableId` <- RatingTableModelDetailsResponseObject$`ratingTableId`
      self$`samplePct` <- RatingTableModelDetailsResponseObject$`samplePct`
      self$`supportsComposableMl` <- RatingTableModelDetailsResponseObject$`supportsComposableMl`
      self$`supportsMonotonicConstraints` <- RatingTableModelDetailsResponseObject$`supportsMonotonicConstraints`
      self$`trainingDuration` <- RatingTableModelDetailsResponseObject$`trainingDuration`
      self$`trainingEndDate` <- ParseRFC3339Timestamp(RatingTableModelDetailsResponseObject$`trainingEndDate`)
      self$`trainingRowCount` <- RatingTableModelDetailsResponseObject$`trainingRowCount`
      self$`trainingStartDate` <- ParseRFC3339Timestamp(RatingTableModelDetailsResponseObject$`trainingStartDate`)

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
