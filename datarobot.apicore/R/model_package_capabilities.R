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
#' @title ModelPackageCapabilities
#'
#' @description ModelPackageCapabilities Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field supportsAutomaticActuals  character [optional] Whether inferring actual values from time series history data and automatically feeding them back for accuracy estimation is supported by this model package.
#'
#' @field supportsChallengerModels  character Whether Challenger Models are supported by this model package.
#'
#' @field supportsFeatureDriftTracking  character Whether Feature Drift is supported by this model package.
#'
#' @field supportsHumilityRecommendedRules  character Whether calculating values for recommended Humility Rules is supported by this model package.
#'
#' @field supportsHumilityRules  character Whether Humility Rules are supported by this model package.
#'
#' @field supportsHumilityRulesDefaultCalculations  character Whether calculating default values for Humility Rules is supported by this model package.
#'
#' @field supportsPredictionWarning  character Whether Prediction Warnings are supported by this model package.
#'
#' @field supportsRetraining  character [optional] Whether deployment supports retraining.
#'
#' @field supportsScoringCodeDownload  character [optional] Whether scoring code download is supported by this model package.
#'
#' @field supportsSecondaryDatasets  character If the deployments supports secondary datasets.
#'
#' @field supportsSegmentedAnalysisDriftAndAccuracy  character Whether tracking features in training and predictions data for segmented analysis is supported by this model package.
#'
#' @field supportsShapBasedPredictionExplanations  character Whether shap-based prediction explanations are supported by this model package.
#'
#' @field supportsTargetDriftTracking  character Whether Target Drift is supported by this model package.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
ModelPackageCapabilities <- R6::R6Class(
  "ModelPackageCapabilities",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`supportsAutomaticActuals` = NULL, `supportsChallengerModels` = NULL, `supportsFeatureDriftTracking` = NULL, `supportsHumilityRecommendedRules` = NULL, `supportsHumilityRules` = NULL, `supportsHumilityRulesDefaultCalculations` = NULL, `supportsPredictionWarning` = NULL, `supportsRetraining` = NULL, `supportsScoringCodeDownload` = NULL, `supportsSecondaryDatasets` = NULL, `supportsSegmentedAnalysisDriftAndAccuracy` = NULL, `supportsShapBasedPredictionExplanations` = NULL, `supportsTargetDriftTracking` = NULL) {
      if (!is.null(`supportsChallengerModels`)) {
        stopifnot(is.logical(`supportsChallengerModels`), length(`supportsChallengerModels`) == 1)
      }
      if (!is.null(`supportsFeatureDriftTracking`)) {
        stopifnot(is.logical(`supportsFeatureDriftTracking`), length(`supportsFeatureDriftTracking`) == 1)
      }
      if (!is.null(`supportsHumilityRecommendedRules`)) {
        stopifnot(is.logical(`supportsHumilityRecommendedRules`), length(`supportsHumilityRecommendedRules`) == 1)
      }
      if (!is.null(`supportsHumilityRules`)) {
        stopifnot(is.logical(`supportsHumilityRules`), length(`supportsHumilityRules`) == 1)
      }
      if (!is.null(`supportsHumilityRulesDefaultCalculations`)) {
        stopifnot(is.logical(`supportsHumilityRulesDefaultCalculations`), length(`supportsHumilityRulesDefaultCalculations`) == 1)
      }
      if (!is.null(`supportsPredictionWarning`)) {
        stopifnot(is.logical(`supportsPredictionWarning`), length(`supportsPredictionWarning`) == 1)
      }
      if (!is.null(`supportsSecondaryDatasets`)) {
        stopifnot(is.logical(`supportsSecondaryDatasets`), length(`supportsSecondaryDatasets`) == 1)
      }
      if (!is.null(`supportsSegmentedAnalysisDriftAndAccuracy`)) {
        stopifnot(is.logical(`supportsSegmentedAnalysisDriftAndAccuracy`), length(`supportsSegmentedAnalysisDriftAndAccuracy`) == 1)
      }
      if (!is.null(`supportsShapBasedPredictionExplanations`)) {
        stopifnot(is.logical(`supportsShapBasedPredictionExplanations`), length(`supportsShapBasedPredictionExplanations`) == 1)
      }
      if (!is.null(`supportsTargetDriftTracking`)) {
        stopifnot(is.logical(`supportsTargetDriftTracking`), length(`supportsTargetDriftTracking`) == 1)
      }
      if (!is.null(`supportsAutomaticActuals`)) {
        stopifnot(is.logical(`supportsAutomaticActuals`), length(`supportsAutomaticActuals`) == 1)
      }
      if (!is.null(`supportsRetraining`)) {
        stopifnot(is.logical(`supportsRetraining`), length(`supportsRetraining`) == 1)
      }
      if (!is.null(`supportsScoringCodeDownload`)) {
        stopifnot(is.logical(`supportsScoringCodeDownload`), length(`supportsScoringCodeDownload`) == 1)
      }
    }
  ),
  public = list(
    `supportsAutomaticActuals` = NULL,
    `supportsChallengerModels` = NULL,
    `supportsFeatureDriftTracking` = NULL,
    `supportsHumilityRecommendedRules` = NULL,
    `supportsHumilityRules` = NULL,
    `supportsHumilityRulesDefaultCalculations` = NULL,
    `supportsPredictionWarning` = NULL,
    `supportsRetraining` = NULL,
    `supportsScoringCodeDownload` = NULL,
    `supportsSecondaryDatasets` = NULL,
    `supportsSegmentedAnalysisDriftAndAccuracy` = NULL,
    `supportsShapBasedPredictionExplanations` = NULL,
    `supportsTargetDriftTracking` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param supportsAutomaticActuals Whether inferring actual values from time series history data and automatically feeding them back for accuracy estimation is supported by this model package.
    #' @param supportsChallengerModels Whether Challenger Models are supported by this model package.
    #' @param supportsFeatureDriftTracking Whether Feature Drift is supported by this model package.
    #' @param supportsHumilityRecommendedRules Whether calculating values for recommended Humility Rules is supported by this model package.
    #' @param supportsHumilityRules Whether Humility Rules are supported by this model package.
    #' @param supportsHumilityRulesDefaultCalculations Whether calculating default values for Humility Rules is supported by this model package.
    #' @param supportsPredictionWarning Whether Prediction Warnings are supported by this model package.
    #' @param supportsRetraining Whether deployment supports retraining.
    #' @param supportsScoringCodeDownload Whether scoring code download is supported by this model package.
    #' @param supportsSecondaryDatasets If the deployments supports secondary datasets.
    #' @param supportsSegmentedAnalysisDriftAndAccuracy Whether tracking features in training and predictions data for segmented analysis is supported by this model package.
    #' @param supportsShapBasedPredictionExplanations Whether shap-based prediction explanations are supported by this model package.
    #' @param supportsTargetDriftTracking Whether Target Drift is supported by this model package.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`supportsChallengerModels` = NULL, `supportsFeatureDriftTracking` = NULL, `supportsHumilityRecommendedRules` = NULL, `supportsHumilityRules` = NULL, `supportsHumilityRulesDefaultCalculations` = NULL, `supportsPredictionWarning` = NULL, `supportsSecondaryDatasets` = NULL, `supportsSegmentedAnalysisDriftAndAccuracy` = NULL, `supportsShapBasedPredictionExplanations` = NULL, `supportsTargetDriftTracking` = NULL, `supportsAutomaticActuals` = NULL, `supportsRetraining` = NULL, `supportsScoringCodeDownload` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`supportsChallengerModels`, `supportsFeatureDriftTracking`, `supportsHumilityRecommendedRules`, `supportsHumilityRules`, `supportsHumilityRulesDefaultCalculations`, `supportsPredictionWarning`, `supportsSecondaryDatasets`, `supportsSegmentedAnalysisDriftAndAccuracy`, `supportsShapBasedPredictionExplanations`, `supportsTargetDriftTracking`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(supportsAutomaticActuals, supportsChallengerModels, supportsFeatureDriftTracking, supportsHumilityRecommendedRules, supportsHumilityRules, supportsHumilityRulesDefaultCalculations, supportsPredictionWarning, supportsRetraining, supportsScoringCodeDownload, supportsSecondaryDatasets, supportsSegmentedAnalysisDriftAndAccuracy, supportsShapBasedPredictionExplanations, supportsTargetDriftTracking)
      }
      self$`supportsAutomaticActuals` <- `supportsAutomaticActuals`
      self$`supportsChallengerModels` <- `supportsChallengerModels`
      self$`supportsFeatureDriftTracking` <- `supportsFeatureDriftTracking`
      self$`supportsHumilityRecommendedRules` <- `supportsHumilityRecommendedRules`
      self$`supportsHumilityRules` <- `supportsHumilityRules`
      self$`supportsHumilityRulesDefaultCalculations` <- `supportsHumilityRulesDefaultCalculations`
      self$`supportsPredictionWarning` <- `supportsPredictionWarning`
      self$`supportsRetraining` <- `supportsRetraining`
      self$`supportsScoringCodeDownload` <- `supportsScoringCodeDownload`
      self$`supportsSecondaryDatasets` <- `supportsSecondaryDatasets`
      self$`supportsSegmentedAnalysisDriftAndAccuracy` <- `supportsSegmentedAnalysisDriftAndAccuracy`
      self$`supportsShapBasedPredictionExplanations` <- `supportsShapBasedPredictionExplanations`
      self$`supportsTargetDriftTracking` <- `supportsTargetDriftTracking`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(supportsAutomaticActuals = self$`supportsAutomaticActuals`, supportsChallengerModels = self$`supportsChallengerModels`, supportsFeatureDriftTracking = self$`supportsFeatureDriftTracking`, supportsHumilityRecommendedRules = self$`supportsHumilityRecommendedRules`, supportsHumilityRules = self$`supportsHumilityRules`, supportsHumilityRulesDefaultCalculations = self$`supportsHumilityRulesDefaultCalculations`, supportsPredictionWarning = self$`supportsPredictionWarning`, supportsRetraining = self$`supportsRetraining`, supportsScoringCodeDownload = self$`supportsScoringCodeDownload`, supportsSecondaryDatasets = self$`supportsSecondaryDatasets`, supportsSegmentedAnalysisDriftAndAccuracy = self$`supportsSegmentedAnalysisDriftAndAccuracy`, supportsShapBasedPredictionExplanations = self$`supportsShapBasedPredictionExplanations`, supportsTargetDriftTracking = self$`supportsTargetDriftTracking`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`supportsAutomaticActuals`)) {
          sprintf(
            '"supportsAutomaticActuals":
            %s
                  ',
            tolower(self$`supportsAutomaticActuals`)
          )
        },
        if (!is.null(self$`supportsChallengerModels`)) {
          sprintf(
            '"supportsChallengerModels":
            %s
                  ',
            tolower(self$`supportsChallengerModels`)
          )
        },
        if (!is.null(self$`supportsFeatureDriftTracking`)) {
          sprintf(
            '"supportsFeatureDriftTracking":
            %s
                  ',
            tolower(self$`supportsFeatureDriftTracking`)
          )
        },
        if (!is.null(self$`supportsHumilityRecommendedRules`)) {
          sprintf(
            '"supportsHumilityRecommendedRules":
            %s
                  ',
            tolower(self$`supportsHumilityRecommendedRules`)
          )
        },
        if (!is.null(self$`supportsHumilityRules`)) {
          sprintf(
            '"supportsHumilityRules":
            %s
                  ',
            tolower(self$`supportsHumilityRules`)
          )
        },
        if (!is.null(self$`supportsHumilityRulesDefaultCalculations`)) {
          sprintf(
            '"supportsHumilityRulesDefaultCalculations":
            %s
                  ',
            tolower(self$`supportsHumilityRulesDefaultCalculations`)
          )
        },
        if (!is.null(self$`supportsPredictionWarning`)) {
          sprintf(
            '"supportsPredictionWarning":
            %s
                  ',
            tolower(self$`supportsPredictionWarning`)
          )
        },
        if (!is.null(self$`supportsRetraining`)) {
          sprintf(
            '"supportsRetraining":
            %s
                  ',
            tolower(self$`supportsRetraining`)
          )
        },
        if (!is.null(self$`supportsScoringCodeDownload`)) {
          sprintf(
            '"supportsScoringCodeDownload":
            %s
                  ',
            tolower(self$`supportsScoringCodeDownload`)
          )
        },
        if (!is.null(self$`supportsSecondaryDatasets`)) {
          sprintf(
            '"supportsSecondaryDatasets":
            %s
                  ',
            tolower(self$`supportsSecondaryDatasets`)
          )
        },
        if (!is.null(self$`supportsSegmentedAnalysisDriftAndAccuracy`)) {
          sprintf(
            '"supportsSegmentedAnalysisDriftAndAccuracy":
            %s
                  ',
            tolower(self$`supportsSegmentedAnalysisDriftAndAccuracy`)
          )
        },
        if (!is.null(self$`supportsShapBasedPredictionExplanations`)) {
          sprintf(
            '"supportsShapBasedPredictionExplanations":
            %s
                  ',
            tolower(self$`supportsShapBasedPredictionExplanations`)
          )
        },
        if (!is.null(self$`supportsTargetDriftTracking`)) {
          sprintf(
            '"supportsTargetDriftTracking":
            %s
                  ',
            tolower(self$`supportsTargetDriftTracking`)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param ModelPackageCapabilitiesJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(ModelPackageCapabilitiesJson, validateParams = FALSE) {
      ModelPackageCapabilitiesObject <- jsonlite::fromJSON(ModelPackageCapabilitiesJson)
      self$`supportsAutomaticActuals` <- ModelPackageCapabilitiesObject$`supportsAutomaticActuals`
      self$`supportsChallengerModels` <- ModelPackageCapabilitiesObject$`supportsChallengerModels`
      self$`supportsFeatureDriftTracking` <- ModelPackageCapabilitiesObject$`supportsFeatureDriftTracking`
      self$`supportsHumilityRecommendedRules` <- ModelPackageCapabilitiesObject$`supportsHumilityRecommendedRules`
      self$`supportsHumilityRules` <- ModelPackageCapabilitiesObject$`supportsHumilityRules`
      self$`supportsHumilityRulesDefaultCalculations` <- ModelPackageCapabilitiesObject$`supportsHumilityRulesDefaultCalculations`
      self$`supportsPredictionWarning` <- ModelPackageCapabilitiesObject$`supportsPredictionWarning`
      self$`supportsRetraining` <- ModelPackageCapabilitiesObject$`supportsRetraining`
      self$`supportsScoringCodeDownload` <- ModelPackageCapabilitiesObject$`supportsScoringCodeDownload`
      self$`supportsSecondaryDatasets` <- ModelPackageCapabilitiesObject$`supportsSecondaryDatasets`
      self$`supportsSegmentedAnalysisDriftAndAccuracy` <- ModelPackageCapabilitiesObject$`supportsSegmentedAnalysisDriftAndAccuracy`
      self$`supportsShapBasedPredictionExplanations` <- ModelPackageCapabilitiesObject$`supportsShapBasedPredictionExplanations`
      self$`supportsTargetDriftTracking` <- ModelPackageCapabilitiesObject$`supportsTargetDriftTracking`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
