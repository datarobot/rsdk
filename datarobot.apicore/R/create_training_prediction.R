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
#' @title CreateTrainingPrediction
#'
#' @description CreateTrainingPrediction Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field dataSubset  character Subset of data predicted on: The value \&quot;all\&quot; returns predictions for all rows in the dataset including data used for training, validation, holdout and any rows discarded. This is not available for large datasets or projects created with Date/Time partitioning. The value \&quot;validationAndHoldout\&quot; returns predictions for the rows used to calculate the validation score and the holdout score. Not available for large projects or Date/Time projects for models trained into the validation set. The value \&quot;holdout\&quot; returns predictions for the rows used to calculate the holdout score. Not available for projects created without a holdout or for models trained into holdout for large datasets or created with Date/Time partitioning. The value \&quot;allBacktests\&quot; returns predictions for the rows used to calculate the backtesting scores for Date/Time projects. The value \&quot;validation\&quot; returns predictions for the rows used to calculate the validation score.
#'
#' @field explanationAlgorithm  character [optional] If set to \&quot;shap\&quot;, the response will include prediction explanations based on the SHAP explainer (SHapley Additive exPlanations). Defaults to null (no prediction explanations)
#'
#' @field maxExplanations  integer [optional] Specifies the maximum number of explanation values that should be returned for each row, ordered by absolute value, greatest to least. In the case of \&quot;shap\&quot;: If not set, explanations are returned for all features. If the number of features is greater than the \&quot;maxExplanations\&quot;, the sum of remaining values will also be returned as \&quot;shapRemainingTotal\&quot;. Defaults to null for datasets narrower than 100 columns, defaults to 100 for datasets wider than 100 columns. Cannot be set if \&quot;explanationAlgorithm\&quot; is omitted.
#'
#' @field modelId  character The model to make predictions on
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
CreateTrainingPrediction <- R6::R6Class(
  "CreateTrainingPrediction",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`dataSubset` = NULL, `explanationAlgorithm` = NULL, `maxExplanations` = NULL, `modelId` = NULL) {
      if (!is.null(`dataSubset`)) {
        stopifnot(is.character(`dataSubset`), length(`dataSubset`) == 1)
      }
      if (!is.null(`modelId`)) {
        stopifnot(is.character(`modelId`), length(`modelId`) == 1)
      }
      if (!is.null(`explanationAlgorithm`)) {
        stopifnot(is.character(`explanationAlgorithm`), length(`explanationAlgorithm`) == 1)
      }
      if (!is.null(`maxExplanations`)) {
        stopifnot(is.numeric(`maxExplanations`), length(`maxExplanations`) == 1)
      }
    }
  ),
  public = list(
    `dataSubset` = NULL,
    `explanationAlgorithm` = NULL,
    `maxExplanations` = NULL,
    `modelId` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param dataSubset Subset of data predicted on: The value \&quot;all\&quot; returns predictions for all rows in the dataset including data used for training, validation, holdout and any rows discarded. This is not available for large datasets or projects created with Date/Time partitioning. The value \&quot;validationAndHoldout\&quot; returns predictions for the rows used to calculate the validation score and the holdout score. Not available for large projects or Date/Time projects for models trained into the validation set. The value \&quot;holdout\&quot; returns predictions for the rows used to calculate the holdout score. Not available for projects created without a holdout or for models trained into holdout for large datasets or created with Date/Time partitioning. The value \&quot;allBacktests\&quot; returns predictions for the rows used to calculate the backtesting scores for Date/Time projects. The value \&quot;validation\&quot; returns predictions for the rows used to calculate the validation score.
    #' @param explanationAlgorithm If set to \&quot;shap\&quot;, the response will include prediction explanations based on the SHAP explainer (SHapley Additive exPlanations). Defaults to null (no prediction explanations)
    #' @param maxExplanations Specifies the maximum number of explanation values that should be returned for each row, ordered by absolute value, greatest to least. In the case of \&quot;shap\&quot;: If not set, explanations are returned for all features. If the number of features is greater than the \&quot;maxExplanations\&quot;, the sum of remaining values will also be returned as \&quot;shapRemainingTotal\&quot;. Defaults to null for datasets narrower than 100 columns, defaults to 100 for datasets wider than 100 columns. Cannot be set if \&quot;explanationAlgorithm\&quot; is omitted.
    #' @param modelId The model to make predictions on
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`dataSubset` = NULL, `modelId` = NULL, `explanationAlgorithm` = NULL, `maxExplanations` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`dataSubset`, `modelId`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(dataSubset, explanationAlgorithm, maxExplanations, modelId)
      }
      self$`dataSubset` <- `dataSubset`
      self$`explanationAlgorithm` <- `explanationAlgorithm`
      self$`maxExplanations` <- `maxExplanations`
      self$`modelId` <- `modelId`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(dataSubset = self$`dataSubset`, explanationAlgorithm = self$`explanationAlgorithm`, maxExplanations = self$`maxExplanations`, modelId = self$`modelId`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`dataSubset`)) {
          sprintf(
            '"dataSubset":
            "%s"
                  ',
            self$`dataSubset`
          )
        },
        if (!is.null(self$`explanationAlgorithm`)) {
          sprintf(
            '"explanationAlgorithm":
            "%s"
                  ',
            self$`explanationAlgorithm`
          )
        },
        if (!is.null(self$`maxExplanations`)) {
          sprintf(
            '"maxExplanations":
            %d
                  ',
            self$`maxExplanations`
          )
        },
        if (!is.null(self$`modelId`)) {
          sprintf(
            '"modelId":
            "%s"
                  ',
            self$`modelId`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param CreateTrainingPredictionJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(CreateTrainingPredictionJson, validateParams = FALSE) {
      CreateTrainingPredictionObject <- jsonlite::fromJSON(CreateTrainingPredictionJson)
      self$`dataSubset` <- CreateTrainingPredictionObject$`dataSubset`
      self$`explanationAlgorithm` <- CreateTrainingPredictionObject$`explanationAlgorithm`
      self$`maxExplanations` <- CreateTrainingPredictionObject$`maxExplanations`
      self$`modelId` <- CreateTrainingPredictionObject$`modelId`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
