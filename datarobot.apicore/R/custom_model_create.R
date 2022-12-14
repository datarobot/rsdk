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
#' @title CustomModelCreate
#'
#' @description CustomModelCreate Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field calibratePredictions  character [optional] Whether model predictions should be calibrated by DataRobot.Only applies to anomaly detection training tasks; we recommend this if you have not already included calibration in your model code.Calibration improves the probability estimates of a model, and modifies the predictions of non-probabilistic models to be interpretable as probabilities. This will facilitate comparison to DataRobot models, and give access to ROC curve insights on external data.
#'
#' @field classLabels  list( character ) [optional] The class labels for multiclass classification. Required for multiclass inference models. If using one of the [DataRobot] base environments and your model produces an ndarray of unlabeled class probabilities, the order of the labels should match the order of the predicted output
#'
#' @field customModelType  character The type of custom model.
#'
#' @field description  character [optional] The user-friendly description of the model.
#'
#' @field desiredMemory  integer [optional] The amount of memory that is expected to be allocated by the custom model.
#'
#' @field language  character [optional] Programming language name in which model is written.
#'
#' @field maximumMemory  integer [optional] The maximum memory that might be allocated by the custom-model. If exceeded, the custom-model will be killed
#'
#' @field name  character The user-friendly name for the model.
#'
#' @field negativeClassLabel  character [optional] The negative class label for custom models that support binary classification. If specified, &#x60;positiveClassLabel&#x60; must also be specified. Default value is \&quot;0\&quot;.
#'
#' @field networkEgressPolicy  character [optional] Network egress policy.
#'
#' @field positiveClassLabel  character [optional] The positive class label for custom models that support binary classification. If specified, &#x60;negativeClassLabel&#x60; must also be specified. Default value is \&quot;1\&quot;.
#'
#' @field predictionThreshold  numeric [optional] The prediction threshold which will be used for binary classification custom model.
#'
#' @field replicas  integer [optional] A fixed number of replicas that will be set for the given custom-model.
#'
#' @field requiresHa  character [optional] Require all custom model replicas to be deployed on different Kubernetes nodes for predictions fault tolerance.
#'
#' @field supportsBinaryClassification  character [optional] Whether the model supports binary classification.
#'
#' @field supportsRegression  character [optional] Whether the model supports regression.
#'
#' @field targetName  character [optional] The name of the target for labeling predictions. Required for model type &#39;inference&#39;. Specifying this value for a model type &#39;training&#39; will result in an error.
#'
#' @field targetType  character [optional] The target type of the custom model
#'
#' @field userProvidedId  character [optional] A user-provided unique ID associated with the given custom inference model.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
CustomModelCreate <- R6::R6Class(
  "CustomModelCreate",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`calibratePredictions` = NULL, `classLabels` = NULL, `customModelType` = NULL, `description` = NULL, `desiredMemory` = NULL, `language` = NULL, `maximumMemory` = NULL, `name` = NULL, `negativeClassLabel` = NULL, `networkEgressPolicy` = NULL, `positiveClassLabel` = NULL, `predictionThreshold` = NULL, `replicas` = NULL, `requiresHa` = NULL, `supportsBinaryClassification` = NULL, `supportsRegression` = NULL, `targetName` = NULL, `targetType` = NULL, `userProvidedId` = NULL) {
      if (!is.null(`customModelType`)) {
        stopifnot(is.character(`customModelType`), length(`customModelType`) == 1)
      }
      if (!is.null(`name`)) {
        stopifnot(is.character(`name`), length(`name`) == 1)
      }
      if (!is.null(`calibratePredictions`)) {
        stopifnot(is.logical(`calibratePredictions`), length(`calibratePredictions`) == 1)
      }
      if (!is.null(`classLabels`) && length(`classLabels`) > 0) {
        stopifnot(is.vector(`classLabels`), sapply(`classLabels`, is.character))
      }
      if (!is.null(`description`)) {
        stopifnot(is.character(`description`), length(`description`) == 1)
      }
      if (!is.null(`desiredMemory`)) {
        stopifnot(is.numeric(`desiredMemory`), length(`desiredMemory`) == 1)
      }
      if (!is.null(`language`)) {
        stopifnot(is.character(`language`), length(`language`) == 1)
      }
      if (!is.null(`maximumMemory`)) {
        stopifnot(is.numeric(`maximumMemory`), length(`maximumMemory`) == 1)
      }
      if (!is.null(`negativeClassLabel`)) {
        stopifnot(is.character(`negativeClassLabel`), length(`negativeClassLabel`) == 1)
      }
      if (!is.null(`networkEgressPolicy`)) {
        stopifnot(is.character(`networkEgressPolicy`), length(`networkEgressPolicy`) == 1)
      }
      if (!is.null(`positiveClassLabel`)) {
        stopifnot(is.character(`positiveClassLabel`), length(`positiveClassLabel`) == 1)
      }
      if (!is.null(`predictionThreshold`)) {
        stopifnot(is.numeric(`predictionThreshold`), length(`predictionThreshold`) == 1)
      }
      if (!is.null(`replicas`)) {
        stopifnot(is.numeric(`replicas`), length(`replicas`) == 1)
      }
      if (!is.null(`requiresHa`)) {
        stopifnot(is.logical(`requiresHa`), length(`requiresHa`) == 1)
      }
      if (!is.null(`supportsBinaryClassification`)) {
        stopifnot(is.logical(`supportsBinaryClassification`), length(`supportsBinaryClassification`) == 1)
      }
      if (!is.null(`supportsRegression`)) {
        stopifnot(is.logical(`supportsRegression`), length(`supportsRegression`) == 1)
      }
      if (!is.null(`targetName`)) {
        stopifnot(is.character(`targetName`), length(`targetName`) == 1)
      }
      if (!is.null(`targetType`)) {
        stopifnot(is.character(`targetType`), length(`targetType`) == 1)
      }
      if (!is.null(`userProvidedId`)) {
        stopifnot(is.character(`userProvidedId`), length(`userProvidedId`) == 1)
      }
    }
  ),
  public = list(
    `calibratePredictions` = NULL,
    `classLabels` = NULL,
    `customModelType` = NULL,
    `description` = NULL,
    `desiredMemory` = NULL,
    `language` = NULL,
    `maximumMemory` = NULL,
    `name` = NULL,
    `negativeClassLabel` = NULL,
    `networkEgressPolicy` = NULL,
    `positiveClassLabel` = NULL,
    `predictionThreshold` = NULL,
    `replicas` = NULL,
    `requiresHa` = NULL,
    `supportsBinaryClassification` = NULL,
    `supportsRegression` = NULL,
    `targetName` = NULL,
    `targetType` = NULL,
    `userProvidedId` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param calibratePredictions Whether model predictions should be calibrated by DataRobot.Only applies to anomaly detection training tasks; we recommend this if you have not already included calibration in your model code.Calibration improves the probability estimates of a model, and modifies the predictions of non-probabilistic models to be interpretable as probabilities. This will facilitate comparison to DataRobot models, and give access to ROC curve insights on external data.
    #' @param classLabels The class labels for multiclass classification. Required for multiclass inference models. If using one of the [DataRobot] base environments and your model produces an ndarray of unlabeled class probabilities, the order of the labels should match the order of the predicted output
    #' @param customModelType The type of custom model.
    #' @param description The user-friendly description of the model.
    #' @param desiredMemory The amount of memory that is expected to be allocated by the custom model.
    #' @param language Programming language name in which model is written.
    #' @param maximumMemory The maximum memory that might be allocated by the custom-model. If exceeded, the custom-model will be killed
    #' @param name The user-friendly name for the model.
    #' @param negativeClassLabel The negative class label for custom models that support binary classification. If specified, &#x60;positiveClassLabel&#x60; must also be specified. Default value is \&quot;0\&quot;.
    #' @param networkEgressPolicy Network egress policy.
    #' @param positiveClassLabel The positive class label for custom models that support binary classification. If specified, &#x60;negativeClassLabel&#x60; must also be specified. Default value is \&quot;1\&quot;.
    #' @param predictionThreshold The prediction threshold which will be used for binary classification custom model.
    #' @param replicas A fixed number of replicas that will be set for the given custom-model.
    #' @param requiresHa Require all custom model replicas to be deployed on different Kubernetes nodes for predictions fault tolerance.
    #' @param supportsBinaryClassification Whether the model supports binary classification.
    #' @param supportsRegression Whether the model supports regression.
    #' @param targetName The name of the target for labeling predictions. Required for model type &#39;inference&#39;. Specifying this value for a model type &#39;training&#39; will result in an error.
    #' @param targetType The target type of the custom model
    #' @param userProvidedId A user-provided unique ID associated with the given custom inference model.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`customModelType` = NULL, `name` = NULL, `calibratePredictions` = TRUE, `classLabels` = NULL, `description` = NULL, `desiredMemory` = NULL, `language` = NULL, `maximumMemory` = NULL, `negativeClassLabel` = NULL, `networkEgressPolicy` = NULL, `positiveClassLabel` = NULL, `predictionThreshold` = 0.5, `replicas` = NULL, `requiresHa` = NULL, `supportsBinaryClassification` = NULL, `supportsRegression` = NULL, `targetName` = NULL, `targetType` = NULL, `userProvidedId` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`customModelType`, `name`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(calibratePredictions, classLabels, customModelType, description, desiredMemory, language, maximumMemory, name, negativeClassLabel, networkEgressPolicy, positiveClassLabel, predictionThreshold, replicas, requiresHa, supportsBinaryClassification, supportsRegression, targetName, targetType, userProvidedId)
      }
      self$`calibratePredictions` <- `calibratePredictions`
      self$`classLabels` <- `classLabels`
      self$`customModelType` <- `customModelType`
      self$`description` <- `description`
      self$`desiredMemory` <- `desiredMemory`
      self$`language` <- `language`
      self$`maximumMemory` <- `maximumMemory`
      self$`name` <- `name`
      self$`negativeClassLabel` <- `negativeClassLabel`
      self$`networkEgressPolicy` <- `networkEgressPolicy`
      self$`positiveClassLabel` <- `positiveClassLabel`
      self$`predictionThreshold` <- `predictionThreshold`
      self$`replicas` <- `replicas`
      self$`requiresHa` <- `requiresHa`
      self$`supportsBinaryClassification` <- `supportsBinaryClassification`
      self$`supportsRegression` <- `supportsRegression`
      self$`targetName` <- `targetName`
      self$`targetType` <- `targetType`
      self$`userProvidedId` <- `userProvidedId`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(calibratePredictions = self$`calibratePredictions`, classLabels = self$`classLabels`, customModelType = self$`customModelType`, description = self$`description`, desiredMemory = self$`desiredMemory`, language = self$`language`, maximumMemory = self$`maximumMemory`, name = self$`name`, negativeClassLabel = self$`negativeClassLabel`, networkEgressPolicy = self$`networkEgressPolicy`, positiveClassLabel = self$`positiveClassLabel`, predictionThreshold = self$`predictionThreshold`, replicas = self$`replicas`, requiresHa = self$`requiresHa`, supportsBinaryClassification = self$`supportsBinaryClassification`, supportsRegression = self$`supportsRegression`, targetName = self$`targetName`, targetType = self$`targetType`, userProvidedId = self$`userProvidedId`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`calibratePredictions`)) {
          sprintf(
            '"calibratePredictions":
            %s
                  ',
            tolower(self$`calibratePredictions`)
          )
        },
        if (!is.null(self$`classLabels`)) {
          sprintf(
            '"classLabels":
            [%s]
                  ',
            paste(unlist(lapply(self$`classLabels`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`customModelType`)) {
          sprintf(
            '"customModelType":
            "%s"
                  ',
            self$`customModelType`
          )
        },
        if (!is.null(self$`description`)) {
          sprintf(
            '"description":
            "%s"
                  ',
            self$`description`
          )
        },
        if (!is.null(self$`desiredMemory`)) {
          sprintf(
            '"desiredMemory":
            %d
                  ',
            self$`desiredMemory`
          )
        },
        if (!is.null(self$`language`)) {
          sprintf(
            '"language":
            "%s"
                  ',
            self$`language`
          )
        },
        if (!is.null(self$`maximumMemory`)) {
          sprintf(
            '"maximumMemory":
            %d
                  ',
            self$`maximumMemory`
          )
        },
        if (!is.null(self$`name`)) {
          sprintf(
            '"name":
            "%s"
                  ',
            self$`name`
          )
        },
        if (!is.null(self$`negativeClassLabel`)) {
          sprintf(
            '"negativeClassLabel":
            "%s"
                  ',
            self$`negativeClassLabel`
          )
        },
        if (!is.null(self$`networkEgressPolicy`)) {
          sprintf(
            '"networkEgressPolicy":
            "%s"
                  ',
            self$`networkEgressPolicy`
          )
        },
        if (!is.null(self$`positiveClassLabel`)) {
          sprintf(
            '"positiveClassLabel":
            "%s"
                  ',
            self$`positiveClassLabel`
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
        if (!is.null(self$`replicas`)) {
          sprintf(
            '"replicas":
            %d
                  ',
            self$`replicas`
          )
        },
        if (!is.null(self$`requiresHa`)) {
          sprintf(
            '"requiresHa":
            %s
                  ',
            tolower(self$`requiresHa`)
          )
        },
        if (!is.null(self$`supportsBinaryClassification`)) {
          sprintf(
            '"supportsBinaryClassification":
            %s
                  ',
            tolower(self$`supportsBinaryClassification`)
          )
        },
        if (!is.null(self$`supportsRegression`)) {
          sprintf(
            '"supportsRegression":
            %s
                  ',
            tolower(self$`supportsRegression`)
          )
        },
        if (!is.null(self$`targetName`)) {
          sprintf(
            '"targetName":
            "%s"
                  ',
            self$`targetName`
          )
        },
        if (!is.null(self$`targetType`)) {
          sprintf(
            '"targetType":
            "%s"
                  ',
            self$`targetType`
          )
        },
        if (!is.null(self$`userProvidedId`)) {
          sprintf(
            '"userProvidedId":
            "%s"
                  ',
            self$`userProvidedId`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param CustomModelCreateJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(CustomModelCreateJson, validateParams = FALSE) {
      CustomModelCreateObject <- jsonlite::fromJSON(CustomModelCreateJson)
      self$`calibratePredictions` <- CustomModelCreateObject$`calibratePredictions`
      self$`classLabels` <- ApiClient$new()$deserializeObj(CustomModelCreateObject$`classLabels`, "array[character]", loadNamespace("datarobot.apicore"))
      self$`customModelType` <- CustomModelCreateObject$`customModelType`
      self$`description` <- CustomModelCreateObject$`description`
      self$`desiredMemory` <- CustomModelCreateObject$`desiredMemory`
      self$`language` <- CustomModelCreateObject$`language`
      self$`maximumMemory` <- CustomModelCreateObject$`maximumMemory`
      self$`name` <- CustomModelCreateObject$`name`
      self$`negativeClassLabel` <- CustomModelCreateObject$`negativeClassLabel`
      self$`networkEgressPolicy` <- CustomModelCreateObject$`networkEgressPolicy`
      self$`positiveClassLabel` <- CustomModelCreateObject$`positiveClassLabel`
      self$`predictionThreshold` <- CustomModelCreateObject$`predictionThreshold`
      self$`replicas` <- CustomModelCreateObject$`replicas`
      self$`requiresHa` <- CustomModelCreateObject$`requiresHa`
      self$`supportsBinaryClassification` <- CustomModelCreateObject$`supportsBinaryClassification`
      self$`supportsRegression` <- CustomModelCreateObject$`supportsRegression`
      self$`targetName` <- CustomModelCreateObject$`targetName`
      self$`targetType` <- CustomModelCreateObject$`targetType`
      self$`userProvidedId` <- CustomModelCreateObject$`userProvidedId`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
