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
#' @title CustomModelDeploymentResponse
#'
#' @description CustomModelDeploymentResponse Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field customModel  \link{CustomModelShortResponse}
#'
#' @field customModelImageId  character The id of the custom model image associated with this deployment.
#'
#' @field customModelVersion  \link{CustomModelVersionShortResponse}
#'
#' @field deployed  character ISO-8601 timestamp of when deployment was created.
#'
#' @field deployedBy  character The username of the user that deployed the custom model.
#'
#' @field executionEnvironment  \link{ExecutionEnvironmentShortResponse}
#'
#' @field executionEnvironmentVersion  \link{ExecutionEnvironmentVersionShortResponse}
#'
#' @field id  character The ID of the deployment.
#'
#' @field imageType  character [optional] The type of the image, either customModelImage if the testing attempt is using a customModelImage as its model or customModelVersion if the testing attempt is using a customModelVersion with dependency management.
#'
#' @field label  character User-friendly name of the model deployment.
#'
#' @field status  character Deployment status.
#'
#' @field testingStatus  character Latest testing status of the deployed custom model image.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
CustomModelDeploymentResponse <- R6::R6Class(
  "CustomModelDeploymentResponse",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`customModel` = NULL, `customModelImageId` = NULL, `customModelVersion` = NULL, `deployed` = NULL, `deployedBy` = NULL, `executionEnvironment` = NULL, `executionEnvironmentVersion` = NULL, `id` = NULL, `imageType` = NULL, `label` = NULL, `status` = NULL, `testingStatus` = NULL) {
      if (!is.null(`customModel`)) {
        stopifnot(R6::is.R6(`customModel`))
      }
      if (!is.null(`customModelImageId`)) {
        stopifnot(is.character(`customModelImageId`), length(`customModelImageId`) == 1)
      }
      if (!is.null(`customModelVersion`)) {
        stopifnot(R6::is.R6(`customModelVersion`))
      }
      if (!is.null(`deployed`)) {
        stopifnot(is.character(`deployed`), length(`deployed`) == 1)
      }
      if (!is.null(`deployedBy`)) {
        stopifnot(is.character(`deployedBy`), length(`deployedBy`) == 1)
      }
      if (!is.null(`executionEnvironment`)) {
        stopifnot(R6::is.R6(`executionEnvironment`))
      }
      if (!is.null(`executionEnvironmentVersion`)) {
        stopifnot(R6::is.R6(`executionEnvironmentVersion`))
      }
      if (!is.null(`id`)) {
        stopifnot(is.character(`id`), length(`id`) == 1)
      }
      if (!is.null(`label`)) {
        stopifnot(is.character(`label`), length(`label`) == 1)
      }
      if (!is.null(`status`)) {
        stopifnot(is.character(`status`), length(`status`) == 1)
      }
      if (!is.null(`testingStatus`)) {
        stopifnot(is.character(`testingStatus`), length(`testingStatus`) == 1)
      }
      if (!is.null(`imageType`)) {
        stopifnot(is.character(`imageType`), length(`imageType`) == 1)
      }
    }
  ),
  public = list(
    `customModel` = NULL,
    `customModelImageId` = NULL,
    `customModelVersion` = NULL,
    `deployed` = NULL,
    `deployedBy` = NULL,
    `executionEnvironment` = NULL,
    `executionEnvironmentVersion` = NULL,
    `id` = NULL,
    `imageType` = NULL,
    `label` = NULL,
    `status` = NULL,
    `testingStatus` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param customModel
    #' @param customModelImageId The id of the custom model image associated with this deployment.
    #' @param customModelVersion
    #' @param deployed ISO-8601 timestamp of when deployment was created.
    #' @param deployedBy The username of the user that deployed the custom model.
    #' @param executionEnvironment
    #' @param executionEnvironmentVersion
    #' @param id The ID of the deployment.
    #' @param imageType The type of the image, either customModelImage if the testing attempt is using a customModelImage as its model or customModelVersion if the testing attempt is using a customModelVersion with dependency management.
    #' @param label User-friendly name of the model deployment.
    #' @param status Deployment status.
    #' @param testingStatus Latest testing status of the deployed custom model image.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`customModel` = NULL, `customModelImageId` = NULL, `customModelVersion` = NULL, `deployed` = NULL, `deployedBy` = NULL, `executionEnvironment` = NULL, `executionEnvironmentVersion` = NULL, `id` = NULL, `label` = NULL, `status` = NULL, `testingStatus` = NULL, `imageType` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`customModel`, `customModelImageId`, `customModelVersion`, `deployed`, `deployedBy`, `executionEnvironment`, `executionEnvironmentVersion`, `id`, `label`, `status`, `testingStatus`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(customModel, customModelImageId, customModelVersion, deployed, deployedBy, executionEnvironment, executionEnvironmentVersion, id, imageType, label, status, testingStatus)
      }
      self$`customModel` <- `customModel`
      self$`customModelImageId` <- `customModelImageId`
      self$`customModelVersion` <- `customModelVersion`
      self$`deployed` <- `deployed`
      self$`deployedBy` <- `deployedBy`
      self$`executionEnvironment` <- `executionEnvironment`
      self$`executionEnvironmentVersion` <- `executionEnvironmentVersion`
      self$`id` <- `id`
      self$`imageType` <- `imageType`
      self$`label` <- `label`
      self$`status` <- `status`
      self$`testingStatus` <- `testingStatus`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(customModel = self$`customModel`, customModelImageId = self$`customModelImageId`, customModelVersion = self$`customModelVersion`, deployed = self$`deployed`, deployedBy = self$`deployedBy`, executionEnvironment = self$`executionEnvironment`, executionEnvironmentVersion = self$`executionEnvironmentVersion`, id = self$`id`, imageType = self$`imageType`, label = self$`label`, status = self$`status`, testingStatus = self$`testingStatus`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`customModel`)) {
          sprintf(
            '"customModel":
            %s
      ',
            jsonlite::toJSON(self$`customModel`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`customModelImageId`)) {
          sprintf(
            '"customModelImageId":
            "%s"
                  ',
            self$`customModelImageId`
          )
        },
        if (!is.null(self$`customModelVersion`)) {
          sprintf(
            '"customModelVersion":
            %s
      ',
            jsonlite::toJSON(self$`customModelVersion`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`deployed`)) {
          sprintf(
            '"deployed":
            "%s"
                  ',
            self$`deployed`
          )
        },
        if (!is.null(self$`deployedBy`)) {
          sprintf(
            '"deployedBy":
            "%s"
                  ',
            self$`deployedBy`
          )
        },
        if (!is.null(self$`executionEnvironment`)) {
          sprintf(
            '"executionEnvironment":
            %s
      ',
            jsonlite::toJSON(self$`executionEnvironment`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`executionEnvironmentVersion`)) {
          sprintf(
            '"executionEnvironmentVersion":
            %s
      ',
            jsonlite::toJSON(self$`executionEnvironmentVersion`$toJSON(), auto_unbox = TRUE, digits = NA)
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
        if (!is.null(self$`imageType`)) {
          sprintf(
            '"imageType":
            "%s"
                  ',
            self$`imageType`
          )
        },
        if (!is.null(self$`label`)) {
          sprintf(
            '"label":
            "%s"
                  ',
            self$`label`
          )
        },
        if (!is.null(self$`status`)) {
          sprintf(
            '"status":
            "%s"
                  ',
            self$`status`
          )
        },
        if (!is.null(self$`testingStatus`)) {
          sprintf(
            '"testingStatus":
            "%s"
                  ',
            self$`testingStatus`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param CustomModelDeploymentResponseJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(CustomModelDeploymentResponseJson, validateParams = FALSE) {
      CustomModelDeploymentResponseObject <- jsonlite::fromJSON(CustomModelDeploymentResponseJson)
      self$`customModel` <- CustomModelShortResponse$new()$fromJSON(jsonlite::toJSON(CustomModelDeploymentResponseObject$customModel, auto_unbox = TRUE, digits = NA, null = "null"))
      self$`customModelImageId` <- CustomModelDeploymentResponseObject$`customModelImageId`
      self$`customModelVersion` <- CustomModelVersionShortResponse$new()$fromJSON(jsonlite::toJSON(CustomModelDeploymentResponseObject$customModelVersion, auto_unbox = TRUE, digits = NA, null = "null"))
      self$`deployed` <- CustomModelDeploymentResponseObject$`deployed`
      self$`deployedBy` <- CustomModelDeploymentResponseObject$`deployedBy`
      self$`executionEnvironment` <- ExecutionEnvironmentShortResponse$new()$fromJSON(jsonlite::toJSON(CustomModelDeploymentResponseObject$executionEnvironment, auto_unbox = TRUE, digits = NA, null = "null"))
      self$`executionEnvironmentVersion` <- ExecutionEnvironmentVersionShortResponse$new()$fromJSON(jsonlite::toJSON(CustomModelDeploymentResponseObject$executionEnvironmentVersion, auto_unbox = TRUE, digits = NA, null = "null"))
      self$`id` <- CustomModelDeploymentResponseObject$`id`
      self$`imageType` <- CustomModelDeploymentResponseObject$`imageType`
      self$`label` <- CustomModelDeploymentResponseObject$`label`
      self$`status` <- CustomModelDeploymentResponseObject$`status`
      self$`testingStatus` <- CustomModelDeploymentResponseObject$`testingStatus`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
