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
#' @title ModelReplacementSubmission
#'
#' @description ModelReplacementSubmission Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field modelId  character [optional] ID of the model used to replace deployment&#39;s champion model. Required if modelPackageId is not provided.
#'
#' @field modelPackageId  character [optional] ID of the model package used to replace deployment&#39;s champion model. Required if modelId is not provided.
#'
#' @field reason  character Reason for the model replacement.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
ModelReplacementSubmission <- R6::R6Class(
  "ModelReplacementSubmission",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`modelId` = NULL, `modelPackageId` = NULL, `reason` = NULL) {
      if (!is.null(`reason`)) {
        stopifnot(is.character(`reason`), length(`reason`) == 1)
      }
      if (!is.null(`modelId`)) {
        stopifnot(is.character(`modelId`), length(`modelId`) == 1)
      }
      if (!is.null(`modelPackageId`)) {
        stopifnot(is.character(`modelPackageId`), length(`modelPackageId`) == 1)
      }
    }
  ),
  public = list(
    `modelId` = NULL,
    `modelPackageId` = NULL,
    `reason` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param modelId ID of the model used to replace deployment&#39;s champion model. Required if modelPackageId is not provided.
    #' @param modelPackageId ID of the model package used to replace deployment&#39;s champion model. Required if modelId is not provided.
    #' @param reason Reason for the model replacement.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`reason` = NULL, `modelId` = NULL, `modelPackageId` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`reason`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(modelId, modelPackageId, reason)
      }
      self$`modelId` <- `modelId`
      self$`modelPackageId` <- `modelPackageId`
      self$`reason` <- `reason`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(modelId = self$`modelId`, modelPackageId = self$`modelPackageId`, reason = self$`reason`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`modelId`)) {
          sprintf(
            '"modelId":
            "%s"
                  ',
            self$`modelId`
          )
        },
        if (!is.null(self$`modelPackageId`)) {
          sprintf(
            '"modelPackageId":
            "%s"
                  ',
            self$`modelPackageId`
          )
        },
        if (!is.null(self$`reason`)) {
          sprintf(
            '"reason":
            "%s"
                  ',
            self$`reason`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param ModelReplacementSubmissionJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(ModelReplacementSubmissionJson, validateParams = FALSE) {
      ModelReplacementSubmissionObject <- jsonlite::fromJSON(ModelReplacementSubmissionJson)
      self$`modelId` <- ModelReplacementSubmissionObject$`modelId`
      self$`modelPackageId` <- ModelReplacementSubmissionObject$`modelPackageId`
      self$`reason` <- ModelReplacementSubmissionObject$`reason`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
