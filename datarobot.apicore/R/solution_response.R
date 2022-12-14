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
#' @title SolutionResponse
#'
#' @description SolutionResponse Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field bestModel  character True if this solution generates the best model.
#'
#' @field complexity  integer The complexity score for this solution. Complexity score is a function of the mathematical operators used in the current solution. The complexity calculation can be tuned via model hyperparameters.
#'
#' @field error  numeric The error for the current solution, as computed by eureqa using the &#x60;errorMetric&#x60; error metric. None if Eureqa model refitted existing solutions.
#'
#' @field eureqaSolutionId  character The ID of the solution.
#'
#' @field expression  character The eureqa \&quot;solution string\&quot;. This is a mathematical expression; human-readable but with strict syntax specifications defined by Eureqa.
#'
#' @field expressionAnnotated  character The &#x60;expression&#x60;, rendered with additional tags to assist in automatic parsing.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
SolutionResponse <- R6::R6Class(
  "SolutionResponse",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`bestModel` = NULL, `complexity` = NULL, `error` = NULL, `eureqaSolutionId` = NULL, `expression` = NULL, `expressionAnnotated` = NULL) {
      if (!is.null(`bestModel`)) {
        stopifnot(is.logical(`bestModel`), length(`bestModel`) == 1)
      }
      if (!is.null(`complexity`)) {
        stopifnot(is.numeric(`complexity`), length(`complexity`) == 1)
      }
      if (!is.null(`error`)) {
      }
      if (!is.null(`eureqaSolutionId`)) {
        stopifnot(is.character(`eureqaSolutionId`), length(`eureqaSolutionId`) == 1)
      }
      if (!is.null(`expression`)) {
        stopifnot(is.character(`expression`), length(`expression`) == 1)
      }
      if (!is.null(`expressionAnnotated`)) {
        stopifnot(is.character(`expressionAnnotated`), length(`expressionAnnotated`) == 1)
      }
    }
  ),
  public = list(
    `bestModel` = NULL,
    `complexity` = NULL,
    `error` = NULL,
    `eureqaSolutionId` = NULL,
    `expression` = NULL,
    `expressionAnnotated` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param bestModel True if this solution generates the best model.
    #' @param complexity The complexity score for this solution. Complexity score is a function of the mathematical operators used in the current solution. The complexity calculation can be tuned via model hyperparameters.
    #' @param error The error for the current solution, as computed by eureqa using the &#x60;errorMetric&#x60; error metric. None if Eureqa model refitted existing solutions.
    #' @param eureqaSolutionId The ID of the solution.
    #' @param expression The eureqa \&quot;solution string\&quot;. This is a mathematical expression; human-readable but with strict syntax specifications defined by Eureqa.
    #' @param expressionAnnotated The &#x60;expression&#x60;, rendered with additional tags to assist in automatic parsing.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`bestModel` = NULL, `complexity` = NULL, `error` = NULL, `eureqaSolutionId` = NULL, `expression` = NULL, `expressionAnnotated` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`bestModel`, `complexity`, `error`, `eureqaSolutionId`, `expression`, `expressionAnnotated`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(bestModel, complexity, error, eureqaSolutionId, expression, expressionAnnotated)
      }
      self$`bestModel` <- `bestModel`
      self$`complexity` <- `complexity`
      self$`error` <- `error`
      self$`eureqaSolutionId` <- `eureqaSolutionId`
      self$`expression` <- `expression`
      self$`expressionAnnotated` <- `expressionAnnotated`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(bestModel = self$`bestModel`, complexity = self$`complexity`, error = self$`error`, eureqaSolutionId = self$`eureqaSolutionId`, expression = self$`expression`, expressionAnnotated = self$`expressionAnnotated`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`bestModel`)) {
          sprintf(
            '"bestModel":
            %s
                  ',
            tolower(self$`bestModel`)
          )
        },
        if (!is.null(self$`complexity`)) {
          sprintf(
            '"complexity":
            %d
                  ',
            self$`complexity`
          )
        },
        if (!is.null(self$`error`)) {
          sprintf(
            '"error":
            %d
                  ',
            self$`error`
          )
        },
        if (!is.null(self$`eureqaSolutionId`)) {
          sprintf(
            '"eureqaSolutionId":
            "%s"
                  ',
            self$`eureqaSolutionId`
          )
        },
        if (!is.null(self$`expression`)) {
          sprintf(
            '"expression":
            "%s"
                  ',
            self$`expression`
          )
        },
        if (!is.null(self$`expressionAnnotated`)) {
          sprintf(
            '"expressionAnnotated":
            "%s"
                  ',
            self$`expressionAnnotated`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param SolutionResponseJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(SolutionResponseJson, validateParams = FALSE) {
      SolutionResponseObject <- jsonlite::fromJSON(SolutionResponseJson)
      self$`bestModel` <- SolutionResponseObject$`bestModel`
      self$`complexity` <- SolutionResponseObject$`complexity`
      self$`error` <- SolutionResponseObject$`error`
      self$`eureqaSolutionId` <- SolutionResponseObject$`eureqaSolutionId`
      self$`expression` <- SolutionResponseObject$`expression`
      self$`expressionAnnotated` <- SolutionResponseObject$`expressionAnnotated`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
