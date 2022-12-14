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
#' @title ApprovalPolicyIntendedAction
#'
#' @description ApprovalPolicyIntendedAction Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field action  character Type of action to trigger on.
#'
#' @field condition  \link{ApprovalPolicyIntendedActionCondition} [optional]
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
ApprovalPolicyIntendedAction <- R6::R6Class(
  "ApprovalPolicyIntendedAction",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`action` = NULL, `condition` = NULL) {
      if (!is.null(`action`)) {
        stopifnot(is.character(`action`), length(`action`) == 1)
      }
      if (!is.null(`condition`)) {
        stopifnot(R6::is.R6(`condition`))
      }
    }
  ),
  public = list(
    `action` = NULL,
    `condition` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param action Type of action to trigger on.
    #' @param condition
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`action` = NULL, `condition` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`action`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(action, condition)
      }
      self$`action` <- `action`
      self$`condition` <- `condition`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(action = self$`action`, condition = self$`condition`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`action`)) {
          sprintf(
            '"action":
            "%s"
                  ',
            self$`action`
          )
        },
        if (!is.null(self$`condition`)) {
          sprintf(
            '"condition":
            %s
      ',
            jsonlite::toJSON(self$`condition`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param ApprovalPolicyIntendedActionJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(ApprovalPolicyIntendedActionJson, validateParams = FALSE) {
      ApprovalPolicyIntendedActionObject <- jsonlite::fromJSON(ApprovalPolicyIntendedActionJson)
      self$`action` <- ApprovalPolicyIntendedActionObject$`action`
      self$`condition` <- ApprovalPolicyIntendedActionCondition$new()$fromJSON(jsonlite::toJSON(ApprovalPolicyIntendedActionObject$condition, auto_unbox = TRUE, digits = NA, null = "null"))

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
