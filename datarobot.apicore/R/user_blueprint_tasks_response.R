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
#' @title UserBlueprintTasksResponse
#'
#' @description UserBlueprintTasksResponse Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field categories  list( \link{UserBlueprintTaskCategoryItem} ) A list of the available task categories, sub-categories, and tasks.
#'
#' @field tasks  list( \link{UserBlueprintTaskLookupEntry} ) A list of task codes and their task definitions.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
UserBlueprintTasksResponse <- R6::R6Class(
  "UserBlueprintTasksResponse",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`categories` = NULL, `tasks` = NULL) {
      if (!is.null(`categories`)) {
        stopifnot(is.vector(`categories`), sapply(`categories`, R6::is.R6))
      }
      if (!is.null(`tasks`)) {
        stopifnot(is.vector(`tasks`), sapply(`tasks`, R6::is.R6))
      }
    }
  ),
  public = list(
    `categories` = NULL,
    `tasks` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param categories A list of the available task categories, sub-categories, and tasks.
    #' @param tasks A list of task codes and their task definitions.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`categories` = NULL, `tasks` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`categories`, `tasks`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(categories, tasks)
      }
      self$`categories` <- `categories`
      self$`tasks` <- `tasks`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(categories = self$`categories`, tasks = self$`tasks`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`categories`)) {
          sprintf(
            '"categories":
            [%s]
      ',
            paste(sapply(self$`categories`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`tasks`)) {
          sprintf(
            '"tasks":
            [%s]
      ',
            paste(sapply(self$`tasks`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param UserBlueprintTasksResponseJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(UserBlueprintTasksResponseJson, validateParams = FALSE) {
      UserBlueprintTasksResponseObject <- jsonlite::fromJSON(UserBlueprintTasksResponseJson)
      self$`categories` <- ApiClient$new()$deserializeObj(UserBlueprintTasksResponseObject$`categories`, "array[UserBlueprintTaskCategoryItem]", loadNamespace("datarobot.apicore"))
      self$`tasks` <- ApiClient$new()$deserializeObj(UserBlueprintTasksResponseObject$`tasks`, "array[UserBlueprintTaskLookupEntry]", loadNamespace("datarobot.apicore"))

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
