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
#' @title SharedRolesUpdate
#'
#' @description SharedRolesUpdate Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field operation  character Name of the action being taken. The only operation is &#39;updateRoles&#39;.
#'
#' @field roles  list( \link{OneOfGrantAccessControlWithUsernameGrantAccessControlWithId} ) Array of GrantAccessControl objects., up to maximum 100 objects.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
SharedRolesUpdate <- R6::R6Class(
  "SharedRolesUpdate",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`operation` = NULL, `roles` = NULL) {
      if (!is.null(`operation`)) {
        stopifnot(is.character(`operation`), length(`operation`) == 1)
      }
      if (!is.null(`roles`)) {
        .setComplexProperty(typeList = list(GrantAccessControlWithUsername, GrantAccessControlWithId), propertyData = roles)
      }
    }
  ),
  public = list(
    `operation` = NULL,
    `roles` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param operation Name of the action being taken. The only operation is &#39;updateRoles&#39;.
    #' @param roles Array of GrantAccessControl objects., up to maximum 100 objects.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`operation` = NULL, `roles` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`operation`, `roles`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(operation, roles)
      }
      self$`operation` <- `operation`
      self$`roles` <- sapply(`roles`, function(item) .setComplexProperty(typeList = list(GrantAccessControlWithUsername, GrantAccessControlWithId), propertyData = item))
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(operation = self$`operation`, roles = self$`roles`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`operation`)) {
          sprintf(
            '"operation":
            "%s"
                  ',
            self$`operation`
          )
        },
        if (!is.null(self$`roles`)) {
          sprintf(
            '"roles":
            [%s]
      ',
            paste(sapply(self$`roles`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param SharedRolesUpdateJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(SharedRolesUpdateJson, validateParams = FALSE) {
      SharedRolesUpdateObject <- jsonlite::fromJSON(SharedRolesUpdateJson)
      self$`operation` <- SharedRolesUpdateObject$`operation`
      self$`roles` <- ApiClient$new()$deserializeObj(SharedRolesUpdateObject$`roles`, "array[OneOfGrantAccessControlWithUsernameGrantAccessControlWithId]", loadNamespace("datarobot.apicore"))

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
