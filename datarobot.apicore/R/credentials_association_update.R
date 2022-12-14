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
#' @title CredentialsAssociationUpdate
#'
#' @description CredentialsAssociationUpdate Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field credentialsToAdd  list( \link{CredentialsToAdd} ) [optional] Objects to associate with given credentials.
#'
#' @field credentialsToRemove  list( character ) [optional] Object IDs, each of which identifies an object to be disassociated from this credential. To see which objects are currently associated, see the response from :http:get:&#x60;/api/v2/credentials/(credentialId)/associations/&#x60;.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
CredentialsAssociationUpdate <- R6::R6Class(
  "CredentialsAssociationUpdate",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`credentialsToAdd` = NULL, `credentialsToRemove` = NULL) {
      if (!is.null(`credentialsToAdd`) && length(`credentialsToAdd`) > 0) {
        stopifnot(is.vector(`credentialsToAdd`), sapply(`credentialsToAdd`, R6::is.R6))
      }
      if (!is.null(`credentialsToRemove`) && length(`credentialsToRemove`) > 0) {
        stopifnot(is.vector(`credentialsToRemove`), sapply(`credentialsToRemove`, is.character))
      }
    }
  ),
  public = list(
    `credentialsToAdd` = NULL,
    `credentialsToRemove` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param credentialsToAdd Objects to associate with given credentials.
    #' @param credentialsToRemove Object IDs, each of which identifies an object to be disassociated from this credential. To see which objects are currently associated, see the response from :http:get:&#x60;/api/v2/credentials/(credentialId)/associations/&#x60;.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`credentialsToAdd` = NULL, `credentialsToRemove` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(credentialsToAdd, credentialsToRemove)
      }
      self$`credentialsToAdd` <- `credentialsToAdd`
      self$`credentialsToRemove` <- `credentialsToRemove`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(credentialsToAdd = self$`credentialsToAdd`, credentialsToRemove = self$`credentialsToRemove`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`credentialsToAdd`)) {
          sprintf(
            '"credentialsToAdd":
            [%s]
      ',
            paste(sapply(self$`credentialsToAdd`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`credentialsToRemove`)) {
          sprintf(
            '"credentialsToRemove":
            [%s]
                  ',
            paste(unlist(lapply(self$`credentialsToRemove`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param CredentialsAssociationUpdateJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(CredentialsAssociationUpdateJson, validateParams = FALSE) {
      CredentialsAssociationUpdateObject <- jsonlite::fromJSON(CredentialsAssociationUpdateJson)
      self$`credentialsToAdd` <- ApiClient$new()$deserializeObj(CredentialsAssociationUpdateObject$`credentialsToAdd`, "array[CredentialsToAdd]", loadNamespace("datarobot.apicore"))
      self$`credentialsToRemove` <- ApiClient$new()$deserializeObj(CredentialsAssociationUpdateObject$`credentialsToRemove`, "array[character]", loadNamespace("datarobot.apicore"))

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
