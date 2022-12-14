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
#' @title UpdateCatalogMetadata
#'
#' @description UpdateCatalogMetadata Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field description  character [optional] New catalog item description
#'
#' @field name  character [optional] New catalog item name
#'
#' @field tags  list( character ) [optional] New catalog item tags. Tags must be the lower case, without spaces,and cannot include -$.,{}\&quot;#&#39; special characters.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
UpdateCatalogMetadata <- R6::R6Class(
  "UpdateCatalogMetadata",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`description` = NULL, `name` = NULL, `tags` = NULL) {
      if (!is.null(`description`)) {
        stopifnot(is.character(`description`), length(`description`) == 1)
      }
      if (!is.null(`name`)) {
        stopifnot(is.character(`name`), length(`name`) == 1)
      }
      if (!is.null(`tags`) && length(`tags`) > 0) {
        stopifnot(is.vector(`tags`), sapply(`tags`, is.character))
      }
    }
  ),
  public = list(
    `description` = NULL,
    `name` = NULL,
    `tags` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param description New catalog item description
    #' @param name New catalog item name
    #' @param tags New catalog item tags. Tags must be the lower case, without spaces,and cannot include -$.,{}\&quot;#&#39; special characters.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`description` = NULL, `name` = NULL, `tags` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(description, name, tags)
      }
      self$`description` <- `description`
      self$`name` <- `name`
      self$`tags` <- `tags`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(description = self$`description`, name = self$`name`, tags = self$`tags`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`description`)) {
          sprintf(
            '"description":
            "%s"
                  ',
            self$`description`
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
        if (!is.null(self$`tags`)) {
          sprintf(
            '"tags":
            [%s]
                  ',
            paste(unlist(lapply(self$`tags`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param UpdateCatalogMetadataJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(UpdateCatalogMetadataJson, validateParams = FALSE) {
      UpdateCatalogMetadataObject <- jsonlite::fromJSON(UpdateCatalogMetadataJson)
      self$`description` <- UpdateCatalogMetadataObject$`description`
      self$`name` <- UpdateCatalogMetadataObject$`name`
      self$`tags` <- ApiClient$new()$deserializeObj(UpdateCatalogMetadataObject$`tags`, "array[character]", loadNamespace("datarobot.apicore"))

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
