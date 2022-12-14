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
#' @title Reasons
#'
#' @description Reasons Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field supportsDocumentTextExtractionSampleInsight  character [optional] If present, the reason document text extraction sample insights are not supported for the model.
#'
#' @field supportsImageActivationMaps  character [optional] If present, the reason image activation maps are not supported for the model.
#'
#' @field supportsImageEmbedding  character [optional] If present, the reason image embeddings are not supported for the model.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Reasons <- R6::R6Class(
  "Reasons",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`supportsDocumentTextExtractionSampleInsight` = NULL, `supportsImageActivationMaps` = NULL, `supportsImageEmbedding` = NULL) {
      if (!is.null(`supportsDocumentTextExtractionSampleInsight`)) {
        stopifnot(is.character(`supportsDocumentTextExtractionSampleInsight`), length(`supportsDocumentTextExtractionSampleInsight`) == 1)
      }
      if (!is.null(`supportsImageActivationMaps`)) {
        stopifnot(is.character(`supportsImageActivationMaps`), length(`supportsImageActivationMaps`) == 1)
      }
      if (!is.null(`supportsImageEmbedding`)) {
        stopifnot(is.character(`supportsImageEmbedding`), length(`supportsImageEmbedding`) == 1)
      }
    }
  ),
  public = list(
    `supportsDocumentTextExtractionSampleInsight` = NULL,
    `supportsImageActivationMaps` = NULL,
    `supportsImageEmbedding` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param supportsDocumentTextExtractionSampleInsight If present, the reason document text extraction sample insights are not supported for the model.
    #' @param supportsImageActivationMaps If present, the reason image activation maps are not supported for the model.
    #' @param supportsImageEmbedding If present, the reason image embeddings are not supported for the model.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`supportsDocumentTextExtractionSampleInsight` = NULL, `supportsImageActivationMaps` = NULL, `supportsImageEmbedding` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(supportsDocumentTextExtractionSampleInsight, supportsImageActivationMaps, supportsImageEmbedding)
      }
      self$`supportsDocumentTextExtractionSampleInsight` <- `supportsDocumentTextExtractionSampleInsight`
      self$`supportsImageActivationMaps` <- `supportsImageActivationMaps`
      self$`supportsImageEmbedding` <- `supportsImageEmbedding`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(supportsDocumentTextExtractionSampleInsight = self$`supportsDocumentTextExtractionSampleInsight`, supportsImageActivationMaps = self$`supportsImageActivationMaps`, supportsImageEmbedding = self$`supportsImageEmbedding`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`supportsDocumentTextExtractionSampleInsight`)) {
          sprintf(
            '"supportsDocumentTextExtractionSampleInsight":
            "%s"
                  ',
            self$`supportsDocumentTextExtractionSampleInsight`
          )
        },
        if (!is.null(self$`supportsImageActivationMaps`)) {
          sprintf(
            '"supportsImageActivationMaps":
            "%s"
                  ',
            self$`supportsImageActivationMaps`
          )
        },
        if (!is.null(self$`supportsImageEmbedding`)) {
          sprintf(
            '"supportsImageEmbedding":
            "%s"
                  ',
            self$`supportsImageEmbedding`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param ReasonsJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(ReasonsJson, validateParams = FALSE) {
      ReasonsObject <- jsonlite::fromJSON(ReasonsJson)
      self$`supportsDocumentTextExtractionSampleInsight` <- ReasonsObject$`supportsDocumentTextExtractionSampleInsight`
      self$`supportsImageActivationMaps` <- ReasonsObject$`supportsImageActivationMaps`
      self$`supportsImageEmbedding` <- ReasonsObject$`supportsImageEmbedding`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
