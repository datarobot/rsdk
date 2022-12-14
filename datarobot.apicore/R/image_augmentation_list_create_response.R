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
#' @title ImageAugmentationListCreateResponse
#'
#' @description ImageAugmentationListCreateResponse Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field augmentationListId  character                  Id of the newly created augmentation list which can be used to:                 - retrieve the full augmentation list details;                 - retrieve augmentation previews;                 - to set the list id of a model in advanced tuning;                 - to delete or rename the list.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
ImageAugmentationListCreateResponse <- R6::R6Class(
  "ImageAugmentationListCreateResponse",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`augmentationListId` = NULL) {
      if (!is.null(`augmentationListId`)) {
        stopifnot(is.character(`augmentationListId`), length(`augmentationListId`) == 1)
      }
    }
  ),
  public = list(
    `augmentationListId` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param augmentationListId                  Id of the newly created augmentation list which can be used to:                 - retrieve the full augmentation list details;                 - retrieve augmentation previews;                 - to set the list id of a model in advanced tuning;                 - to delete or rename the list.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`augmentationListId` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`augmentationListId`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(augmentationListId)
      }
      self$`augmentationListId` <- `augmentationListId`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(augmentationListId = self$`augmentationListId`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`augmentationListId`)) {
          sprintf(
            '"augmentationListId":
            "%s"
                  ',
            self$`augmentationListId`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param ImageAugmentationListCreateResponseJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(ImageAugmentationListCreateResponseJson, validateParams = FALSE) {
      ImageAugmentationListCreateResponseObject <- jsonlite::fromJSON(ImageAugmentationListCreateResponseJson)
      self$`augmentationListId` <- ImageAugmentationListCreateResponseObject$`augmentationListId`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
