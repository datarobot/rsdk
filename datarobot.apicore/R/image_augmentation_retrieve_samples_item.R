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
#' @title ImageAugmentationRetrieveSamplesItem
#'
#' @description ImageAugmentationRetrieveSamplesItem Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field height  integer Height of the image in pixels
#'
#' @field imageId  character Id of the image. The augmented image file can be retrieved with :http:get:&#x60;/api/v2/projects/(projectId)/images/(imageId)/file/&#x60;
#'
#' @field originalImageId  character Id of the original image that was transformed to produce the augmented image. If this is an original image (from the original training dataset) this value will be null. The id can be used to retrieve the original image file with: :http:get:&#x60;/api/v2/projects/(projectId)/images/(imageId)/file/&#x60;
#'
#' @field projectId  character Project id
#'
#' @field width  integer Width of the image in pixels
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
ImageAugmentationRetrieveSamplesItem <- R6::R6Class(
  "ImageAugmentationRetrieveSamplesItem",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`height` = NULL, `imageId` = NULL, `originalImageId` = NULL, `projectId` = NULL, `width` = NULL) {
      if (!is.null(`height`)) {
        stopifnot(is.numeric(`height`), length(`height`) == 1)
      }
      if (!is.null(`imageId`)) {
        stopifnot(is.character(`imageId`), length(`imageId`) == 1)
      }
      if (!is.null(`originalImageId`)) {
        stopifnot(is.character(`originalImageId`), length(`originalImageId`) == 1)
      }
      if (!is.null(`projectId`)) {
        stopifnot(is.character(`projectId`), length(`projectId`) == 1)
      }
      if (!is.null(`width`)) {
        stopifnot(is.numeric(`width`), length(`width`) == 1)
      }
    }
  ),
  public = list(
    `height` = NULL,
    `imageId` = NULL,
    `originalImageId` = NULL,
    `projectId` = NULL,
    `width` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param height Height of the image in pixels
    #' @param imageId Id of the image. The augmented image file can be retrieved with :http:get:&#x60;/api/v2/projects/(projectId)/images/(imageId)/file/&#x60;
    #' @param originalImageId Id of the original image that was transformed to produce the augmented image. If this is an original image (from the original training dataset) this value will be null. The id can be used to retrieve the original image file with: :http:get:&#x60;/api/v2/projects/(projectId)/images/(imageId)/file/&#x60;
    #' @param projectId Project id
    #' @param width Width of the image in pixels
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`height` = NULL, `imageId` = NULL, `originalImageId` = NULL, `projectId` = NULL, `width` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`height`, `imageId`, `originalImageId`, `projectId`, `width`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(height, imageId, originalImageId, projectId, width)
      }
      self$`height` <- `height`
      self$`imageId` <- `imageId`
      self$`originalImageId` <- `originalImageId`
      self$`projectId` <- `projectId`
      self$`width` <- `width`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(height = self$`height`, imageId = self$`imageId`, originalImageId = self$`originalImageId`, projectId = self$`projectId`, width = self$`width`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`height`)) {
          sprintf(
            '"height":
            %d
                  ',
            self$`height`
          )
        },
        if (!is.null(self$`imageId`)) {
          sprintf(
            '"imageId":
            "%s"
                  ',
            self$`imageId`
          )
        },
        if (!is.null(self$`originalImageId`)) {
          sprintf(
            '"originalImageId":
            "%s"
                  ',
            self$`originalImageId`
          )
        },
        if (!is.null(self$`projectId`)) {
          sprintf(
            '"projectId":
            "%s"
                  ',
            self$`projectId`
          )
        },
        if (!is.null(self$`width`)) {
          sprintf(
            '"width":
            %d
                  ',
            self$`width`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param ImageAugmentationRetrieveSamplesItemJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(ImageAugmentationRetrieveSamplesItemJson, validateParams = FALSE) {
      ImageAugmentationRetrieveSamplesItemObject <- jsonlite::fromJSON(ImageAugmentationRetrieveSamplesItemJson)
      self$`height` <- ImageAugmentationRetrieveSamplesItemObject$`height`
      self$`imageId` <- ImageAugmentationRetrieveSamplesItemObject$`imageId`
      self$`originalImageId` <- ImageAugmentationRetrieveSamplesItemObject$`originalImageId`
      self$`projectId` <- ImageAugmentationRetrieveSamplesItemObject$`projectId`
      self$`width` <- ImageAugmentationRetrieveSamplesItemObject$`width`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
