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
#' @title CustomTaskVersionCreateFromRepository
#'
#' @description CustomTaskVersionCreateFromRepository Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field baseEnvironmentId  character [optional] The base environment to use with this version.
#'
#' @field isMajorUpdate  character [optional] If set to true, new major version will created, otherwise minor version will be created.
#'
#' @field ref  character [optional] Remote reference (branch, commit, etc). Latest, if not specified.
#'
#' @field repositoryId  character The ID of remote repository used to pull sources. This ID can be found using the /api/v2/remoteRepositories/ endpoint.
#'
#' @field requiredMetadata  object [optional] Additional parameters required by the execution environment. The required keys are defined by the fieldNames in the base environment&#39;s requiredMetadataKeys. Once set, they cannot be changed. If you to change them, make a new version.
#'
#' @field requiredMetadataValues  list( \link{RequiredMetadataValue} ) [optional] Additional parameters required by the execution environment. The required fieldNames are defined by the fieldNames in the base environment&#39;s requiredMetadataKeys.
#'
#' @field sourcePath  \link{OneOfstringarray} [optional] A remote repository file path to be pulled into a custom model or custom task.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
CustomTaskVersionCreateFromRepository <- R6::R6Class(
  "CustomTaskVersionCreateFromRepository",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`baseEnvironmentId` = NULL, `isMajorUpdate` = NULL, `ref` = NULL, `repositoryId` = NULL, `requiredMetadata` = NULL, `requiredMetadataValues` = NULL, `sourcePath` = NULL) {
      if (!is.null(`repositoryId`)) {
        stopifnot(is.character(`repositoryId`), length(`repositoryId`) == 1)
      }
      if (!is.null(`baseEnvironmentId`)) {
        stopifnot(is.character(`baseEnvironmentId`), length(`baseEnvironmentId`) == 1)
      }
      if (!is.null(`isMajorUpdate`)) {
        stopifnot(is.logical(`isMajorUpdate`), length(`isMajorUpdate`) == 1)
      }
      if (!is.null(`ref`)) {
        stopifnot(is.character(`ref`), length(`ref`) == 1)
      }
      if (!is.null(`requiredMetadata`)) {
      }
      if (!is.null(`requiredMetadataValues`) && length(`requiredMetadataValues`) > 0) {
        stopifnot(is.vector(`requiredMetadataValues`), sapply(`requiredMetadataValues`, R6::is.R6))
      }
      if (!is.null(`sourcePath`)) {
        .setPrimitiveProperty(typeList = list("character", "array"), propertyData = sourcePath)
      }
    }
  ),
  public = list(
    `baseEnvironmentId` = NULL,
    `isMajorUpdate` = NULL,
    `ref` = NULL,
    `repositoryId` = NULL,
    `requiredMetadata` = NULL,
    `requiredMetadataValues` = NULL,
    `sourcePath` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param baseEnvironmentId The base environment to use with this version.
    #' @param isMajorUpdate If set to true, new major version will created, otherwise minor version will be created.
    #' @param ref Remote reference (branch, commit, etc). Latest, if not specified.
    #' @param repositoryId The ID of remote repository used to pull sources. This ID can be found using the /api/v2/remoteRepositories/ endpoint.
    #' @param requiredMetadata Additional parameters required by the execution environment. The required keys are defined by the fieldNames in the base environment&#39;s requiredMetadataKeys. Once set, they cannot be changed. If you to change them, make a new version.
    #' @param requiredMetadataValues Additional parameters required by the execution environment. The required fieldNames are defined by the fieldNames in the base environment&#39;s requiredMetadataKeys.
    #' @param sourcePath A remote repository file path to be pulled into a custom model or custom task.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`repositoryId` = NULL, `baseEnvironmentId` = NULL, `isMajorUpdate` = TRUE, `ref` = NULL, `requiredMetadata` = NULL, `requiredMetadataValues` = NULL, `sourcePath` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`repositoryId`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(baseEnvironmentId, isMajorUpdate, ref, repositoryId, requiredMetadata, requiredMetadataValues, sourcePath)
      }
      self$`baseEnvironmentId` <- `baseEnvironmentId`
      self$`isMajorUpdate` <- `isMajorUpdate`
      self$`ref` <- `ref`
      self$`repositoryId` <- `repositoryId`
      self$`requiredMetadata` <- `requiredMetadata`
      self$`requiredMetadataValues` <- `requiredMetadataValues`
      self$`sourcePath` <- .setPrimitiveProperty(typeList = list("character", "array"), propertyData = sourcePath)
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(baseEnvironmentId = self$`baseEnvironmentId`, isMajorUpdate = self$`isMajorUpdate`, ref = self$`ref`, repositoryId = self$`repositoryId`, requiredMetadata = self$`requiredMetadata`, requiredMetadataValues = self$`requiredMetadataValues`, sourcePath = self$`sourcePath`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`baseEnvironmentId`)) {
          sprintf(
            '"baseEnvironmentId":
            "%s"
                  ',
            self$`baseEnvironmentId`
          )
        },
        if (!is.null(self$`isMajorUpdate`)) {
          sprintf(
            '"isMajorUpdate":
            %s
                  ',
            tolower(self$`isMajorUpdate`)
          )
        },
        if (!is.null(self$`ref`)) {
          sprintf(
            '"ref":
            "%s"
                  ',
            self$`ref`
          )
        },
        if (!is.null(self$`repositoryId`)) {
          sprintf(
            '"repositoryId":
            "%s"
                  ',
            self$`repositoryId`
          )
        },
        if (!is.null(self$`requiredMetadata`)) {
          sprintf(
            '"requiredMetadata":
            "%s"
                  ',
            self$`requiredMetadata`
          )
        },
        if (!is.null(self$`requiredMetadataValues`)) {
          sprintf(
            '"requiredMetadataValues":
            [%s]
      ',
            paste(sapply(self$`requiredMetadataValues`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`sourcePath`)) {
          sprintf(
            '"sourcePath":
            %s
      ',
            self$`sourcePath`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param CustomTaskVersionCreateFromRepositoryJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(CustomTaskVersionCreateFromRepositoryJson, validateParams = FALSE) {
      CustomTaskVersionCreateFromRepositoryObject <- jsonlite::fromJSON(CustomTaskVersionCreateFromRepositoryJson)
      self$`baseEnvironmentId` <- CustomTaskVersionCreateFromRepositoryObject$`baseEnvironmentId`
      self$`isMajorUpdate` <- CustomTaskVersionCreateFromRepositoryObject$`isMajorUpdate`
      self$`ref` <- CustomTaskVersionCreateFromRepositoryObject$`ref`
      self$`repositoryId` <- CustomTaskVersionCreateFromRepositoryObject$`repositoryId`
      self$`requiredMetadata` <- CustomTaskVersionCreateFromRepositoryObject$`requiredMetadata`
      self$`requiredMetadataValues` <- ApiClient$new()$deserializeObj(CustomTaskVersionCreateFromRepositoryObject$`requiredMetadataValues`, "array[RequiredMetadataValue]", loadNamespace("datarobot.apicore"))
      self$`sourcePath` <- .setPrimitiveProperty(typeList = list("character", "array"), propertyData = CustomTaskVersionCreateFromRepositoryObject$sourcePath)

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
