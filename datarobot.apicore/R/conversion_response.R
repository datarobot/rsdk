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
#' @title ConversionResponse
#'
#' @description ConversionResponse Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field conversionInProgress  character [optional] Whether a custom model conversion is in progress or not.
#'
#' @field conversionSucceeded  character [optional] Indication for a successful custom model conversion.
#'
#' @field created  character ISO-8601 timestamp of when the custom model conversion created.
#'
#' @field customModelVersionId  character ID of the custom model version.
#'
#' @field generatedMetadata  \link{GeneratedMetadata} [optional]
#'
#' @field id  character ID of the custom model version.
#'
#' @field logMessage  character [optional] The output log message from the custom model conversion process.
#'
#' @field mainProgramItemId  character [optional] The main program file item ID.
#'
#' @field shouldStop  character [optional] Whether the user requested to stop the given conversion.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
ConversionResponse <- R6::R6Class(
  "ConversionResponse",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`conversionInProgress` = NULL, `conversionSucceeded` = NULL, `created` = NULL, `customModelVersionId` = NULL, `generatedMetadata` = NULL, `id` = NULL, `logMessage` = NULL, `mainProgramItemId` = NULL, `shouldStop` = NULL) {
      if (!is.null(`created`)) {
        stopifnot(is.character(`created`), length(`created`) == 1)
      }
      if (!is.null(`customModelVersionId`)) {
        stopifnot(is.character(`customModelVersionId`), length(`customModelVersionId`) == 1)
      }
      if (!is.null(`id`)) {
        stopifnot(is.character(`id`), length(`id`) == 1)
      }
      if (!is.null(`conversionInProgress`)) {
        stopifnot(is.logical(`conversionInProgress`), length(`conversionInProgress`) == 1)
      }
      if (!is.null(`conversionSucceeded`)) {
        stopifnot(is.logical(`conversionSucceeded`), length(`conversionSucceeded`) == 1)
      }
      if (!is.null(`generatedMetadata`)) {
        stopifnot(R6::is.R6(`generatedMetadata`))
      }
      if (!is.null(`logMessage`)) {
        stopifnot(is.character(`logMessage`), length(`logMessage`) == 1)
      }
      if (!is.null(`mainProgramItemId`)) {
        stopifnot(is.character(`mainProgramItemId`), length(`mainProgramItemId`) == 1)
      }
      if (!is.null(`shouldStop`)) {
        stopifnot(is.logical(`shouldStop`), length(`shouldStop`) == 1)
      }
    }
  ),
  public = list(
    `conversionInProgress` = NULL,
    `conversionSucceeded` = NULL,
    `created` = NULL,
    `customModelVersionId` = NULL,
    `generatedMetadata` = NULL,
    `id` = NULL,
    `logMessage` = NULL,
    `mainProgramItemId` = NULL,
    `shouldStop` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param conversionInProgress Whether a custom model conversion is in progress or not.
    #' @param conversionSucceeded Indication for a successful custom model conversion.
    #' @param created ISO-8601 timestamp of when the custom model conversion created.
    #' @param customModelVersionId ID of the custom model version.
    #' @param generatedMetadata
    #' @param id ID of the custom model version.
    #' @param logMessage The output log message from the custom model conversion process.
    #' @param mainProgramItemId The main program file item ID.
    #' @param shouldStop Whether the user requested to stop the given conversion.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`created` = NULL, `customModelVersionId` = NULL, `id` = NULL, `conversionInProgress` = NULL, `conversionSucceeded` = NULL, `generatedMetadata` = NULL, `logMessage` = NULL, `mainProgramItemId` = NULL, `shouldStop` = FALSE, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`created`, `customModelVersionId`, `id`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(conversionInProgress, conversionSucceeded, created, customModelVersionId, generatedMetadata, id, logMessage, mainProgramItemId, shouldStop)
      }
      self$`conversionInProgress` <- `conversionInProgress`
      self$`conversionSucceeded` <- `conversionSucceeded`
      self$`created` <- `created`
      self$`customModelVersionId` <- `customModelVersionId`
      self$`generatedMetadata` <- `generatedMetadata`
      self$`id` <- `id`
      self$`logMessage` <- `logMessage`
      self$`mainProgramItemId` <- `mainProgramItemId`
      self$`shouldStop` <- `shouldStop`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(conversionInProgress = self$`conversionInProgress`, conversionSucceeded = self$`conversionSucceeded`, created = self$`created`, customModelVersionId = self$`customModelVersionId`, generatedMetadata = self$`generatedMetadata`, id = self$`id`, logMessage = self$`logMessage`, mainProgramItemId = self$`mainProgramItemId`, shouldStop = self$`shouldStop`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`conversionInProgress`)) {
          sprintf(
            '"conversionInProgress":
            %s
                  ',
            tolower(self$`conversionInProgress`)
          )
        },
        if (!is.null(self$`conversionSucceeded`)) {
          sprintf(
            '"conversionSucceeded":
            %s
                  ',
            tolower(self$`conversionSucceeded`)
          )
        },
        if (!is.null(self$`created`)) {
          sprintf(
            '"created":
            "%s"
                  ',
            self$`created`
          )
        },
        if (!is.null(self$`customModelVersionId`)) {
          sprintf(
            '"customModelVersionId":
            "%s"
                  ',
            self$`customModelVersionId`
          )
        },
        if (!is.null(self$`generatedMetadata`)) {
          sprintf(
            '"generatedMetadata":
            %s
      ',
            jsonlite::toJSON(self$`generatedMetadata`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`id`)) {
          sprintf(
            '"id":
            "%s"
                  ',
            self$`id`
          )
        },
        if (!is.null(self$`logMessage`)) {
          sprintf(
            '"logMessage":
            "%s"
                  ',
            self$`logMessage`
          )
        },
        if (!is.null(self$`mainProgramItemId`)) {
          sprintf(
            '"mainProgramItemId":
            "%s"
                  ',
            self$`mainProgramItemId`
          )
        },
        if (!is.null(self$`shouldStop`)) {
          sprintf(
            '"shouldStop":
            %s
                  ',
            tolower(self$`shouldStop`)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param ConversionResponseJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(ConversionResponseJson, validateParams = FALSE) {
      ConversionResponseObject <- jsonlite::fromJSON(ConversionResponseJson)
      self$`conversionInProgress` <- ConversionResponseObject$`conversionInProgress`
      self$`conversionSucceeded` <- ConversionResponseObject$`conversionSucceeded`
      self$`created` <- ConversionResponseObject$`created`
      self$`customModelVersionId` <- ConversionResponseObject$`customModelVersionId`
      self$`generatedMetadata` <- GeneratedMetadata$new()$fromJSON(jsonlite::toJSON(ConversionResponseObject$generatedMetadata, auto_unbox = TRUE, digits = NA, null = "null"))
      self$`id` <- ConversionResponseObject$`id`
      self$`logMessage` <- ConversionResponseObject$`logMessage`
      self$`mainProgramItemId` <- ConversionResponseObject$`mainProgramItemId`
      self$`shouldStop` <- ConversionResponseObject$`shouldStop`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
