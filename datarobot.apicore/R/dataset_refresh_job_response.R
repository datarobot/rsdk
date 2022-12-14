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
#' @title DatasetRefreshJobResponse
#'
#' @description DatasetRefreshJobResponse Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field categories  \link{OneOfstringarray} An array of strings describing the intended use of the dataset.
#'
#' @field createdBy  character The user who created this dataset refresh job.
#'
#' @field credentialId  character ID used to validate with Kerberos authentication service if Kerberos is enabled.
#'
#' @field credentials  character A JSON string describing the data engine queries credentials to use when refreshing.
#'
#' @field datasetId  character ID of the dataset the user scheduled job applies to.
#'
#' @field enabled  character Indicates whether the scheduled job is active (true) or inactive(false).
#'
#' @field jobId  character The scheduled job ID.
#'
#' @field name  character The scheduled job&#39;s name.
#'
#' @field schedule  \link{DatasetRefresh}
#'
#' @field scheduleReferenceDate  character The UTC reference date in RFC-3339 format of when the schedule starts from. Can be used to help build a more intuitive schedule picker.
#'
#' @field updatedAt  character The UTC date in RFC-3339 format of when the job was last updated.
#'
#' @field updatedBy  character The user who last modified this dataset refresh job.
#'
#' @field useKerberos  character Boolean (true) if the Kerberos authentication service is needed when refreshing a job.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
DatasetRefreshJobResponse <- R6::R6Class(
  "DatasetRefreshJobResponse",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`categories` = NULL, `createdBy` = NULL, `credentialId` = NULL, `credentials` = NULL, `datasetId` = NULL, `enabled` = NULL, `jobId` = NULL, `name` = NULL, `schedule` = NULL, `scheduleReferenceDate` = NULL, `updatedAt` = NULL, `updatedBy` = NULL, `useKerberos` = NULL) {
      if (!is.null(`categories`)) {
        .setPrimitiveProperty(typeList = list("character", "array"), propertyData = categories)
      }
      if (!is.null(`createdBy`)) {
        stopifnot(is.character(`createdBy`), length(`createdBy`) == 1)
      }
      if (!is.null(`credentialId`)) {
        stopifnot(is.character(`credentialId`), length(`credentialId`) == 1)
      }
      if (!is.null(`credentials`)) {
        stopifnot(is.character(`credentials`), length(`credentials`) == 1)
      }
      if (!is.null(`datasetId`)) {
        stopifnot(is.character(`datasetId`), length(`datasetId`) == 1)
      }
      if (!is.null(`enabled`)) {
        stopifnot(is.logical(`enabled`), length(`enabled`) == 1)
      }
      if (!is.null(`jobId`)) {
        stopifnot(is.character(`jobId`), length(`jobId`) == 1)
      }
      if (!is.null(`name`)) {
        stopifnot(is.character(`name`), length(`name`) == 1)
      }
      if (!is.null(`schedule`)) {
        stopifnot(R6::is.R6(`schedule`))
      }
      if (!is.null(`scheduleReferenceDate`)) {
        stopifnot(inherits(`scheduleReferenceDate`, "POSIXt"))
      }
      if (!is.null(`updatedAt`)) {
        stopifnot(inherits(`updatedAt`, "POSIXt"))
      }
      if (!is.null(`updatedBy`)) {
        stopifnot(is.character(`updatedBy`), length(`updatedBy`) == 1)
      }
      if (!is.null(`useKerberos`)) {
        stopifnot(is.logical(`useKerberos`), length(`useKerberos`) == 1)
      }
    }
  ),
  public = list(
    `categories` = NULL,
    `createdBy` = NULL,
    `credentialId` = NULL,
    `credentials` = NULL,
    `datasetId` = NULL,
    `enabled` = NULL,
    `jobId` = NULL,
    `name` = NULL,
    `schedule` = NULL,
    `scheduleReferenceDate` = NULL,
    `updatedAt` = NULL,
    `updatedBy` = NULL,
    `useKerberos` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param categories An array of strings describing the intended use of the dataset.
    #' @param createdBy The user who created this dataset refresh job.
    #' @param credentialId ID used to validate with Kerberos authentication service if Kerberos is enabled.
    #' @param credentials A JSON string describing the data engine queries credentials to use when refreshing.
    #' @param datasetId ID of the dataset the user scheduled job applies to.
    #' @param enabled Indicates whether the scheduled job is active (true) or inactive(false).
    #' @param jobId The scheduled job ID.
    #' @param name The scheduled job&#39;s name.
    #' @param schedule
    #' @param scheduleReferenceDate The UTC reference date in RFC-3339 format of when the schedule starts from. Can be used to help build a more intuitive schedule picker.
    #' @param updatedAt The UTC date in RFC-3339 format of when the job was last updated.
    #' @param updatedBy The user who last modified this dataset refresh job.
    #' @param useKerberos Boolean (true) if the Kerberos authentication service is needed when refreshing a job.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`categories` = NULL, `createdBy` = NULL, `credentialId` = NULL, `credentials` = NULL, `datasetId` = NULL, `enabled` = NULL, `jobId` = NULL, `name` = NULL, `schedule` = NULL, `scheduleReferenceDate` = NULL, `updatedAt` = NULL, `updatedBy` = NULL, `useKerberos` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`categories`, `createdBy`, `credentialId`, `credentials`, `datasetId`, `enabled`, `jobId`, `name`, `schedule`, `scheduleReferenceDate`, `updatedAt`, `updatedBy`, `useKerberos`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(categories, createdBy, credentialId, credentials, datasetId, enabled, jobId, name, schedule, scheduleReferenceDate, updatedAt, updatedBy, useKerberos)
      }
      self$`categories` <- .setPrimitiveProperty(typeList = list("character", "array"), propertyData = categories)
      self$`createdBy` <- `createdBy`
      self$`credentialId` <- `credentialId`
      self$`credentials` <- `credentials`
      self$`datasetId` <- `datasetId`
      self$`enabled` <- `enabled`
      self$`jobId` <- `jobId`
      self$`name` <- `name`
      self$`schedule` <- `schedule`
      self$`scheduleReferenceDate` <- `scheduleReferenceDate`
      self$`updatedAt` <- `updatedAt`
      self$`updatedBy` <- `updatedBy`
      self$`useKerberos` <- `useKerberos`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(categories = self$`categories`, createdBy = self$`createdBy`, credentialId = self$`credentialId`, credentials = self$`credentials`, datasetId = self$`datasetId`, enabled = self$`enabled`, jobId = self$`jobId`, name = self$`name`, schedule = self$`schedule`, scheduleReferenceDate = self$`scheduleReferenceDate`, updatedAt = self$`updatedAt`, updatedBy = self$`updatedBy`, useKerberos = self$`useKerberos`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`categories`)) {
          sprintf(
            '"categories":
            %s
      ',
            self$`categories`
          )
        },
        if (!is.null(self$`createdBy`)) {
          sprintf(
            '"createdBy":
            "%s"
                  ',
            self$`createdBy`
          )
        },
        if (!is.null(self$`credentialId`)) {
          sprintf(
            '"credentialId":
            "%s"
                  ',
            self$`credentialId`
          )
        },
        if (!is.null(self$`credentials`)) {
          sprintf(
            '"credentials":
            "%s"
                  ',
            self$`credentials`
          )
        },
        if (!is.null(self$`datasetId`)) {
          sprintf(
            '"datasetId":
            "%s"
                  ',
            self$`datasetId`
          )
        },
        if (!is.null(self$`enabled`)) {
          sprintf(
            '"enabled":
            %s
                  ',
            tolower(self$`enabled`)
          )
        },
        if (!is.null(self$`jobId`)) {
          sprintf(
            '"jobId":
            "%s"
                  ',
            self$`jobId`
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
        if (!is.null(self$`schedule`)) {
          sprintf(
            '"schedule":
            %s
      ',
            jsonlite::toJSON(self$`schedule`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`scheduleReferenceDate`)) {
          sprintf(
            '"scheduleReferenceDate":
            "%s"
                  ',
            format(self$`scheduleReferenceDate`, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
          )
        },
        if (!is.null(self$`updatedAt`)) {
          sprintf(
            '"updatedAt":
            "%s"
                  ',
            format(self$`updatedAt`, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
          )
        },
        if (!is.null(self$`updatedBy`)) {
          sprintf(
            '"updatedBy":
            "%s"
                  ',
            self$`updatedBy`
          )
        },
        if (!is.null(self$`useKerberos`)) {
          sprintf(
            '"useKerberos":
            %s
                  ',
            tolower(self$`useKerberos`)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param DatasetRefreshJobResponseJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(DatasetRefreshJobResponseJson, validateParams = FALSE) {
      DatasetRefreshJobResponseObject <- jsonlite::fromJSON(DatasetRefreshJobResponseJson)
      self$`categories` <- .setPrimitiveProperty(typeList = list("character", "array"), propertyData = DatasetRefreshJobResponseObject$categories)
      self$`createdBy` <- DatasetRefreshJobResponseObject$`createdBy`
      self$`credentialId` <- DatasetRefreshJobResponseObject$`credentialId`
      self$`credentials` <- DatasetRefreshJobResponseObject$`credentials`
      self$`datasetId` <- DatasetRefreshJobResponseObject$`datasetId`
      self$`enabled` <- DatasetRefreshJobResponseObject$`enabled`
      self$`jobId` <- DatasetRefreshJobResponseObject$`jobId`
      self$`name` <- DatasetRefreshJobResponseObject$`name`
      self$`schedule` <- DatasetRefresh$new()$fromJSON(jsonlite::toJSON(DatasetRefreshJobResponseObject$schedule, auto_unbox = TRUE, digits = NA, null = "null"))
      self$`scheduleReferenceDate` <- ParseRFC3339Timestamp(DatasetRefreshJobResponseObject$`scheduleReferenceDate`)
      self$`updatedAt` <- ParseRFC3339Timestamp(DatasetRefreshJobResponseObject$`updatedAt`)
      self$`updatedBy` <- DatasetRefreshJobResponseObject$`updatedBy`
      self$`useKerberos` <- DatasetRefreshJobResponseObject$`useKerberos`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
