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
#' @title DatasetSharedRoles
#'
#' @description DatasetSharedRoles Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field applyGrantToLinkedObjects  character [optional] If true for any users being granted access to the dataset, grant the user read access to any linked objects such as DataSources and DataStores that may be used by this dataset. Ignored if no such objects are relevant for dataset. Will not result in access being lowered for a user if the user already has higher access to linked objects than read access. However, if the target user does not have sharing permissions to the linked object, they will be given sharing access without lowering existing permissions. May result in an error if user making call does not have sufficient permissions to complete grant. Default value is false.
#'
#' @field operation  character The name of the action being taken. The only operation is \&quot;updateRoles\&quot;.
#'
#' @field roles  list( \link{OneOfDatasetRolesWithNamesDatasetRolesWithId} ) An array of RoleRequest objects. May contain at most 100 such objects.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
DatasetSharedRoles <- R6::R6Class(
  "DatasetSharedRoles",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`applyGrantToLinkedObjects` = NULL, `operation` = NULL, `roles` = NULL) {
      if (!is.null(`operation`)) {
        stopifnot(is.character(`operation`), length(`operation`) == 1)
      }
      if (!is.null(`roles`)) {
        .setComplexProperty(typeList = list(DatasetRolesWithNames, DatasetRolesWithId), propertyData = roles)
      }
      if (!is.null(`applyGrantToLinkedObjects`)) {
        stopifnot(is.logical(`applyGrantToLinkedObjects`), length(`applyGrantToLinkedObjects`) == 1)
      }
    }
  ),
  public = list(
    `applyGrantToLinkedObjects` = NULL,
    `operation` = NULL,
    `roles` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param applyGrantToLinkedObjects If true for any users being granted access to the dataset, grant the user read access to any linked objects such as DataSources and DataStores that may be used by this dataset. Ignored if no such objects are relevant for dataset. Will not result in access being lowered for a user if the user already has higher access to linked objects than read access. However, if the target user does not have sharing permissions to the linked object, they will be given sharing access without lowering existing permissions. May result in an error if user making call does not have sufficient permissions to complete grant. Default value is false.
    #' @param operation The name of the action being taken. The only operation is \&quot;updateRoles\&quot;.
    #' @param roles An array of RoleRequest objects. May contain at most 100 such objects.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`operation` = NULL, `roles` = NULL, `applyGrantToLinkedObjects` = FALSE, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`operation`, `roles`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(applyGrantToLinkedObjects, operation, roles)
      }
      self$`applyGrantToLinkedObjects` <- `applyGrantToLinkedObjects`
      self$`operation` <- `operation`
      self$`roles` <- sapply(`roles`, function(item) .setComplexProperty(typeList = list(DatasetRolesWithNames, DatasetRolesWithId), propertyData = item))
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(applyGrantToLinkedObjects = self$`applyGrantToLinkedObjects`, operation = self$`operation`, roles = self$`roles`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`applyGrantToLinkedObjects`)) {
          sprintf(
            '"applyGrantToLinkedObjects":
            %s
                  ',
            tolower(self$`applyGrantToLinkedObjects`)
          )
        },
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
    #' @param DatasetSharedRolesJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(DatasetSharedRolesJson, validateParams = FALSE) {
      DatasetSharedRolesObject <- jsonlite::fromJSON(DatasetSharedRolesJson)
      self$`applyGrantToLinkedObjects` <- DatasetSharedRolesObject$`applyGrantToLinkedObjects`
      self$`operation` <- DatasetSharedRolesObject$`operation`
      self$`roles` <- ApiClient$new()$deserializeObj(DatasetSharedRolesObject$`roles`, "array[OneOfDatasetRolesWithNamesDatasetRolesWithId]", loadNamespace("datarobot.apicore"))

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
