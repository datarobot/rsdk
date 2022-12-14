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
#' @title DatasetRolesWithNames
#'
#' @description DatasetRolesWithNames Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field canShare  character [optional] Whether the org/group/user should be able to share with others. If true, the org/group/user will be able to grant any role up to and including their own to other orgs/groups/user. If &#x60;role&#x60; is &#x60;NO_ROLE&#x60; &#x60;canShare&#x60; is ignored.
#'
#' @field canUseData  character [optional] Whether the user/group/org should be able to view, download and process data (use to create projects, predictions, etc). For OWNER &#x60;canUseData&#x60; is always True. If &#x60;role&#x60; is empty &#x60;canUseData&#x60; is ignored.
#'
#' @field name  character Name of the user/group/org to update the access role for.
#'
#' @field role  character The role of the org/group/user on this dataset or \&quot;NO_ROLE\&quot; for removing access when used with route to modify access.
#'
#' @field shareRecipientType  character It describes the recipient type.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
DatasetRolesWithNames <- R6::R6Class(
  "DatasetRolesWithNames",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`canShare` = NULL, `canUseData` = NULL, `name` = NULL, `role` = NULL, `shareRecipientType` = NULL) {
      if (!is.null(`name`)) {
        stopifnot(is.character(`name`), length(`name`) == 1)
      }
      if (!is.null(`role`)) {
        stopifnot(is.character(`role`), length(`role`) == 1)
      }
      if (!is.null(`shareRecipientType`)) {
        stopifnot(is.character(`shareRecipientType`), length(`shareRecipientType`) == 1)
      }
      if (!is.null(`canShare`)) {
        stopifnot(is.logical(`canShare`), length(`canShare`) == 1)
      }
      if (!is.null(`canUseData`)) {
        stopifnot(is.logical(`canUseData`), length(`canUseData`) == 1)
      }
    }
  ),
  public = list(
    `canShare` = NULL,
    `canUseData` = NULL,
    `name` = NULL,
    `role` = NULL,
    `shareRecipientType` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param canShare Whether the org/group/user should be able to share with others. If true, the org/group/user will be able to grant any role up to and including their own to other orgs/groups/user. If &#x60;role&#x60; is &#x60;NO_ROLE&#x60; &#x60;canShare&#x60; is ignored.
    #' @param canUseData Whether the user/group/org should be able to view, download and process data (use to create projects, predictions, etc). For OWNER &#x60;canUseData&#x60; is always True. If &#x60;role&#x60; is empty &#x60;canUseData&#x60; is ignored.
    #' @param name Name of the user/group/org to update the access role for.
    #' @param role The role of the org/group/user on this dataset or \&quot;NO_ROLE\&quot; for removing access when used with route to modify access.
    #' @param shareRecipientType It describes the recipient type.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`name` = NULL, `role` = NULL, `shareRecipientType` = NULL, `canShare` = FALSE, `canUseData` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`name`, `role`, `shareRecipientType`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(canShare, canUseData, name, role, shareRecipientType)
      }
      self$`canShare` <- `canShare`
      self$`canUseData` <- `canUseData`
      self$`name` <- `name`
      self$`role` <- `role`
      self$`shareRecipientType` <- `shareRecipientType`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(canShare = self$`canShare`, canUseData = self$`canUseData`, name = self$`name`, role = self$`role`, shareRecipientType = self$`shareRecipientType`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`canShare`)) {
          sprintf(
            '"canShare":
            %s
                  ',
            tolower(self$`canShare`)
          )
        },
        if (!is.null(self$`canUseData`)) {
          sprintf(
            '"canUseData":
            %s
                  ',
            tolower(self$`canUseData`)
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
        if (!is.null(self$`role`)) {
          sprintf(
            '"role":
            "%s"
                  ',
            self$`role`
          )
        },
        if (!is.null(self$`shareRecipientType`)) {
          sprintf(
            '"shareRecipientType":
            "%s"
                  ',
            self$`shareRecipientType`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param DatasetRolesWithNamesJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(DatasetRolesWithNamesJson, validateParams = FALSE) {
      DatasetRolesWithNamesObject <- jsonlite::fromJSON(DatasetRolesWithNamesJson)
      self$`canShare` <- DatasetRolesWithNamesObject$`canShare`
      self$`canUseData` <- DatasetRolesWithNamesObject$`canUseData`
      self$`name` <- DatasetRolesWithNamesObject$`name`
      self$`role` <- DatasetRolesWithNamesObject$`role`
      self$`shareRecipientType` <- DatasetRolesWithNamesObject$`shareRecipientType`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
