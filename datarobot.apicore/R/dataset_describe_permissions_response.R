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
#' @title DatasetDescribePermissionsResponse
#'
#' @description DatasetDescribePermissionsResponse Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field canCreateFeaturelist  character True if the user can create a new featurelist for this dataset.
#'
#' @field canDeleteDataset  character True if the user can delete dataset.
#'
#' @field canDownloadDatasetData  character True if the user can download data.
#'
#' @field canGetCatalogItemInfo  character True if the user can view catalog info.
#'
#' @field canGetDatasetInfo  character True if the user can view dataset info.
#'
#' @field canGetDatasetPermissions  character True if the user can view dataset permissions.
#'
#' @field canGetFeatureInfo  character True if the user can retrieve feature info of dataset.
#'
#' @field canGetFeaturelists  character True if the user can view featurelist for this dataset.
#'
#' @field canPatchCatalogInfo  character True if the user can modify catalog info.
#'
#' @field canPatchDatasetAliases  character True if the user can modify dataset feature aliases.
#'
#' @field canPatchDatasetInfo  character True if the user can modify dataset info.
#'
#' @field canPatchDatasetPermissions  character True if the user can modify dataset permissions.
#'
#' @field canPatchFeaturelists  character True if the user can modify featurelists for this dataset.
#'
#' @field canPostDataset  character True if the user can create a new dataset.
#'
#' @field canReloadDataset  character True if the user can reload dataset.
#'
#' @field canShareDataset  character True if the user can share the dataset.
#'
#' @field canSnapshotDataset  character True if the user can save snapshot of dataset.
#'
#' @field canUndeleteDataset  character True if the user can undelete dataset.
#'
#' @field canUseDatasetData  character True if the user can use dataset data to create projects, train custom models or provide predictions.
#'
#' @field canUseFeaturelists  character True if the user can use featurelists for this dataset. (for project creation)
#'
#' @field datasetId  character The ID of the dataset.
#'
#' @field uid  character The ID of the user identified by username.
#'
#' @field username  character &#x60;username&#x60; of a user with access to this dataset.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
DatasetDescribePermissionsResponse <- R6::R6Class(
  "DatasetDescribePermissionsResponse",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`canCreateFeaturelist` = NULL, `canDeleteDataset` = NULL, `canDownloadDatasetData` = NULL, `canGetCatalogItemInfo` = NULL, `canGetDatasetInfo` = NULL, `canGetDatasetPermissions` = NULL, `canGetFeatureInfo` = NULL, `canGetFeaturelists` = NULL, `canPatchCatalogInfo` = NULL, `canPatchDatasetAliases` = NULL, `canPatchDatasetInfo` = NULL, `canPatchDatasetPermissions` = NULL, `canPatchFeaturelists` = NULL, `canPostDataset` = NULL, `canReloadDataset` = NULL, `canShareDataset` = NULL, `canSnapshotDataset` = NULL, `canUndeleteDataset` = NULL, `canUseDatasetData` = NULL, `canUseFeaturelists` = NULL, `datasetId` = NULL, `uid` = NULL, `username` = NULL) {
      if (!is.null(`canCreateFeaturelist`)) {
        stopifnot(is.logical(`canCreateFeaturelist`), length(`canCreateFeaturelist`) == 1)
      }
      if (!is.null(`canDeleteDataset`)) {
        stopifnot(is.logical(`canDeleteDataset`), length(`canDeleteDataset`) == 1)
      }
      if (!is.null(`canDownloadDatasetData`)) {
        stopifnot(is.logical(`canDownloadDatasetData`), length(`canDownloadDatasetData`) == 1)
      }
      if (!is.null(`canGetCatalogItemInfo`)) {
        stopifnot(is.logical(`canGetCatalogItemInfo`), length(`canGetCatalogItemInfo`) == 1)
      }
      if (!is.null(`canGetDatasetInfo`)) {
        stopifnot(is.logical(`canGetDatasetInfo`), length(`canGetDatasetInfo`) == 1)
      }
      if (!is.null(`canGetDatasetPermissions`)) {
        stopifnot(is.logical(`canGetDatasetPermissions`), length(`canGetDatasetPermissions`) == 1)
      }
      if (!is.null(`canGetFeatureInfo`)) {
        stopifnot(is.logical(`canGetFeatureInfo`), length(`canGetFeatureInfo`) == 1)
      }
      if (!is.null(`canGetFeaturelists`)) {
        stopifnot(is.logical(`canGetFeaturelists`), length(`canGetFeaturelists`) == 1)
      }
      if (!is.null(`canPatchCatalogInfo`)) {
        stopifnot(is.logical(`canPatchCatalogInfo`), length(`canPatchCatalogInfo`) == 1)
      }
      if (!is.null(`canPatchDatasetAliases`)) {
        stopifnot(is.logical(`canPatchDatasetAliases`), length(`canPatchDatasetAliases`) == 1)
      }
      if (!is.null(`canPatchDatasetInfo`)) {
        stopifnot(is.logical(`canPatchDatasetInfo`), length(`canPatchDatasetInfo`) == 1)
      }
      if (!is.null(`canPatchDatasetPermissions`)) {
        stopifnot(is.logical(`canPatchDatasetPermissions`), length(`canPatchDatasetPermissions`) == 1)
      }
      if (!is.null(`canPatchFeaturelists`)) {
        stopifnot(is.logical(`canPatchFeaturelists`), length(`canPatchFeaturelists`) == 1)
      }
      if (!is.null(`canPostDataset`)) {
        stopifnot(is.logical(`canPostDataset`), length(`canPostDataset`) == 1)
      }
      if (!is.null(`canReloadDataset`)) {
        stopifnot(is.logical(`canReloadDataset`), length(`canReloadDataset`) == 1)
      }
      if (!is.null(`canShareDataset`)) {
        stopifnot(is.logical(`canShareDataset`), length(`canShareDataset`) == 1)
      }
      if (!is.null(`canSnapshotDataset`)) {
        stopifnot(is.logical(`canSnapshotDataset`), length(`canSnapshotDataset`) == 1)
      }
      if (!is.null(`canUndeleteDataset`)) {
        stopifnot(is.logical(`canUndeleteDataset`), length(`canUndeleteDataset`) == 1)
      }
      if (!is.null(`canUseDatasetData`)) {
        stopifnot(is.logical(`canUseDatasetData`), length(`canUseDatasetData`) == 1)
      }
      if (!is.null(`canUseFeaturelists`)) {
        stopifnot(is.logical(`canUseFeaturelists`), length(`canUseFeaturelists`) == 1)
      }
      if (!is.null(`datasetId`)) {
        stopifnot(is.character(`datasetId`), length(`datasetId`) == 1)
      }
      if (!is.null(`uid`)) {
        stopifnot(is.character(`uid`), length(`uid`) == 1)
      }
      if (!is.null(`username`)) {
        stopifnot(is.character(`username`), length(`username`) == 1)
      }
    }
  ),
  public = list(
    `canCreateFeaturelist` = NULL,
    `canDeleteDataset` = NULL,
    `canDownloadDatasetData` = NULL,
    `canGetCatalogItemInfo` = NULL,
    `canGetDatasetInfo` = NULL,
    `canGetDatasetPermissions` = NULL,
    `canGetFeatureInfo` = NULL,
    `canGetFeaturelists` = NULL,
    `canPatchCatalogInfo` = NULL,
    `canPatchDatasetAliases` = NULL,
    `canPatchDatasetInfo` = NULL,
    `canPatchDatasetPermissions` = NULL,
    `canPatchFeaturelists` = NULL,
    `canPostDataset` = NULL,
    `canReloadDataset` = NULL,
    `canShareDataset` = NULL,
    `canSnapshotDataset` = NULL,
    `canUndeleteDataset` = NULL,
    `canUseDatasetData` = NULL,
    `canUseFeaturelists` = NULL,
    `datasetId` = NULL,
    `uid` = NULL,
    `username` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param canCreateFeaturelist True if the user can create a new featurelist for this dataset.
    #' @param canDeleteDataset True if the user can delete dataset.
    #' @param canDownloadDatasetData True if the user can download data.
    #' @param canGetCatalogItemInfo True if the user can view catalog info.
    #' @param canGetDatasetInfo True if the user can view dataset info.
    #' @param canGetDatasetPermissions True if the user can view dataset permissions.
    #' @param canGetFeatureInfo True if the user can retrieve feature info of dataset.
    #' @param canGetFeaturelists True if the user can view featurelist for this dataset.
    #' @param canPatchCatalogInfo True if the user can modify catalog info.
    #' @param canPatchDatasetAliases True if the user can modify dataset feature aliases.
    #' @param canPatchDatasetInfo True if the user can modify dataset info.
    #' @param canPatchDatasetPermissions True if the user can modify dataset permissions.
    #' @param canPatchFeaturelists True if the user can modify featurelists for this dataset.
    #' @param canPostDataset True if the user can create a new dataset.
    #' @param canReloadDataset True if the user can reload dataset.
    #' @param canShareDataset True if the user can share the dataset.
    #' @param canSnapshotDataset True if the user can save snapshot of dataset.
    #' @param canUndeleteDataset True if the user can undelete dataset.
    #' @param canUseDatasetData True if the user can use dataset data to create projects, train custom models or provide predictions.
    #' @param canUseFeaturelists True if the user can use featurelists for this dataset. (for project creation)
    #' @param datasetId The ID of the dataset.
    #' @param uid The ID of the user identified by username.
    #' @param username &#x60;username&#x60; of a user with access to this dataset.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`canCreateFeaturelist` = NULL, `canDeleteDataset` = NULL, `canDownloadDatasetData` = NULL, `canGetCatalogItemInfo` = NULL, `canGetDatasetInfo` = NULL, `canGetDatasetPermissions` = NULL, `canGetFeatureInfo` = NULL, `canGetFeaturelists` = NULL, `canPatchCatalogInfo` = NULL, `canPatchDatasetAliases` = NULL, `canPatchDatasetInfo` = NULL, `canPatchDatasetPermissions` = NULL, `canPatchFeaturelists` = NULL, `canPostDataset` = NULL, `canReloadDataset` = NULL, `canShareDataset` = NULL, `canSnapshotDataset` = NULL, `canUndeleteDataset` = NULL, `canUseDatasetData` = NULL, `canUseFeaturelists` = NULL, `datasetId` = NULL, `uid` = NULL, `username` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`canCreateFeaturelist`, `canDeleteDataset`, `canDownloadDatasetData`, `canGetCatalogItemInfo`, `canGetDatasetInfo`, `canGetDatasetPermissions`, `canGetFeatureInfo`, `canGetFeaturelists`, `canPatchCatalogInfo`, `canPatchDatasetAliases`, `canPatchDatasetInfo`, `canPatchDatasetPermissions`, `canPatchFeaturelists`, `canPostDataset`, `canReloadDataset`, `canShareDataset`, `canSnapshotDataset`, `canUndeleteDataset`, `canUseDatasetData`, `canUseFeaturelists`, `datasetId`, `uid`, `username`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(canCreateFeaturelist, canDeleteDataset, canDownloadDatasetData, canGetCatalogItemInfo, canGetDatasetInfo, canGetDatasetPermissions, canGetFeatureInfo, canGetFeaturelists, canPatchCatalogInfo, canPatchDatasetAliases, canPatchDatasetInfo, canPatchDatasetPermissions, canPatchFeaturelists, canPostDataset, canReloadDataset, canShareDataset, canSnapshotDataset, canUndeleteDataset, canUseDatasetData, canUseFeaturelists, datasetId, uid, username)
      }
      self$`canCreateFeaturelist` <- `canCreateFeaturelist`
      self$`canDeleteDataset` <- `canDeleteDataset`
      self$`canDownloadDatasetData` <- `canDownloadDatasetData`
      self$`canGetCatalogItemInfo` <- `canGetCatalogItemInfo`
      self$`canGetDatasetInfo` <- `canGetDatasetInfo`
      self$`canGetDatasetPermissions` <- `canGetDatasetPermissions`
      self$`canGetFeatureInfo` <- `canGetFeatureInfo`
      self$`canGetFeaturelists` <- `canGetFeaturelists`
      self$`canPatchCatalogInfo` <- `canPatchCatalogInfo`
      self$`canPatchDatasetAliases` <- `canPatchDatasetAliases`
      self$`canPatchDatasetInfo` <- `canPatchDatasetInfo`
      self$`canPatchDatasetPermissions` <- `canPatchDatasetPermissions`
      self$`canPatchFeaturelists` <- `canPatchFeaturelists`
      self$`canPostDataset` <- `canPostDataset`
      self$`canReloadDataset` <- `canReloadDataset`
      self$`canShareDataset` <- `canShareDataset`
      self$`canSnapshotDataset` <- `canSnapshotDataset`
      self$`canUndeleteDataset` <- `canUndeleteDataset`
      self$`canUseDatasetData` <- `canUseDatasetData`
      self$`canUseFeaturelists` <- `canUseFeaturelists`
      self$`datasetId` <- `datasetId`
      self$`uid` <- `uid`
      self$`username` <- `username`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(canCreateFeaturelist = self$`canCreateFeaturelist`, canDeleteDataset = self$`canDeleteDataset`, canDownloadDatasetData = self$`canDownloadDatasetData`, canGetCatalogItemInfo = self$`canGetCatalogItemInfo`, canGetDatasetInfo = self$`canGetDatasetInfo`, canGetDatasetPermissions = self$`canGetDatasetPermissions`, canGetFeatureInfo = self$`canGetFeatureInfo`, canGetFeaturelists = self$`canGetFeaturelists`, canPatchCatalogInfo = self$`canPatchCatalogInfo`, canPatchDatasetAliases = self$`canPatchDatasetAliases`, canPatchDatasetInfo = self$`canPatchDatasetInfo`, canPatchDatasetPermissions = self$`canPatchDatasetPermissions`, canPatchFeaturelists = self$`canPatchFeaturelists`, canPostDataset = self$`canPostDataset`, canReloadDataset = self$`canReloadDataset`, canShareDataset = self$`canShareDataset`, canSnapshotDataset = self$`canSnapshotDataset`, canUndeleteDataset = self$`canUndeleteDataset`, canUseDatasetData = self$`canUseDatasetData`, canUseFeaturelists = self$`canUseFeaturelists`, datasetId = self$`datasetId`, uid = self$`uid`, username = self$`username`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`canCreateFeaturelist`)) {
          sprintf(
            '"canCreateFeaturelist":
            %s
                  ',
            tolower(self$`canCreateFeaturelist`)
          )
        },
        if (!is.null(self$`canDeleteDataset`)) {
          sprintf(
            '"canDeleteDataset":
            %s
                  ',
            tolower(self$`canDeleteDataset`)
          )
        },
        if (!is.null(self$`canDownloadDatasetData`)) {
          sprintf(
            '"canDownloadDatasetData":
            %s
                  ',
            tolower(self$`canDownloadDatasetData`)
          )
        },
        if (!is.null(self$`canGetCatalogItemInfo`)) {
          sprintf(
            '"canGetCatalogItemInfo":
            %s
                  ',
            tolower(self$`canGetCatalogItemInfo`)
          )
        },
        if (!is.null(self$`canGetDatasetInfo`)) {
          sprintf(
            '"canGetDatasetInfo":
            %s
                  ',
            tolower(self$`canGetDatasetInfo`)
          )
        },
        if (!is.null(self$`canGetDatasetPermissions`)) {
          sprintf(
            '"canGetDatasetPermissions":
            %s
                  ',
            tolower(self$`canGetDatasetPermissions`)
          )
        },
        if (!is.null(self$`canGetFeatureInfo`)) {
          sprintf(
            '"canGetFeatureInfo":
            %s
                  ',
            tolower(self$`canGetFeatureInfo`)
          )
        },
        if (!is.null(self$`canGetFeaturelists`)) {
          sprintf(
            '"canGetFeaturelists":
            %s
                  ',
            tolower(self$`canGetFeaturelists`)
          )
        },
        if (!is.null(self$`canPatchCatalogInfo`)) {
          sprintf(
            '"canPatchCatalogInfo":
            %s
                  ',
            tolower(self$`canPatchCatalogInfo`)
          )
        },
        if (!is.null(self$`canPatchDatasetAliases`)) {
          sprintf(
            '"canPatchDatasetAliases":
            %s
                  ',
            tolower(self$`canPatchDatasetAliases`)
          )
        },
        if (!is.null(self$`canPatchDatasetInfo`)) {
          sprintf(
            '"canPatchDatasetInfo":
            %s
                  ',
            tolower(self$`canPatchDatasetInfo`)
          )
        },
        if (!is.null(self$`canPatchDatasetPermissions`)) {
          sprintf(
            '"canPatchDatasetPermissions":
            %s
                  ',
            tolower(self$`canPatchDatasetPermissions`)
          )
        },
        if (!is.null(self$`canPatchFeaturelists`)) {
          sprintf(
            '"canPatchFeaturelists":
            %s
                  ',
            tolower(self$`canPatchFeaturelists`)
          )
        },
        if (!is.null(self$`canPostDataset`)) {
          sprintf(
            '"canPostDataset":
            %s
                  ',
            tolower(self$`canPostDataset`)
          )
        },
        if (!is.null(self$`canReloadDataset`)) {
          sprintf(
            '"canReloadDataset":
            %s
                  ',
            tolower(self$`canReloadDataset`)
          )
        },
        if (!is.null(self$`canShareDataset`)) {
          sprintf(
            '"canShareDataset":
            %s
                  ',
            tolower(self$`canShareDataset`)
          )
        },
        if (!is.null(self$`canSnapshotDataset`)) {
          sprintf(
            '"canSnapshotDataset":
            %s
                  ',
            tolower(self$`canSnapshotDataset`)
          )
        },
        if (!is.null(self$`canUndeleteDataset`)) {
          sprintf(
            '"canUndeleteDataset":
            %s
                  ',
            tolower(self$`canUndeleteDataset`)
          )
        },
        if (!is.null(self$`canUseDatasetData`)) {
          sprintf(
            '"canUseDatasetData":
            %s
                  ',
            tolower(self$`canUseDatasetData`)
          )
        },
        if (!is.null(self$`canUseFeaturelists`)) {
          sprintf(
            '"canUseFeaturelists":
            %s
                  ',
            tolower(self$`canUseFeaturelists`)
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
        if (!is.null(self$`uid`)) {
          sprintf(
            '"uid":
            "%s"
                  ',
            self$`uid`
          )
        },
        if (!is.null(self$`username`)) {
          sprintf(
            '"username":
            "%s"
                  ',
            self$`username`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param DatasetDescribePermissionsResponseJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(DatasetDescribePermissionsResponseJson, validateParams = FALSE) {
      DatasetDescribePermissionsResponseObject <- jsonlite::fromJSON(DatasetDescribePermissionsResponseJson)
      self$`canCreateFeaturelist` <- DatasetDescribePermissionsResponseObject$`canCreateFeaturelist`
      self$`canDeleteDataset` <- DatasetDescribePermissionsResponseObject$`canDeleteDataset`
      self$`canDownloadDatasetData` <- DatasetDescribePermissionsResponseObject$`canDownloadDatasetData`
      self$`canGetCatalogItemInfo` <- DatasetDescribePermissionsResponseObject$`canGetCatalogItemInfo`
      self$`canGetDatasetInfo` <- DatasetDescribePermissionsResponseObject$`canGetDatasetInfo`
      self$`canGetDatasetPermissions` <- DatasetDescribePermissionsResponseObject$`canGetDatasetPermissions`
      self$`canGetFeatureInfo` <- DatasetDescribePermissionsResponseObject$`canGetFeatureInfo`
      self$`canGetFeaturelists` <- DatasetDescribePermissionsResponseObject$`canGetFeaturelists`
      self$`canPatchCatalogInfo` <- DatasetDescribePermissionsResponseObject$`canPatchCatalogInfo`
      self$`canPatchDatasetAliases` <- DatasetDescribePermissionsResponseObject$`canPatchDatasetAliases`
      self$`canPatchDatasetInfo` <- DatasetDescribePermissionsResponseObject$`canPatchDatasetInfo`
      self$`canPatchDatasetPermissions` <- DatasetDescribePermissionsResponseObject$`canPatchDatasetPermissions`
      self$`canPatchFeaturelists` <- DatasetDescribePermissionsResponseObject$`canPatchFeaturelists`
      self$`canPostDataset` <- DatasetDescribePermissionsResponseObject$`canPostDataset`
      self$`canReloadDataset` <- DatasetDescribePermissionsResponseObject$`canReloadDataset`
      self$`canShareDataset` <- DatasetDescribePermissionsResponseObject$`canShareDataset`
      self$`canSnapshotDataset` <- DatasetDescribePermissionsResponseObject$`canSnapshotDataset`
      self$`canUndeleteDataset` <- DatasetDescribePermissionsResponseObject$`canUndeleteDataset`
      self$`canUseDatasetData` <- DatasetDescribePermissionsResponseObject$`canUseDatasetData`
      self$`canUseFeaturelists` <- DatasetDescribePermissionsResponseObject$`canUseFeaturelists`
      self$`datasetId` <- DatasetDescribePermissionsResponseObject$`datasetId`
      self$`uid` <- DatasetDescribePermissionsResponseObject$`uid`
      self$`username` <- DatasetDescribePermissionsResponseObject$`username`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
