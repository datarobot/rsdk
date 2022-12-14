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


# NOTE: This file is auto generated by OpenAPI Generator (https://openapi-generator.tech).
# Do not edit the file manually.

# API Wrapper methods for Infrastructure
# To use these methods without modification, DR endpoint and token
# should be set as environment variables. This will automatically
# happen when you call `datarobot::ConnectToDataRobot()`.



#' ListClusterLicense
#'
#' Retrieve the information about cluster license
#'
#' Retrieve the information about the currently deployed cluster license.
#' @details This method invokes `GET /clusterLicense/` in the DataRobot Public API.
#'
#' @param ... Additional keyword arguments to be passed on to the `datarobot.apicore` and `httr` libraries.
#'
#' @seealso The method ClusterLicenseList in [datarobot.apicore::InfrastructureApi], which invokes the same underlying API endpoint.
#' @export
#' @md
ListClusterLicense <- function(...) {
  return(datarobot.apicore::InfrastructureApi$new()$ClusterLicenseList(...))
}

#' PutManyClusterLicense
#'
#' Create or replace license
#'
#' Create or replace the cluster license.
#' @details This method invokes `PUT /clusterLicense/` in the DataRobot Public API.
#'
#' @param ... Additional keyword arguments to be passed on to the `datarobot.apicore` and `httr` libraries.
#'
#' @seealso The method ClusterLicensePutMany in [datarobot.apicore::InfrastructureApi], which invokes the same underlying API endpoint.
#' @export
#' @md
PutManyClusterLicense <- function(licenseKey, ...) {
  clusterLicenseUpdate <- datarobot.apicore::ClusterLicenseUpdate$new(licenseKey = licenseKey, validateParams = TRUE)
  return(datarobot.apicore::InfrastructureApi$new()$ClusterLicensePutMany(clusterLicenseUpdate = clusterLicenseUpdate, ...))
}

#' CreateClusterLicenseValidation
#'
#' Check if a license is valid
#'
#' Check if a cluster license is valid.
#' @details This method invokes `POST /clusterLicenseValidation/` in the DataRobot Public API.
#'
#' @param ... Additional keyword arguments to be passed on to the `datarobot.apicore` and `httr` libraries.
#'
#' @seealso The method ClusterLicenseValidationCreate in [datarobot.apicore::InfrastructureApi], which invokes the same underlying API endpoint.
#' @export
#' @md
CreateClusterLicenseValidation <- function(licenseKey, ...) {
  clusterLicenseUpdate <- datarobot.apicore::ClusterLicenseUpdate$new(licenseKey = licenseKey, validateParams = TRUE)
  return(datarobot.apicore::InfrastructureApi$new()$ClusterLicenseValidationCreate(clusterLicenseUpdate = clusterLicenseUpdate, ...))
}

#' GetServerVersion
#'
#' Retrieve version information.
#'
#' Provides the version information for the current version of the API.
#' @details This method invokes `GET /version/` in the DataRobot Public API.
#'
#' @param ... Additional keyword arguments to be passed on to the `datarobot.apicore` and `httr` libraries.
#'
#' @seealso The method VersionList in [datarobot.apicore::InfrastructureApi], which invokes the same underlying API endpoint.
#' @export
#' @md
GetServerVersion <- function(...) {
  dataRobotUrl <- Sys.getenv("DATAROBOT_API_ENDPOINT")
  errorMessage <-
    paste(
      "Server did not reply with an API version. This may indicate the endpoint ", dataRobotUrl,
      "\n is misconfigured, or that the server API version precedes this version \n  ",
      "of the DataRobot client package and is likely incompatible."
    )
  ver <- tryCatch(
    {
      routeString <- UrlJoin("version")
      # start of generated addition
      # end of generated addition
      # start of generated addition
      # end of generated addition
      modelInfo <- DataRobotGET(routeString)
    },
    ConfigError = function(e) {
      warning(errorMessage)
      ver <- NULL
    }
  )
  ver
}
