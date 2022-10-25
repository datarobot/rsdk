# Copyright 2021 DataRobot, Inc. and its affiliates.
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
# The version of the OpenAPI document: 2.28.0
# Contact: api-maintainer@datarobot.com
# Generated by: https://openapi-generator.tech


# NOTE: This file is auto generated by OpenAPI Generator (https://openapi-generator.tech).
# Do not edit the file manually.

# API Wrapper methods for Applications
# To use these methods without modification, DR endpoint and token
# should be set as environment variables. This will automatically
# happen when you call `datarobot::ConnectToDataRobot()`.



#' ListApplicationTypes
#'
#' List of available application types to deploy.
#'
#' List of available application types to deploy.
#'
#' @seealso The method ApplicationTypesList in [datarobot.apicore::ApplicationsApi], which this function wraps.
#' @family Applications
#' @export
ListApplicationTypes <- function(offset = 0, limit = 0, ...) {
  return(datarobot.apicore::ApplicationsApi$new()$ApplicationTypesList(offset = offset, limit = limit))
}

#' RetrieveApplicationTypes
#'
#' Retrieve one application type
#'
#' Retrieve one application type
#'
#' @seealso The method ApplicationTypesRetrieve in [datarobot.apicore::ApplicationsApi], which this function wraps.
#' @family Applications
#' @export
RetrieveApplicationTypes <- function(applicationTypeId, ...) {
  return(datarobot.apicore::ApplicationsApi$new()$ApplicationTypesRetrieve(applicationTypeId = applicationTypeId))
}

#' ListApplicationTypesUsecases
#'
#' List application type usecases
#'
#' List of available usecases for application type.
#'
#' @seealso The method ApplicationTypesUsecasesList in [datarobot.apicore::ApplicationsApi], which this function wraps.
#' @family Applications
#' @export
ListApplicationTypesUsecases <- function(applicationTypeId, offset = 0, limit = 100, ...) {
  return(datarobot.apicore::ApplicationsApi$new()$ApplicationTypesUsecasesList(offset = offset, applicationTypeId = applicationTypeId, limit = limit))
}

#' RetrieveApplicationUserRole
#'
#' Get application user role
#'
#' Get application user role
#'
#' @seealso The method ApplicationUserRoleRetrieve in [datarobot.apicore::ApplicationsApi], which this function wraps.
#' @family Applications
#' @export
RetrieveApplicationUserRole <- function(applicationId, ...) {
  return(datarobot.apicore::ApplicationsApi$new()$ApplicationUserRoleRetrieve(applicationId = applicationId))
}

#' ListApplicationsAccessControl
#'
#' A list of users with access to this application
#'
#' A list of users who have access to this application and their roles
#'
#' @seealso The method ApplicationsAccessControlList in [datarobot.apicore::ApplicationsApi], which this function wraps.
#' @family Applications
#' @export
ListApplicationsAccessControl <- function(applicationId, userId, username, offset = 0, limit = 0, ...) {
  return(datarobot.apicore::ApplicationsApi$new()$ApplicationsAccessControlList(offset = offset, limit = limit, applicationId = applicationId, userId = userId, username = username))
}

#' PatchManyApplicationsAccessControl
#'
#' Update access control for this application.
#'
#' Update access control for this application. Request is processed only if updates can be performed on all entries.
#'
#' @seealso The method ApplicationsAccessControlPatchMany in [datarobot.apicore::ApplicationsApi], which this function wraps.
#' @family Applications
#' @export
PatchManyApplicationsAccessControl <- function(data, permissions, applicationId, ...) {
  applicationAccessControlUpdateRequest <- datarobot.apicore::ApplicationAccessControlUpdateRequest$new(data = data, permissions = permissions, validateParams = TRUE)
  return(datarobot.apicore::ApplicationsApi$new()$ApplicationsAccessControlPatchMany(applicationAccessControlUpdateRequest = applicationAccessControlUpdateRequest, applicationId = applicationId))
}

#' PatchManyApplicationsAuthToken
#'
#' Redeploys application with refreshed auth token.
#'
#' Redeploys application with refreshed auth token.
#'
#' @seealso The method ApplicationsAuthTokenPatchMany in [datarobot.apicore::ApplicationsApi], which this function wraps.
#' @family Applications
#' @export
PatchManyApplicationsAuthToken <- function(applicationId, ...) {
  return(datarobot.apicore::ApplicationsApi$new()$ApplicationsAuthTokenPatchMany(applicationId = applicationId))
}

#' CreateApplicationsBuildJobs
#'
#' Schedules a build job
#'
#' Schedules a build job, which is normally automatically scheduled at the end of DataRobot autopilot. This is currently used in tests only, so this route is for internal use only.
#'
#' @seealso The method ApplicationsBuildJobsCreate in [datarobot.apicore::ApplicationsApi], which this function wraps.
#' @family Applications
#' @export
CreateApplicationsBuildJobs <- function(body, applicationId, ...) {
  return(datarobot.apicore::ApplicationsApi$new()$ApplicationsBuildJobsCreate(body = body, applicationId = applicationId))
}

#' CreateApplications
#'
#' Create an application
#'
#' Create an application. Note that the number of active applications users can have at the same time is limited.
#'
#' @seealso The method ApplicationsCreate in [datarobot.apicore::ApplicationsApi], which this function wraps.
#' @family Applications
#' @export
CreateApplications <- function(sources, purpose, applicationTypeId, name, description, modelDeploymentId, cloudProvider = "drcloud", authenticationType = "invitedUsersOnly", ...) {
  applicationCreate <- datarobot.apicore::ApplicationCreate$new(sources = sources, purpose = purpose, applicationTypeId = applicationTypeId, cloudProvider = cloudProvider, name = name, description = description, modelDeploymentId = modelDeploymentId, authenticationType = authenticationType, validateParams = TRUE)
  return(datarobot.apicore::ApplicationsApi$new()$ApplicationsCreate(applicationCreate = applicationCreate))
}

#' DeleteApplications
#'
#' Delete an application
#'
#' Delete an application
#'
#' @seealso The method ApplicationsDelete in [datarobot.apicore::ApplicationsApi], which this function wraps.
#' @family Applications
#' @export
DeleteApplications <- function(applicationId, ...) {
  return(datarobot.apicore::ApplicationsApi$new()$ApplicationsDelete(applicationId = applicationId))
}

#' PatchManyApplicationsDeploymentState
#'
#' Update the application's deployment state.
#'
#' Update the application's deployment state. Transitioning the app to `deactivated` state will keep the app record in the database, but the app will not be accessible at its URL until changed back to `deployed` state again. When the limit of active apps has been reached, this API can be used to deactivate some applications to make space for new ones.
#'
#' @seealso The method ApplicationsDeploymentStatePatchMany in [datarobot.apicore::ApplicationsApi], which this function wraps.
#' @family Applications
#' @export
PatchManyApplicationsDeploymentState <- function(newDeploymentState, applicationId, ...) {
  applicationDeploymentStateUpdate <- datarobot.apicore::ApplicationDeploymentStateUpdate$new(newDeploymentState = newDeploymentState, validateParams = TRUE)
  return(datarobot.apicore::ApplicationsApi$new()$ApplicationsDeploymentStatePatchMany(applicationDeploymentStateUpdate = applicationDeploymentStateUpdate, applicationId = applicationId))
}

#' CreateApplicationsDeployments
#'
#' Links a deployment to an application
#'
#' If application creates deployment during its lifetime, we want to have an API to link deployment with application.
#'
#' @seealso The method ApplicationsDeploymentsCreate in [datarobot.apicore::ApplicationsApi], which this function wraps.
#' @family Applications
#' @export
CreateApplicationsDeployments <- function(modelDeploymentId, applicationId, linkName, ...) {
  addDeploymentToApplication <- datarobot.apicore::AddDeploymentToApplication$new(modelDeploymentId = modelDeploymentId, linkName = linkName, validateParams = TRUE)
  return(datarobot.apicore::ApplicationsApi$new()$ApplicationsDeploymentsCreate(addDeploymentToApplication = addDeploymentToApplication, applicationId = applicationId))
}

#' DeleteApplicationsDeployments
#'
#' Delete link between application and deployment.
#'
#' Delete link between application and deployment.
#'
#' @seealso The method ApplicationsDeploymentsDelete in [datarobot.apicore::ApplicationsApi], which this function wraps.
#' @family Applications
#' @export
DeleteApplicationsDeployments <- function(modelDeploymentId, applicationId, ...) {
  return(datarobot.apicore::ApplicationsApi$new()$ApplicationsDeploymentsDelete(modelDeploymentId = modelDeploymentId, applicationId = applicationId))
}

#' CreateApplicationsDuplicate
#'
#' Create a duplicate of the application
#'
#' Create a copy of App Builder application. Note that the number of active applications users can have at the same time is limited.
#'
#' @seealso The method ApplicationsDuplicateCreate in [datarobot.apicore::ApplicationsApi], which this function wraps.
#' @family Applications
#' @export
CreateApplicationsDuplicate <- function(name, description, applicationId, duplicatePredictions = FALSE, ...) {
  applicationDuplicate <- datarobot.apicore::ApplicationDuplicate$new(name = name, description = description, duplicatePredictions = duplicatePredictions, validateParams = TRUE)
  return(datarobot.apicore::ApplicationsApi$new()$ApplicationsDuplicateCreate(applicationDuplicate = applicationDuplicate, applicationId = applicationId))
}

#' ListApplications
#'
#' Paginated list of applications created by the currently authenticated user.
#'
#' Paginated list of applications created by the currently authenticated user.
#'
#' @seealso The method ApplicationsList in [datarobot.apicore::ApplicationsApi], which this function wraps.
#' @family Applications
#' @export
ListApplications <- function(offset = 0, limit = 0, ...) {
  return(datarobot.apicore::ApplicationsApi$new()$ApplicationsList(offset = offset, limit = limit))
}

#' PatchApplications
#'
#' Update an application's name and/or domain prefix
#'
#' Update an application's name and/or domain prefix
#'
#' @seealso The method ApplicationsPatch in [datarobot.apicore::ApplicationsApi], which this function wraps.
#' @family Applications
#' @export
PatchApplications <- function(domain, name, description, applicationId, ...) {
  applicationDeploymentUpdate <- datarobot.apicore::ApplicationDeploymentUpdate$new(domain = domain, name = name, description = description, validateParams = TRUE)
  return(datarobot.apicore::ApplicationsApi$new()$ApplicationsPatch(applicationDeploymentUpdate = applicationDeploymentUpdate, applicationId = applicationId))
}

#' RetrieveApplications
#'
#' Retrieve an application
#'
#' Retrieve an application
#'
#' @seealso The method ApplicationsRetrieve in [datarobot.apicore::ApplicationsApi], which this function wraps.
#' @family Applications
#' @export
RetrieveApplications <- function(applicationId, ...) {
  return(datarobot.apicore::ApplicationsApi$new()$ApplicationsRetrieve(applicationId = applicationId))
}

#' ListApplicationsSharedRoles
#'
#' Get a list of users, groups and organizations that have an access to this application
#'
#' Get a list of users, groups and organizations that have an access to this application
#'
#' @seealso The method ApplicationsSharedRolesList in [datarobot.apicore::ApplicationsApi], which this function wraps.
#' @family Applications
#' @export
ListApplicationsSharedRoles <- function(next_, shareRecipientType, previous, data, count, name, limit, id, totalCount, applicationId, offset = 0, ...) {
  applicationSharedRolesList <- datarobot.apicore::ApplicationSharedRolesList$new(next_ = next_, previous = previous, data = data, count = count, totalCount = totalCount, validateParams = TRUE)
  return(datarobot.apicore::ApplicationsApi$new()$ApplicationsSharedRolesList(applicationSharedRolesList = applicationSharedRolesList, shareRecipientType = shareRecipientType, offset = offset, name = name, limit = limit, id = id, applicationId = applicationId))
}

#' PatchManyApplicationsSharedRoles
#'
#' Share an application with a user, group, or organization
#'
#' Share an application with a user, group, or organization
#'
#' @seealso The method ApplicationsSharedRolesPatchMany in [datarobot.apicore::ApplicationsApi], which this function wraps.
#' @family Applications
#' @export
PatchManyApplicationsSharedRoles <- function(roles, applicationId, operation, note = "", sendNotification = FALSE, ...) {
  applicationSharingUpdateOrRemove <- datarobot.apicore::ApplicationSharingUpdateOrRemove$new(note = note, validateParams = TRUE)
  return(datarobot.apicore::ApplicationsApi$new()$ApplicationsSharedRolesPatchMany(applicationSharingUpdateOrRemove = applicationSharingUpdateOrRemove, roles = roles, sendNotification = sendNotification, applicationId = applicationId, operation = operation))
}

#' PatchManyApplicationsVersion
#'
#' Redeploys application with specified version.
#'
#' Redeploys application with specified version. If version is omitted latest available version is used.
#'
#' @seealso The method ApplicationsVersionPatchMany in [datarobot.apicore::ApplicationsApi], which this function wraps.
#' @family Applications
#' @export
PatchManyApplicationsVersion <- function(applicationId, version, ...) {
  applicationUpgradeDeploymentVersion <- datarobot.apicore::ApplicationUpgradeDeploymentVersion$new(version = version, validateParams = TRUE)
  return(datarobot.apicore::ApplicationsApi$new()$ApplicationsVersionPatchMany(applicationId = applicationId, applicationUpgradeDeploymentVersion = applicationUpgradeDeploymentVersion))
}

#' CreateDemoApplications
#'
#' Create a demo application
#'
#' Create a demo application. The number of active applications users can have at the same time is limited.
#'
#' @seealso The method DemoApplicationsCreate in [datarobot.apicore::ApplicationsApi], which this function wraps.
#' @family Applications
#' @export
CreateDemoApplications <- function(applicationTypeId, usecaseId, ...) {
  demoApplication <- datarobot.apicore::DemoApplication$new(applicationTypeId = applicationTypeId, usecaseId = usecaseId, validateParams = TRUE)
  return(datarobot.apicore::ApplicationsApi$new()$DemoApplicationsCreate(demoApplication = demoApplication))
}