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

# API Wrapper methods for CustomTasks
# To use these methods without modification, DR endpoint and token
# should be set as environment variables. This will automatically
# happen when you call `datarobot::ConnectToDataRobot()`.



#' CreateFromLatestCustomTaskVersion
#'
#' Update custom task version files.
#'
#' Create a new custom task version with files added, replaced or deleted. Files from the previous version of a custom task will be used as a basis.
#' @details This method invokes `PATCH /customTasks/{customTaskId}/versions/` in the DataRobot Public API.
#'
#' @param ... Additional keyword arguments to be passed on to the `datarobot.apicore` and `httr` libraries.
#'
#' @seealso The method CustomTaskVersionCreateFromLatest in [datarobot.apicore::CustomTasksApi], which invokes the same underlying API endpoint.
#' @export
#' @md
CreateFromLatestCustomTaskVersion <- function(requiredMetadata, file, baseEnvironmentId, filePath, customTaskId, maximumMemory, filesToDelete, requiredMetadataValues, isMajorUpdate = "true", ...) {
  return(datarobot.apicore::CustomTasksApi$new()$CustomTaskVersionCreateFromLatest(requiredMetadata = requiredMetadata, file = file, isMajorUpdate = isMajorUpdate, baseEnvironmentId = baseEnvironmentId, filePath = filePath, customTaskId = customTaskId, maximumMemory = maximumMemory, filesToDelete = filesToDelete, requiredMetadataValues = requiredMetadataValues, ...))
}

#' ListCustomTasksAccessControl
#'
#' Get a list of users who have access to this custom task and their roles on it.
#'
#' Get a list of users who have access to this custom task and their roles on it.
#' @details This method invokes `GET /customTasks/{customTaskId}/accessControl/` in the DataRobot Public API.
#'
#' @param ... Additional keyword arguments to be passed on to the `datarobot.apicore` and `httr` libraries.
#'
#' @seealso The method CustomTasksAccessControlList in [datarobot.apicore::CustomTasksApi], which invokes the same underlying API endpoint.
#' @export
#' @md
ListCustomTasksAccessControl <- function(customTaskId, offset = 0, limit = 1000, ...) {
  return(datarobot.apicore::CustomTasksApi$new()$CustomTasksAccessControlList(offset = offset, customTaskId = customTaskId, limit = limit, ...))
}

#' PatchManyCustomTasksAccessControl
#'
#' Grant access or update roles for users on this custom task and appropriate learning data.
#'
#' Grant access or update roles for users on this custom task and appropriate learning data. Up to 100 user roles may be set in a single request.
#' @details This method invokes `PATCH /customTasks/{customTaskId}/accessControl/` in the DataRobot Public API.
#'
#' @param ... Additional keyword arguments to be passed on to the `datarobot.apicore` and `httr` libraries.
#'
#' @seealso The method CustomTasksAccessControlPatchMany in [datarobot.apicore::CustomTasksApi], which invokes the same underlying API endpoint.
#' @export
#' @md
PatchManyCustomTasksAccessControl <- function(data, customTaskId, ...) {
  sharingUpdateOrRemoveWithGrant <- datarobot.apicore::SharingUpdateOrRemoveWithGrant$new(data = data, validateParams = TRUE)
  return(datarobot.apicore::CustomTasksApi$new()$CustomTasksAccessControlPatchMany(sharingUpdateOrRemoveWithGrant = sharingUpdateOrRemoveWithGrant, customTaskId = customTaskId, ...))
}

#' CreateCustomTasks
#'
#' Create a custom task
#'
#' Creates a new custom task and returns the newly created metadata record for it.  A custom task may either be an estimator or a transform. Estimators must support a single target type (e.g. binaryClassification, regression). Regression and anomaly detection models are expected to produce predictions that are arbitrary floating-point or integer numbers. A classification model is expected to return predictions with probability scores for each class. For example, a binary classification model might return:  ``` Python {     positiveClassLabel: probability,     negativeClassLabel: 1.0 - probability } ```  Transforms are expected to return a dataframe or sparse matrix with the same number of rows as the input feature matrix. At this time, only numeric outputs are supported for custom transforms.
#' @details This method invokes `POST /customTasks/` in the DataRobot Public API.
#'
#' @param ... Additional keyword arguments to be passed on to the `datarobot.apicore` and `httr` libraries.
#'
#' @seealso The method CustomTasksCreate in [datarobot.apicore::CustomTasksApi], which invokes the same underlying API endpoint.
#' @export
#' @md
CreateCustomTasks <- function(name, maximumMemory, description, targetType, language, calibratePredictions = TRUE, ...) {
  customTaskCreate <- datarobot.apicore::CustomTaskCreate$new(calibratePredictions = calibratePredictions, name = name, maximumMemory = maximumMemory, description = description, targetType = targetType, language = language, validateParams = TRUE)
  return(datarobot.apicore::CustomTasksApi$new()$CustomTasksCreate(customTaskCreate = customTaskCreate, ...))
}

#' DeleteCustomTasks
#'
#' Delete custom task.
#'
#' Delete a custom task. Only users who have permission to edit custom task can delete it. Only custom tasks which are not currently deployed can be deleted. Relevant CustomTaskImage will be deleted also.
#' @details This method invokes `DELETE /customTasks/{customTaskId}/` in the DataRobot Public API.
#'
#' @param ... Additional keyword arguments to be passed on to the `datarobot.apicore` and `httr` libraries.
#'
#' @seealso The method CustomTasksDelete in [datarobot.apicore::CustomTasksApi], which invokes the same underlying API endpoint.
#' @export
#' @md
DeleteCustomTasks <- function(customTaskId, ...) {
  return(datarobot.apicore::CustomTasksApi$new()$CustomTasksDelete(customTaskId = customTaskId, ...))
}

#' ListCustomTasksDownload
#'
#' Download the latest custom task version content.
#'
#' Download the latest item bundle from a custom task as a zip compressed archive.
#' @details This method invokes `GET /customTasks/{customTaskId}/download/` in the DataRobot Public API.
#'
#' @param ... Additional keyword arguments to be passed on to the `datarobot.apicore` and `httr` libraries.
#'
#' @seealso The method CustomTasksDownloadList in [datarobot.apicore::CustomTasksApi], which invokes the same underlying API endpoint.
#' @export
#' @md
ListCustomTasksDownload <- function(customTaskId, ...) {
  return(datarobot.apicore::CustomTasksApi$new()$CustomTasksDownloadList(customTaskId = customTaskId, ...))
}

#' CreateCustomTasksFromCustomTask
#'
#' Clone custom task.
#'
#' Creates a copy of the provided custom task, including metadata, versions of that task, and uploaded files. Associates the new versions with files owned by the custom task.
#' @details This method invokes `POST /customTasks/fromCustomTask/` in the DataRobot Public API.
#'
#' @param ... Additional keyword arguments to be passed on to the `datarobot.apicore` and `httr` libraries.
#'
#' @seealso The method CustomTasksFromCustomTaskCreate in [datarobot.apicore::CustomTasksApi], which invokes the same underlying API endpoint.
#' @export
#' @md
CreateCustomTasksFromCustomTask <- function(customTaskId, ...) {
  customTaskCopy <- datarobot.apicore::CustomTaskCopy$new(customTaskId = customTaskId, validateParams = TRUE)
  return(datarobot.apicore::CustomTasksApi$new()$CustomTasksFromCustomTaskCreate(customTaskCopy = customTaskCopy, ...))
}

#' ListCustomTasks
#'
#' List custom tasks.
#'
#' Retrieve metadata for all custom tasks the user has access to.
#' @details This method invokes `GET /customTasks/` in the DataRobot Public API.
#'
#' @param ... Additional keyword arguments to be passed on to the `datarobot.apicore` and `httr` libraries.
#'
#' @seealso The method CustomTasksList in [datarobot.apicore::CustomTasksApi], which invokes the same underlying API endpoint.
#' @export
#' @md
ListCustomTasks <- function(orderBy, searchFor, offset = 0, limit = 1000, ...) {
  return(datarobot.apicore::CustomTasksApi$new()$CustomTasksList(offset = offset, limit = limit, orderBy = orderBy, searchFor = searchFor, ...))
}

#' PatchCustomTasks
#'
#' Update custom task.
#'
#' Updates metadata for an existing custom task.  All custom tasks must support one target type (e.g. binaryClassification, regression, transform).  Setting positiveClassLabel and negativeClassLabel to null will set the labels to their default values (1 and 0 for positiveClassLabel and negativeClassLabel, respectively).  Setting positiveClassLabel, negativeClassLabel, targetName is disabled if task has active deployments.
#' @details This method invokes `PATCH /customTasks/{customTaskId}/` in the DataRobot Public API.
#'
#' @param ... Additional keyword arguments to be passed on to the `datarobot.apicore` and `httr` libraries.
#'
#' @seealso The method CustomTasksPatch in [datarobot.apicore::CustomTasksApi], which invokes the same underlying API endpoint.
#' @export
#' @md
PatchCustomTasks <- function(customTaskId, name, maximumMemory, description, language, ...) {
  customTaskUpdate <- datarobot.apicore::CustomTaskUpdate$new(name = name, maximumMemory = maximumMemory, description = description, language = language, validateParams = TRUE)
  return(datarobot.apicore::CustomTasksApi$new()$CustomTasksPatch(customTaskId = customTaskId, customTaskUpdate = customTaskUpdate, ...))
}

#' RetrieveCustomTasks
#'
#' Get custom task.
#'
#' Retrieve metadata for a custom task
#' @details This method invokes `GET /customTasks/{customTaskId}/` in the DataRobot Public API.
#'
#' @param ... Additional keyword arguments to be passed on to the `datarobot.apicore` and `httr` libraries.
#'
#' @seealso The method CustomTasksRetrieve in [datarobot.apicore::CustomTasksApi], which invokes the same underlying API endpoint.
#' @export
#' @md
RetrieveCustomTasks <- function(customTaskId, ...) {
  return(datarobot.apicore::CustomTasksApi$new()$CustomTasksRetrieve(customTaskId = customTaskId, ...))
}

#' CreateCustomTasksVersions
#'
#' Create custom task version.
#'
#' Create a new custom task version with attached files if supplied.
#' @details This method invokes `POST /customTasks/{customTaskId}/versions/` in the DataRobot Public API.
#'
#' @param ... Additional keyword arguments to be passed on to the `datarobot.apicore` and `httr` libraries.
#'
#' @seealso The method CustomTasksVersionsCreate in [datarobot.apicore::CustomTasksApi], which invokes the same underlying API endpoint.
#' @export
#' @md
CreateCustomTasksVersions <- function(requiredMetadata, file, baseEnvironmentId, filePath, customTaskId, maximumMemory, requiredMetadataValues, isMajorUpdate = "true", ...) {
  return(datarobot.apicore::CustomTasksApi$new()$CustomTasksVersionsCreate(requiredMetadata = requiredMetadata, file = file, isMajorUpdate = isMajorUpdate, baseEnvironmentId = baseEnvironmentId, filePath = filePath, customTaskId = customTaskId, maximumMemory = maximumMemory, requiredMetadataValues = requiredMetadataValues, ...))
}

#' CreateCustomTasksVersionsDependencyBuild
#'
#' Start a custom task version's dependency build.
#'
#' Start a custom task version's dependency build. This is required to test, deploy, or train custom tasks.
#' @details This method invokes `POST /customTasks/{customTaskId}/versions/{customTaskVersionId}/dependencyBuild/` in the DataRobot Public API.
#'
#' @param ... Additional keyword arguments to be passed on to the `datarobot.apicore` and `httr` libraries.
#'
#' @seealso The method CustomTasksVersionsDependencyBuildCreate in [datarobot.apicore::CustomTasksApi], which invokes the same underlying API endpoint.
#' @export
#' @md
CreateCustomTasksVersionsDependencyBuild <- function(customTaskVersionId, customTaskId, ...) {
  return(datarobot.apicore::CustomTasksApi$new()$CustomTasksVersionsDependencyBuildCreate(customTaskVersionId = customTaskVersionId, customTaskId = customTaskId, ...))
}

#' DeleteManyCustomTasksVersionsDependencyBuild
#'
#' Cancel dependency build.
#'
#' Cancel the custom task version's dependency build.
#' @details This method invokes `DELETE /customTasks/{customTaskId}/versions/{customTaskVersionId}/dependencyBuild/` in the DataRobot Public API.
#'
#' @param ... Additional keyword arguments to be passed on to the `datarobot.apicore` and `httr` libraries.
#'
#' @seealso The method CustomTasksVersionsDependencyBuildDeleteMany in [datarobot.apicore::CustomTasksApi], which invokes the same underlying API endpoint.
#' @export
#' @md
DeleteManyCustomTasksVersionsDependencyBuild <- function(customTaskVersionId, customTaskId, ...) {
  return(datarobot.apicore::CustomTasksApi$new()$CustomTasksVersionsDependencyBuildDeleteMany(customTaskVersionId = customTaskVersionId, customTaskId = customTaskId, ...))
}

#' ListCustomTasksVersionsDependencyBuild
#'
#' Retrieve the custom task version's dependency build status.
#'
#' Retrieve the custom task version's dependency build status.
#' @details This method invokes `GET /customTasks/{customTaskId}/versions/{customTaskVersionId}/dependencyBuild/` in the DataRobot Public API.
#'
#' @param ... Additional keyword arguments to be passed on to the `datarobot.apicore` and `httr` libraries.
#'
#' @seealso The method CustomTasksVersionsDependencyBuildList in [datarobot.apicore::CustomTasksApi], which invokes the same underlying API endpoint.
#' @export
#' @md
ListCustomTasksVersionsDependencyBuild <- function(customTaskVersionId, customTaskId, ...) {
  return(datarobot.apicore::CustomTasksApi$new()$CustomTasksVersionsDependencyBuildList(customTaskVersionId = customTaskVersionId, customTaskId = customTaskId, ...))
}

#' ListCustomTasksVersionsDependencyBuildLog
#'
#' Retrieve the custom task version's dependency build log.
#'
#' Retrieve the custom task version's dependency build log.
#' @details This method invokes `GET /customTasks/{customTaskId}/versions/{customTaskVersionId}/dependencyBuildLog/` in the DataRobot Public API.
#'
#' @param ... Additional keyword arguments to be passed on to the `datarobot.apicore` and `httr` libraries.
#'
#' @seealso The method CustomTasksVersionsDependencyBuildLogList in [datarobot.apicore::CustomTasksApi], which invokes the same underlying API endpoint.
#' @export
#' @md
ListCustomTasksVersionsDependencyBuildLog <- function(customTaskVersionId, customTaskId, ...) {
  return(datarobot.apicore::CustomTasksApi$new()$CustomTasksVersionsDependencyBuildLogList(customTaskVersionId = customTaskVersionId, customTaskId = customTaskId, ...))
}

#' ListCustomTasksVersionsDownload
#'
#' Download custom task version content.
#'
#' Download a specific item bundle from a custom task as a zip compressed archive.
#' @details This method invokes `GET /customTasks/{customTaskId}/versions/{customTaskVersionId}/download/` in the DataRobot Public API.
#'
#' @param ... Additional keyword arguments to be passed on to the `datarobot.apicore` and `httr` libraries.
#'
#' @seealso The method CustomTasksVersionsDownloadList in [datarobot.apicore::CustomTasksApi], which invokes the same underlying API endpoint.
#' @export
#' @md
ListCustomTasksVersionsDownload <- function(customTaskVersionId, customTaskId, ...) {
  return(datarobot.apicore::CustomTasksApi$new()$CustomTasksVersionsDownloadList(customTaskVersionId = customTaskVersionId, customTaskId = customTaskId, ...))
}

#' CreateCustomTasksVersionsFromRepository
#'
#' Create custom task version from remote repository.
#'
#' Create a new custom task version with only files added from the specified remote repository.
#' @details This method invokes `POST /customTasks/{customTaskId}/versions/fromRepository/` in the DataRobot Public API.
#'
#' @param ... Additional keyword arguments to be passed on to the `datarobot.apicore` and `httr` libraries.
#'
#' @seealso The method CustomTasksVersionsFromRepositoryCreate in [datarobot.apicore::CustomTasksApi], which invokes the same underlying API endpoint.
#' @export
#' @md
CreateCustomTasksVersionsFromRepository <- function(requiredMetadata, ref, baseEnvironmentId, customTaskId, repositoryId, requiredMetadataValues, sourcePath, isMajorUpdate = TRUE, ...) {
  customTaskVersionCreateFromRepository <- datarobot.apicore::CustomTaskVersionCreateFromRepository$new(requiredMetadata = requiredMetadata, ref = ref, isMajorUpdate = isMajorUpdate, baseEnvironmentId = baseEnvironmentId, repositoryId = repositoryId, requiredMetadataValues = requiredMetadataValues, sourcePath = sourcePath, validateParams = TRUE)
  return(datarobot.apicore::CustomTasksApi$new()$CustomTasksVersionsFromRepositoryCreate(customTaskVersionCreateFromRepository = customTaskVersionCreateFromRepository, customTaskId = customTaskId, ...))
}

#' PatchManyCustomTasksVersionsFromRepository
#'
#' Create custom task version from remote repository with files from previous version.
#'
#' Create a new custom task version with files added from a remote repository. Files from the previous version of a custom task will be used as a basis.
#' @details This method invokes `PATCH /customTasks/{customTaskId}/versions/fromRepository/` in the DataRobot Public API.
#'
#' @param ... Additional keyword arguments to be passed on to the `datarobot.apicore` and `httr` libraries.
#'
#' @seealso The method CustomTasksVersionsFromRepositoryPatchMany in [datarobot.apicore::CustomTasksApi], which invokes the same underlying API endpoint.
#' @export
#' @md
PatchManyCustomTasksVersionsFromRepository <- function(requiredMetadata, ref, baseEnvironmentId, customTaskId, repositoryId, requiredMetadataValues, sourcePath, isMajorUpdate = TRUE, ...) {
  customTaskVersionCreateFromRepository <- datarobot.apicore::CustomTaskVersionCreateFromRepository$new(requiredMetadata = requiredMetadata, ref = ref, isMajorUpdate = isMajorUpdate, baseEnvironmentId = baseEnvironmentId, repositoryId = repositoryId, requiredMetadataValues = requiredMetadataValues, sourcePath = sourcePath, validateParams = TRUE)
  return(datarobot.apicore::CustomTasksApi$new()$CustomTasksVersionsFromRepositoryPatchMany(customTaskVersionCreateFromRepository = customTaskVersionCreateFromRepository, customTaskId = customTaskId, ...))
}

#' ListCustomTasksVersions
#'
#' List custom task versions.
#'
#' List custom task versions.
#' @details This method invokes `GET /customTasks/{customTaskId}/versions/` in the DataRobot Public API.
#'
#' @param ... Additional keyword arguments to be passed on to the `datarobot.apicore` and `httr` libraries.
#'
#' @seealso The method CustomTasksVersionsList in [datarobot.apicore::CustomTasksApi], which invokes the same underlying API endpoint.
#' @export
#' @md
ListCustomTasksVersions <- function(customTaskId, offset = 0, limit = 1000, ...) {
  return(datarobot.apicore::CustomTasksApi$new()$CustomTasksVersionsList(offset = offset, customTaskId = customTaskId, limit = limit, ...))
}

#' PatchCustomTasksVersions
#'
#' Update custom task version.
#'
#' Edit metadata of a specific task version.
#' @details This method invokes `PATCH /customTasks/{customTaskId}/versions/{customTaskVersionId}/` in the DataRobot Public API.
#'
#' @param ... Additional keyword arguments to be passed on to the `datarobot.apicore` and `httr` libraries.
#'
#' @seealso The method CustomTasksVersionsPatch in [datarobot.apicore::CustomTasksApi], which invokes the same underlying API endpoint.
#' @export
#' @md
PatchCustomTasksVersions <- function(requiredMetadata, customTaskVersionId, customTaskId, description, requiredMetadataValues, ...) {
  customTaskVersionMetadataUpdate <- datarobot.apicore::CustomTaskVersionMetadataUpdate$new(requiredMetadata = requiredMetadata, description = description, requiredMetadataValues = requiredMetadataValues, validateParams = TRUE)
  return(datarobot.apicore::CustomTasksApi$new()$CustomTasksVersionsPatch(customTaskVersionMetadataUpdate = customTaskVersionMetadataUpdate, customTaskVersionId = customTaskVersionId, customTaskId = customTaskId, ...))
}

#' RetrieveCustomTasksVersions
#'
#' Get custom task version.
#'
#' Display a requested version of a custom task along with the files attached to it.
#' @details This method invokes `GET /customTasks/{customTaskId}/versions/{customTaskVersionId}/` in the DataRobot Public API.
#'
#' @param ... Additional keyword arguments to be passed on to the `datarobot.apicore` and `httr` libraries.
#'
#' @seealso The method CustomTasksVersionsRetrieve in [datarobot.apicore::CustomTasksApi], which invokes the same underlying API endpoint.
#' @export
#' @md
RetrieveCustomTasksVersions <- function(customTaskVersionId, customTaskId, ...) {
  return(datarobot.apicore::CustomTasksApi$new()$CustomTasksVersionsRetrieve(customTaskVersionId = customTaskVersionId, customTaskId = customTaskId, ...))
}
