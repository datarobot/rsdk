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

# API Wrapper methods for SsoConfiguration
# To use these methods without modification, DR endpoint and token
# should be set as environment variables. This will automatically
# happen when you call `datarobot::ConnectToDataRobot()`.



#' CreateSsoConfigurations
#'
#' Create an SSO configuration for a specific organization
#'
#' Create an SSO configuration for a specific organization
#'
#' @seealso The method SsoConfigurationsCreate in [datarobot.apicore::SsoConfigurationApi], which this function wraps.
#' @family SsoConfiguration
#' @export
CreateSsoConfigurations <- function(autoGenerateUsers, certificate, securityParameters, entityId, issuer, signOnUrl, organizationId, idpMetadataUrl, roleMapping, idpMetadataHttpsVerify, idpMetadata, attributeMapping, signOutUrl, enableSso, groupMapping, name, enforceSso, configurationType, idpResponseMethod = "POST", sessionLengthSeconds = 604800, spRequestMethod = "REDIRECT", ...) {
  createSsoConfiguration <- datarobot.apicore::CreateSsoConfiguration$new(autoGenerateUsers = autoGenerateUsers, certificate = certificate, securityParameters = securityParameters, entityId = entityId, issuer = issuer, signOnUrl = signOnUrl, organizationId = organizationId, idpMetadataUrl = idpMetadataUrl, roleMapping = roleMapping, idpMetadataHttpsVerify = idpMetadataHttpsVerify, idpMetadata = idpMetadata, attributeMapping = attributeMapping, idpResponseMethod = idpResponseMethod, signOutUrl = signOutUrl, sessionLengthSeconds = sessionLengthSeconds, enableSso = enableSso, groupMapping = groupMapping, name = name, enforceSso = enforceSso, spRequestMethod = spRequestMethod, configurationType = configurationType, validateParams = TRUE)
  return(datarobot.apicore::SsoConfigurationApi$new()$SsoConfigurationsCreate(createSsoConfiguration = createSsoConfiguration))
}

#' ListSsoConfigurations
#'
#' List sso configurations.
#'
#' List the sso configurations that correspond to provided conditions.
#'
#' @seealso The method SsoConfigurationsList in [datarobot.apicore::SsoConfigurationApi], which this function wraps.
#' @family SsoConfiguration
#' @export
ListSsoConfigurations <- function(orgId, offset = 0, limit = 100, ...) {
  return(datarobot.apicore::SsoConfigurationApi$new()$SsoConfigurationsList(offset = offset, limit = limit, orgId = orgId))
}

#' PatchSsoConfigurations
#'
#' Update an SSO configuration for a specific organization.
#'
#' Update an SSO configuration for a specific organization.
#'
#' @seealso The method SsoConfigurationsPatch in [datarobot.apicore::SsoConfigurationApi], which this function wraps.
#' @family SsoConfiguration
#' @export
PatchSsoConfigurations <- function(autoGenerateUsers, certificate, securityParameters, entityId, issuer, signOnUrl, organizationId, idpMetadataUrl, roleMapping, idpMetadataHttpsVerify, idpMetadata, attributeMapping, idpResponseMethod, signOutUrl, sessionLengthSeconds, enableSso, groupMapping, name, enforceSso, configurationId, spRequestMethod, configurationType, advancedConfiguration, ...) {
  updateSsoConfiguration <- datarobot.apicore::UpdateSsoConfiguration$new(autoGenerateUsers = autoGenerateUsers, certificate = certificate, securityParameters = securityParameters, entityId = entityId, issuer = issuer, signOnUrl = signOnUrl, organizationId = organizationId, idpMetadataUrl = idpMetadataUrl, roleMapping = roleMapping, idpMetadataHttpsVerify = idpMetadataHttpsVerify, idpMetadata = idpMetadata, attributeMapping = attributeMapping, idpResponseMethod = idpResponseMethod, signOutUrl = signOutUrl, sessionLengthSeconds = sessionLengthSeconds, enableSso = enableSso, groupMapping = groupMapping, name = name, enforceSso = enforceSso, spRequestMethod = spRequestMethod, configurationType = configurationType, advancedConfiguration = advancedConfiguration, validateParams = TRUE)
  return(datarobot.apicore::SsoConfigurationApi$new()$SsoConfigurationsPatch(updateSsoConfiguration = updateSsoConfiguration, configurationId = configurationId))
}

#' RetrieveSsoConfigurations
#'
#' Retrieve SSO configuration of a specific organization.
#'
#' Retrieve SSO configuration of a specific organization.
#'
#' @seealso The method SsoConfigurationsRetrieve in [datarobot.apicore::SsoConfigurationApi], which this function wraps.
#' @family SsoConfiguration
#' @export
RetrieveSsoConfigurations <- function(configurationId, ...) {
  return(datarobot.apicore::SsoConfigurationApi$new()$SsoConfigurationsRetrieve(configurationId = configurationId))
}