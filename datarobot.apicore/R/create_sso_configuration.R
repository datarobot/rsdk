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
#' @title CreateSsoConfiguration
#'
#' @description CreateSsoConfiguration Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field attributeMapping  \link{SamlAttributeMapping} [optional]
#'
#' @field autoGenerateUsers  character [optional] Determines if DataRobot automatically creates an account on first successful login via IdP if the user doesn&#39;t exist in the DataRobot application.
#'
#' @field certificate  \link{SamlCertificate} [optional]
#'
#' @field configurationType  character The type of the SSO configuration, defines the source of SSO metadata.             It can be one of the following: &#x60;METADATA&#x60; - when IDP metadata is provided in the             config, &#x60;METADATA_URL&#x60; - when an URL for metadata retrieval is provided in the config and             &#x60;MANUAL&#x60; - when IDP sign-on/sign-out URLs and certificate are provided.
#'
#' @field enableSso  character Defines if SSO is enabled.
#'
#' @field enforceSso  character Defines if SSO is enforced.
#'
#' @field entityId  character The globally unique identifier of the entity. Provided by IdP service.
#'
#' @field groupMapping  list( \link{SamlGroupMapping} ) [optional] The list of DataRobot group to identity provider group maps.
#'
#' @field idpMetadata  \link{SamlMetadataFile} [optional]
#'
#' @field idpMetadataHttpsVerify  character [optional] When idp_metadata_url uses HTTPS, require the server to have a trusted certificate.             To avoid security vulnerabilities, only set to False when a trusted server has a             self-signed certificate.
#'
#' @field idpMetadataUrl  character [optional] URL to the IdP SSO descriptor. Provided by IdP service.
#'
#' @field idpResponseMethod  character Identity provider response method, used to move user from IdP&#39;s authentication form back to the DataRobot side.
#'
#' @field issuer  character [optional] Optional Issuer field that may be required by IdP.
#'
#' @field name  character The name of the SSO configuration.
#'
#' @field organizationId  character [optional] The organization ID to which the SSO config belongs.
#'
#' @field roleMapping  list( \link{SamlRoleMapping} ) [optional] The list of DataRobot access role to identity provider role maps.
#'
#' @field securityParameters  \link{SamlSecurityParameters} [optional]
#'
#' @field sessionLengthSeconds  integer Time window for the authentication session via IDP
#'
#' @field signOnUrl  character [optional] URL to sign on via SSO.
#'
#' @field signOutUrl  character [optional] URL to sign out via SSO.
#'
#' @field spRequestMethod  character Service provider (DataRobot application) request method, is used to move user to the IdP&#39;s authentication form.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
CreateSsoConfiguration <- R6::R6Class(
  "CreateSsoConfiguration",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`attributeMapping` = NULL, `autoGenerateUsers` = NULL, `certificate` = NULL, `configurationType` = NULL, `enableSso` = NULL, `enforceSso` = NULL, `entityId` = NULL, `groupMapping` = NULL, `idpMetadata` = NULL, `idpMetadataHttpsVerify` = NULL, `idpMetadataUrl` = NULL, `idpResponseMethod` = NULL, `issuer` = NULL, `name` = NULL, `organizationId` = NULL, `roleMapping` = NULL, `securityParameters` = NULL, `sessionLengthSeconds` = NULL, `signOnUrl` = NULL, `signOutUrl` = NULL, `spRequestMethod` = NULL) {
      if (!is.null(`configurationType`)) {
        stopifnot(is.character(`configurationType`), length(`configurationType`) == 1)
      }
      if (!is.null(`enableSso`)) {
        stopifnot(is.logical(`enableSso`), length(`enableSso`) == 1)
      }
      if (!is.null(`enforceSso`)) {
        stopifnot(is.logical(`enforceSso`), length(`enforceSso`) == 1)
      }
      if (!is.null(`entityId`)) {
        stopifnot(is.character(`entityId`), length(`entityId`) == 1)
      }
      if (!is.null(`idpResponseMethod`)) {
        stopifnot(is.character(`idpResponseMethod`), length(`idpResponseMethod`) == 1)
      }
      if (!is.null(`name`)) {
        stopifnot(is.character(`name`), length(`name`) == 1)
      }
      if (!is.null(`sessionLengthSeconds`)) {
        stopifnot(is.numeric(`sessionLengthSeconds`), length(`sessionLengthSeconds`) == 1)
      }
      if (!is.null(`spRequestMethod`)) {
        stopifnot(is.character(`spRequestMethod`), length(`spRequestMethod`) == 1)
      }
      if (!is.null(`attributeMapping`)) {
        stopifnot(R6::is.R6(`attributeMapping`))
      }
      if (!is.null(`autoGenerateUsers`)) {
        stopifnot(is.logical(`autoGenerateUsers`), length(`autoGenerateUsers`) == 1)
      }
      if (!is.null(`certificate`)) {
        stopifnot(R6::is.R6(`certificate`))
      }
      if (!is.null(`groupMapping`) && length(`groupMapping`) > 0) {
        stopifnot(is.vector(`groupMapping`), sapply(`groupMapping`, R6::is.R6))
      }
      if (!is.null(`idpMetadata`)) {
        stopifnot(R6::is.R6(`idpMetadata`))
      }
      if (!is.null(`idpMetadataHttpsVerify`)) {
        stopifnot(is.logical(`idpMetadataHttpsVerify`), length(`idpMetadataHttpsVerify`) == 1)
      }
      if (!is.null(`idpMetadataUrl`)) {
        stopifnot(is.character(`idpMetadataUrl`), length(`idpMetadataUrl`) == 1)
      }
      if (!is.null(`issuer`)) {
        stopifnot(is.character(`issuer`), length(`issuer`) == 1)
      }
      if (!is.null(`organizationId`)) {
        stopifnot(is.character(`organizationId`), length(`organizationId`) == 1)
      }
      if (!is.null(`roleMapping`) && length(`roleMapping`) > 0) {
        stopifnot(is.vector(`roleMapping`), sapply(`roleMapping`, R6::is.R6))
      }
      if (!is.null(`securityParameters`)) {
        stopifnot(R6::is.R6(`securityParameters`))
      }
      if (!is.null(`signOnUrl`)) {
        stopifnot(is.character(`signOnUrl`), length(`signOnUrl`) == 1)
      }
      if (!is.null(`signOutUrl`)) {
        stopifnot(is.character(`signOutUrl`), length(`signOutUrl`) == 1)
      }
    }
  ),
  public = list(
    `attributeMapping` = NULL,
    `autoGenerateUsers` = NULL,
    `certificate` = NULL,
    `configurationType` = NULL,
    `enableSso` = NULL,
    `enforceSso` = NULL,
    `entityId` = NULL,
    `groupMapping` = NULL,
    `idpMetadata` = NULL,
    `idpMetadataHttpsVerify` = NULL,
    `idpMetadataUrl` = NULL,
    `idpResponseMethod` = NULL,
    `issuer` = NULL,
    `name` = NULL,
    `organizationId` = NULL,
    `roleMapping` = NULL,
    `securityParameters` = NULL,
    `sessionLengthSeconds` = NULL,
    `signOnUrl` = NULL,
    `signOutUrl` = NULL,
    `spRequestMethod` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param attributeMapping
    #' @param autoGenerateUsers Determines if DataRobot automatically creates an account on first successful login via IdP if the user doesn&#39;t exist in the DataRobot application.
    #' @param certificate
    #' @param configurationType The type of the SSO configuration, defines the source of SSO metadata.             It can be one of the following: &#x60;METADATA&#x60; - when IDP metadata is provided in the             config, &#x60;METADATA_URL&#x60; - when an URL for metadata retrieval is provided in the config and             &#x60;MANUAL&#x60; - when IDP sign-on/sign-out URLs and certificate are provided.
    #' @param enableSso Defines if SSO is enabled.
    #' @param enforceSso Defines if SSO is enforced.
    #' @param entityId The globally unique identifier of the entity. Provided by IdP service.
    #' @param groupMapping The list of DataRobot group to identity provider group maps.
    #' @param idpMetadata
    #' @param idpMetadataHttpsVerify When idp_metadata_url uses HTTPS, require the server to have a trusted certificate.             To avoid security vulnerabilities, only set to False when a trusted server has a             self-signed certificate.
    #' @param idpMetadataUrl URL to the IdP SSO descriptor. Provided by IdP service.
    #' @param idpResponseMethod Identity provider response method, used to move user from IdP&#39;s authentication form back to the DataRobot side.
    #' @param issuer Optional Issuer field that may be required by IdP.
    #' @param name The name of the SSO configuration.
    #' @param organizationId The organization ID to which the SSO config belongs.
    #' @param roleMapping The list of DataRobot access role to identity provider role maps.
    #' @param securityParameters
    #' @param sessionLengthSeconds Time window for the authentication session via IDP
    #' @param signOnUrl URL to sign on via SSO.
    #' @param signOutUrl URL to sign out via SSO.
    #' @param spRequestMethod Service provider (DataRobot application) request method, is used to move user to the IdP&#39;s authentication form.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`configurationType` = NULL, `enableSso` = NULL, `enforceSso` = NULL, `entityId` = NULL, `idpResponseMethod` = NULL, `name` = NULL, `sessionLengthSeconds` = NULL, `spRequestMethod` = NULL, `attributeMapping` = NULL, `autoGenerateUsers` = NULL, `certificate` = NULL, `groupMapping` = NULL, `idpMetadata` = NULL, `idpMetadataHttpsVerify` = NULL, `idpMetadataUrl` = NULL, `issuer` = NULL, `organizationId` = NULL, `roleMapping` = NULL, `securityParameters` = NULL, `signOnUrl` = NULL, `signOutUrl` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`configurationType`, `enableSso`, `enforceSso`, `entityId`, `idpResponseMethod`, `name`, `sessionLengthSeconds`, `spRequestMethod`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(attributeMapping, autoGenerateUsers, certificate, configurationType, enableSso, enforceSso, entityId, groupMapping, idpMetadata, idpMetadataHttpsVerify, idpMetadataUrl, idpResponseMethod, issuer, name, organizationId, roleMapping, securityParameters, sessionLengthSeconds, signOnUrl, signOutUrl, spRequestMethod)
      }
      self$`attributeMapping` <- `attributeMapping`
      self$`autoGenerateUsers` <- `autoGenerateUsers`
      self$`certificate` <- `certificate`
      self$`configurationType` <- `configurationType`
      self$`enableSso` <- `enableSso`
      self$`enforceSso` <- `enforceSso`
      self$`entityId` <- `entityId`
      self$`groupMapping` <- `groupMapping`
      self$`idpMetadata` <- `idpMetadata`
      self$`idpMetadataHttpsVerify` <- `idpMetadataHttpsVerify`
      self$`idpMetadataUrl` <- `idpMetadataUrl`
      self$`idpResponseMethod` <- `idpResponseMethod`
      self$`issuer` <- `issuer`
      self$`name` <- `name`
      self$`organizationId` <- `organizationId`
      self$`roleMapping` <- `roleMapping`
      self$`securityParameters` <- `securityParameters`
      self$`sessionLengthSeconds` <- `sessionLengthSeconds`
      self$`signOnUrl` <- `signOnUrl`
      self$`signOutUrl` <- `signOutUrl`
      self$`spRequestMethod` <- `spRequestMethod`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(attributeMapping = self$`attributeMapping`, autoGenerateUsers = self$`autoGenerateUsers`, certificate = self$`certificate`, configurationType = self$`configurationType`, enableSso = self$`enableSso`, enforceSso = self$`enforceSso`, entityId = self$`entityId`, groupMapping = self$`groupMapping`, idpMetadata = self$`idpMetadata`, idpMetadataHttpsVerify = self$`idpMetadataHttpsVerify`, idpMetadataUrl = self$`idpMetadataUrl`, idpResponseMethod = self$`idpResponseMethod`, issuer = self$`issuer`, name = self$`name`, organizationId = self$`organizationId`, roleMapping = self$`roleMapping`, securityParameters = self$`securityParameters`, sessionLengthSeconds = self$`sessionLengthSeconds`, signOnUrl = self$`signOnUrl`, signOutUrl = self$`signOutUrl`, spRequestMethod = self$`spRequestMethod`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`attributeMapping`)) {
          sprintf(
            '"attributeMapping":
            %s
      ',
            jsonlite::toJSON(self$`attributeMapping`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`autoGenerateUsers`)) {
          sprintf(
            '"autoGenerateUsers":
            %s
                  ',
            tolower(self$`autoGenerateUsers`)
          )
        },
        if (!is.null(self$`certificate`)) {
          sprintf(
            '"certificate":
            %s
      ',
            jsonlite::toJSON(self$`certificate`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`configurationType`)) {
          sprintf(
            '"configurationType":
            "%s"
                  ',
            self$`configurationType`
          )
        },
        if (!is.null(self$`enableSso`)) {
          sprintf(
            '"enableSso":
            %s
                  ',
            tolower(self$`enableSso`)
          )
        },
        if (!is.null(self$`enforceSso`)) {
          sprintf(
            '"enforceSso":
            %s
                  ',
            tolower(self$`enforceSso`)
          )
        },
        if (!is.null(self$`entityId`)) {
          sprintf(
            '"entityId":
            "%s"
                  ',
            self$`entityId`
          )
        },
        if (!is.null(self$`groupMapping`)) {
          sprintf(
            '"groupMapping":
            [%s]
      ',
            paste(sapply(self$`groupMapping`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`idpMetadata`)) {
          sprintf(
            '"idpMetadata":
            %s
      ',
            jsonlite::toJSON(self$`idpMetadata`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`idpMetadataHttpsVerify`)) {
          sprintf(
            '"idpMetadataHttpsVerify":
            %s
                  ',
            tolower(self$`idpMetadataHttpsVerify`)
          )
        },
        if (!is.null(self$`idpMetadataUrl`)) {
          sprintf(
            '"idpMetadataUrl":
            "%s"
                  ',
            self$`idpMetadataUrl`
          )
        },
        if (!is.null(self$`idpResponseMethod`)) {
          sprintf(
            '"idpResponseMethod":
            "%s"
                  ',
            self$`idpResponseMethod`
          )
        },
        if (!is.null(self$`issuer`)) {
          sprintf(
            '"issuer":
            "%s"
                  ',
            self$`issuer`
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
        if (!is.null(self$`organizationId`)) {
          sprintf(
            '"organizationId":
            "%s"
                  ',
            self$`organizationId`
          )
        },
        if (!is.null(self$`roleMapping`)) {
          sprintf(
            '"roleMapping":
            [%s]
      ',
            paste(sapply(self$`roleMapping`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`securityParameters`)) {
          sprintf(
            '"securityParameters":
            %s
      ',
            jsonlite::toJSON(self$`securityParameters`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`sessionLengthSeconds`)) {
          sprintf(
            '"sessionLengthSeconds":
            %d
                  ',
            self$`sessionLengthSeconds`
          )
        },
        if (!is.null(self$`signOnUrl`)) {
          sprintf(
            '"signOnUrl":
            "%s"
                  ',
            self$`signOnUrl`
          )
        },
        if (!is.null(self$`signOutUrl`)) {
          sprintf(
            '"signOutUrl":
            "%s"
                  ',
            self$`signOutUrl`
          )
        },
        if (!is.null(self$`spRequestMethod`)) {
          sprintf(
            '"spRequestMethod":
            "%s"
                  ',
            self$`spRequestMethod`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param CreateSsoConfigurationJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(CreateSsoConfigurationJson, validateParams = FALSE) {
      CreateSsoConfigurationObject <- jsonlite::fromJSON(CreateSsoConfigurationJson)
      self$`attributeMapping` <- SamlAttributeMapping$new()$fromJSON(jsonlite::toJSON(CreateSsoConfigurationObject$attributeMapping, auto_unbox = TRUE, digits = NA, null = "null"))
      self$`autoGenerateUsers` <- CreateSsoConfigurationObject$`autoGenerateUsers`
      self$`certificate` <- SamlCertificate$new()$fromJSON(jsonlite::toJSON(CreateSsoConfigurationObject$certificate, auto_unbox = TRUE, digits = NA, null = "null"))
      self$`configurationType` <- CreateSsoConfigurationObject$`configurationType`
      self$`enableSso` <- CreateSsoConfigurationObject$`enableSso`
      self$`enforceSso` <- CreateSsoConfigurationObject$`enforceSso`
      self$`entityId` <- CreateSsoConfigurationObject$`entityId`
      self$`groupMapping` <- ApiClient$new()$deserializeObj(CreateSsoConfigurationObject$`groupMapping`, "array[SamlGroupMapping]", loadNamespace("datarobot.apicore"))
      self$`idpMetadata` <- SamlMetadataFile$new()$fromJSON(jsonlite::toJSON(CreateSsoConfigurationObject$idpMetadata, auto_unbox = TRUE, digits = NA, null = "null"))
      self$`idpMetadataHttpsVerify` <- CreateSsoConfigurationObject$`idpMetadataHttpsVerify`
      self$`idpMetadataUrl` <- CreateSsoConfigurationObject$`idpMetadataUrl`
      self$`idpResponseMethod` <- CreateSsoConfigurationObject$`idpResponseMethod`
      self$`issuer` <- CreateSsoConfigurationObject$`issuer`
      self$`name` <- CreateSsoConfigurationObject$`name`
      self$`organizationId` <- CreateSsoConfigurationObject$`organizationId`
      self$`roleMapping` <- ApiClient$new()$deserializeObj(CreateSsoConfigurationObject$`roleMapping`, "array[SamlRoleMapping]", loadNamespace("datarobot.apicore"))
      self$`securityParameters` <- SamlSecurityParameters$new()$fromJSON(jsonlite::toJSON(CreateSsoConfigurationObject$securityParameters, auto_unbox = TRUE, digits = NA, null = "null"))
      self$`sessionLengthSeconds` <- CreateSsoConfigurationObject$`sessionLengthSeconds`
      self$`signOnUrl` <- CreateSsoConfigurationObject$`signOnUrl`
      self$`signOutUrl` <- CreateSsoConfigurationObject$`signOutUrl`
      self$`spRequestMethod` <- CreateSsoConfigurationObject$`spRequestMethod`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
