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
#' @title SsoConfiguration operations
#' @description datarobot.apicore.SsoConfiguration
#' @format An \code{R6Class} generator object
#' @field apiClient Handles the client-server communication.
#'
#' @importFrom R6 R6Class
#' @export
SsoConfigurationApi <- R6::R6Class(
  "SsoConfigurationApi",
  public = list(
    apiClient = NULL,

    #' @param apiClient A configurable `ApiClient` instance. If none provided, a new client with default configuration will be created.
    initialize = function(apiClient) {
      if (!missing(apiClient)) {
        self$apiClient <- apiClient
      } else {
        self$apiClient <- ApiClient$new()
      }
    },
    #' @description Create an SSO configuration for a specific organization
    #' Produces: "application/json"
    #'
    #' @details Create an SSO configuration for a specific organization
    #' @details This method invokes `POST /ssoConfigurations/` in the DataRobot Public API.
    #' @param createSsoConfiguration \link{CreateSsoConfiguration}.
    #' @param ... Optional. Additional named parameters to be passed downward.
    #' @return \link{SsoConfigurationResponse}
    #' @details Response status codes, messages, and headers:
    #' \itemize{
    #' \item **`200`** Configuration created successfully
    #' \itemize{
    #' }
    #' }
    #' @examples
    #' \dontrun{
    #' library(datarobot.apicore)
    #' createSsoConfiguration <- CreateSsoConfiguration$new() # CreateSsoConfiguration |
    #'
    #' api.instance <- SsoConfigurationApi$new()
    #' result <- api.instance$SsoConfigurationsCreate(createSsoConfiguration=createSsoConfiguration)
    #' }
    SsoConfigurationsCreate = function(createSsoConfiguration = NULL, ...) {
      apiResponse <- private$SsoConfigurationsCreateWithHttpInfo(createSsoConfiguration, ...)
      resp <- apiResponse$response
      if (httr::status_code(resp) == 202) {
        # When the DataRobot Public API returns a 202, this means that
        # an asynchronous job or task has been started. The response will
        # not have a body, but will have a Location header pointing to an
        # endpoint for checking that job's status.
        apiResponse
      } else if (httr::status_code(resp) >= 200 && httr::status_code(resp) <= 299) {
        .ReturnResponse(apiResponse$content)
      } else if (httr::status_code(resp) >= 300 && httr::status_code(resp) <= 399) {
        apiResponse
      } else if (httr::status_code(resp) >= 400 && httr::status_code(resp) <= 499) {
        apiResponse
      } else if (httr::status_code(resp) >= 500 && httr::status_code(resp) <= 599) {
        apiResponse
      }
    },
    #' @description List sso configurations.
    #' Produces: "application/json"
    #'
    #' @details List the sso configurations that correspond to provided conditions.
    #' @details This method invokes `GET /ssoConfigurations/` in the DataRobot Public API.
    #' @param offset integer. The number of records to skip over.
    #' @param limit integer. The number of records to return.
    #' @param orgId character. The ID of the organization.
    #' @param ... Optional. Additional named parameters to be passed downward.
    #' @return \link{ListSsoConfigurationResponse}
    #' @details Response status codes, messages, and headers:
    #' \itemize{
    #' \item **`200`** List of sso configurations.
    #' \itemize{
    #' }
    #' }
    #' @examples
    #' \dontrun{
    #' library(datarobot.apicore)
    #' offset <- 0 # integer | The number of records to skip over.
    #' limit <- 100 # integer | The number of records to return.
    #' orgId <- 'orgId_example' # character | The ID of the organization.
    #'
    #' api.instance <- SsoConfigurationApi$new()
    #' result <- api.instance$SsoConfigurationsList(offset=offset, limit=limit, orgId=orgId)
    #' }
    SsoConfigurationsList = function(offset = 0, limit = 100, orgId = NULL, ...) {
      apiResponse <- private$SsoConfigurationsListWithHttpInfo(offset, limit, orgId, ...)
      resp <- apiResponse$response
      if (httr::status_code(resp) == 202) {
        # When the DataRobot Public API returns a 202, this means that
        # an asynchronous job or task has been started. The response will
        # not have a body, but will have a Location header pointing to an
        # endpoint for checking that job's status.
        apiResponse
      } else if (httr::status_code(resp) >= 200 && httr::status_code(resp) <= 299) {
        .ReturnResponse(apiResponse$content)
      } else if (httr::status_code(resp) >= 300 && httr::status_code(resp) <= 399) {
        apiResponse
      } else if (httr::status_code(resp) >= 400 && httr::status_code(resp) <= 499) {
        apiResponse
      } else if (httr::status_code(resp) >= 500 && httr::status_code(resp) <= 599) {
        apiResponse
      }
    },
    #' @description Update an SSO configuration for a specific organization.
    #' Produces: NA
    #'
    #' @details Update an SSO configuration for a specific organization.
    #' @details This method invokes `PATCH /ssoConfigurations/{configurationId}/` in the DataRobot Public API.
    #' @param configurationId character. The ID of the organization to retrieve SSO config for.
    #' @param updateSsoConfiguration \link{UpdateSsoConfiguration}.
    #' @param ... Optional. Additional named parameters to be passed downward.
    #' @details Response status codes, messages, and headers:
    #' \itemize{
    #' \item **`200`**
    #' \itemize{
    #' }
    #' }
    #' @examples
    #' \dontrun{
    #' library(datarobot.apicore)
    #' configurationId <- 'configurationId_example' # character | The ID of the organization to retrieve SSO config for.
    #' updateSsoConfiguration <- UpdateSsoConfiguration$new() # UpdateSsoConfiguration |
    #'
    #' api.instance <- SsoConfigurationApi$new()
    #' result <- api.instance$SsoConfigurationsPatch(configurationId, updateSsoConfiguration=updateSsoConfiguration)
    #' }
    SsoConfigurationsPatch = function(configurationId, updateSsoConfiguration = NULL, ...) {
      apiResponse <- private$SsoConfigurationsPatchWithHttpInfo(configurationId, updateSsoConfiguration, ...)
      resp <- apiResponse$response
      if (httr::status_code(resp) == 202) {
        # When the DataRobot Public API returns a 202, this means that
        # an asynchronous job or task has been started. The response will
        # not have a body, but will have a Location header pointing to an
        # endpoint for checking that job's status.
        apiResponse
      } else if (httr::status_code(resp) >= 200 && httr::status_code(resp) <= 299) {
        if (httr::has_content(resp)) {
          httr::content(resp)
        }
      } else if (httr::status_code(resp) >= 300 && httr::status_code(resp) <= 399) {
        apiResponse
      } else if (httr::status_code(resp) >= 400 && httr::status_code(resp) <= 499) {
        apiResponse
      } else if (httr::status_code(resp) >= 500 && httr::status_code(resp) <= 599) {
        apiResponse
      }
    },
    #' @description Retrieve SSO configuration of a specific organization.
    #' Produces: "application/json"
    #'
    #' @details Retrieve SSO configuration of a specific organization.
    #' @details This method invokes `GET /ssoConfigurations/{configurationId}/` in the DataRobot Public API.
    #' @param configurationId character. The ID of the organization to retrieve SSO config for.
    #' @param ... Optional. Additional named parameters to be passed downward.
    #' @return \link{SsoConfigurationResponse}
    #' @details Response status codes, messages, and headers:
    #' \itemize{
    #' \item **`200`** SSO configuration.
    #' \itemize{
    #' }
    #' }
    #' @examples
    #' \dontrun{
    #' library(datarobot.apicore)
    #' configurationId <- 'configurationId_example' # character | The ID of the organization to retrieve SSO config for.
    #'
    #' api.instance <- SsoConfigurationApi$new()
    #' result <- api.instance$SsoConfigurationsRetrieve(configurationId)
    #' }
    SsoConfigurationsRetrieve = function(configurationId, ...) {
      apiResponse <- private$SsoConfigurationsRetrieveWithHttpInfo(configurationId, ...)
      resp <- apiResponse$response
      if (httr::status_code(resp) == 202) {
        # When the DataRobot Public API returns a 202, this means that
        # an asynchronous job or task has been started. The response will
        # not have a body, but will have a Location header pointing to an
        # endpoint for checking that job's status.
        apiResponse
      } else if (httr::status_code(resp) >= 200 && httr::status_code(resp) <= 299) {
        .ReturnResponse(apiResponse$content)
      } else if (httr::status_code(resp) >= 300 && httr::status_code(resp) <= 399) {
        apiResponse
      } else if (httr::status_code(resp) >= 400 && httr::status_code(resp) <= 499) {
        apiResponse
      } else if (httr::status_code(resp) >= 500 && httr::status_code(resp) <= 599) {
        apiResponse
      }
    }
  ),
  private = list(
    # A helper function to invoke the API operation `SsoConfigurationsCreate`. This function is responsible for
    # validating request parameters, building the request, deserializing the response, and handling errors.
    SsoConfigurationsCreateWithHttpInfo = function(createSsoConfiguration = NULL, ...) {
      args <- list(...)
      queryParams <- list()
      headerParams <- c()

      if (!missing(`createSsoConfiguration`) && isa(createSsoConfiguration, c("CreateSsoConfiguration", "R6"))) {
        body <- `createSsoConfiguration`$toJSON()
      } else {
        stop("SsoConfigurationsCreateWithHttpInfo requires parameter createSsoConfiguration to be of type CreateSsoConfiguration.")
      }

      urlPath <- "/ssoConfigurations/"

      resp <- self$apiClient$CallApi(
        url = paste0(self$apiClient$basePath, urlPath),
        method = "POST",
        queryParams = queryParams,
        headerParams = headerParams,
        body = body,
        ...
      )

      if (httr::status_code(resp) >= 200 && httr::status_code(resp) <= 299) {
        deserializedRespObj <- self$apiClient$deserialize(resp, "SsoConfigurationResponse", loadNamespace("datarobot.apicore"))
        ApiResponse$new(deserializedRespObj, resp)
      } else if (httr::status_code(resp) >= 300 && httr::status_code(resp) <= 399) {
        ApiResponse$new(paste("Server returned ", httr::status_code(resp), " response status code. See $response for more detail."), resp)
      } else if (httr::status_code(resp) >= 400 && httr::status_code(resp) <= 499) {
        ApiResponse$new(paste("API client error with ", httr::status_code(resp), " response status code. See $response for more detail."), resp)
      } else if (httr::status_code(resp) >= 500 && httr::status_code(resp) <= 599) {
        ApiResponse$new(paste("API server error with ", httr::status_code(resp), " response status code. See $response for more detail."), resp)
      }
    },
    # A helper function to invoke the API operation `SsoConfigurationsList`. This function is responsible for
    # validating request parameters, building the request, deserializing the response, and handling errors.
    SsoConfigurationsListWithHttpInfo = function(offset = 0, limit = 100, orgId = NULL, ...) {
      args <- list(...)
      queryParams <- list()
      headerParams <- c()

      queryParams["offset"] <- offset

      queryParams["limit"] <- limit

      queryParams["orgId"] <- orgId

      body <- NULL
      urlPath <- "/ssoConfigurations/"

      resp <- self$apiClient$CallApi(
        url = paste0(self$apiClient$basePath, urlPath),
        method = "GET",
        queryParams = queryParams,
        headerParams = headerParams,
        body = body,
        ...
      )

      if (httr::status_code(resp) >= 200 && httr::status_code(resp) <= 299) {
        deserializedRespObj <- self$apiClient$deserialize(resp, "ListSsoConfigurationResponse", loadNamespace("datarobot.apicore"))
        ApiResponse$new(deserializedRespObj, resp)
      } else if (httr::status_code(resp) >= 300 && httr::status_code(resp) <= 399) {
        ApiResponse$new(paste("Server returned ", httr::status_code(resp), " response status code. See $response for more detail."), resp)
      } else if (httr::status_code(resp) >= 400 && httr::status_code(resp) <= 499) {
        ApiResponse$new(paste("API client error with ", httr::status_code(resp), " response status code. See $response for more detail."), resp)
      } else if (httr::status_code(resp) >= 500 && httr::status_code(resp) <= 599) {
        ApiResponse$new(paste("API server error with ", httr::status_code(resp), " response status code. See $response for more detail."), resp)
      }
    },
    # A helper function to invoke the API operation `SsoConfigurationsPatch`. This function is responsible for
    # validating request parameters, building the request, deserializing the response, and handling errors.
    SsoConfigurationsPatchWithHttpInfo = function(configurationId, updateSsoConfiguration = NULL, ...) {
      args <- list(...)
      queryParams <- list()
      headerParams <- c()

      if (missing(`configurationId`)) {
        stop("Missing required parameter `configurationId`.")
      }

      if (!missing(`updateSsoConfiguration`) && isa(updateSsoConfiguration, c("UpdateSsoConfiguration", "R6"))) {
        body <- `updateSsoConfiguration`$toJSON()
      } else {
        stop("SsoConfigurationsPatchWithHttpInfo requires parameter updateSsoConfiguration to be of type UpdateSsoConfiguration.")
      }

      urlPath <- "/ssoConfigurations/{configurationId}/"
      if (!missing(`configurationId`)) {
        urlPath <- gsub(paste0("\\{", "configurationId", "\\}"), URLencode(as.character(`configurationId`), reserved = TRUE), urlPath)
      }


      resp <- self$apiClient$CallApi(
        url = paste0(self$apiClient$basePath, urlPath),
        method = "PATCH",
        queryParams = queryParams,
        headerParams = headerParams,
        body = body,
        ...
      )

      if (httr::status_code(resp) >= 200 && httr::status_code(resp) <= 299) {
        ApiResponse$new(NULL, resp)
      } else if (httr::status_code(resp) >= 300 && httr::status_code(resp) <= 399) {
        ApiResponse$new(paste("Server returned ", httr::status_code(resp), " response status code. See $response for more detail."), resp)
      } else if (httr::status_code(resp) >= 400 && httr::status_code(resp) <= 499) {
        ApiResponse$new(paste("API client error with ", httr::status_code(resp), " response status code. See $response for more detail."), resp)
      } else if (httr::status_code(resp) >= 500 && httr::status_code(resp) <= 599) {
        ApiResponse$new(paste("API server error with ", httr::status_code(resp), " response status code. See $response for more detail."), resp)
      }
    },
    # A helper function to invoke the API operation `SsoConfigurationsRetrieve`. This function is responsible for
    # validating request parameters, building the request, deserializing the response, and handling errors.
    SsoConfigurationsRetrieveWithHttpInfo = function(configurationId, ...) {
      args <- list(...)
      queryParams <- list()
      headerParams <- c()

      if (missing(`configurationId`)) {
        stop("Missing required parameter `configurationId`.")
      }

      body <- NULL
      urlPath <- "/ssoConfigurations/{configurationId}/"
      if (!missing(`configurationId`)) {
        urlPath <- gsub(paste0("\\{", "configurationId", "\\}"), URLencode(as.character(`configurationId`), reserved = TRUE), urlPath)
      }


      resp <- self$apiClient$CallApi(
        url = paste0(self$apiClient$basePath, urlPath),
        method = "GET",
        queryParams = queryParams,
        headerParams = headerParams,
        body = body,
        ...
      )

      if (httr::status_code(resp) >= 200 && httr::status_code(resp) <= 299) {
        deserializedRespObj <- self$apiClient$deserialize(resp, "SsoConfigurationResponse", loadNamespace("datarobot.apicore"))
        ApiResponse$new(deserializedRespObj, resp)
      } else if (httr::status_code(resp) >= 300 && httr::status_code(resp) <= 399) {
        ApiResponse$new(paste("Server returned ", httr::status_code(resp), " response status code. See $response for more detail."), resp)
      } else if (httr::status_code(resp) >= 400 && httr::status_code(resp) <= 499) {
        ApiResponse$new(paste("API client error with ", httr::status_code(resp), " response status code. See $response for more detail."), resp)
      } else if (httr::status_code(resp) >= 500 && httr::status_code(resp) <= 599) {
        ApiResponse$new(paste("API server error with ", httr::status_code(resp), " response status code. See $response for more detail."), resp)
      }
    }
  )
)
