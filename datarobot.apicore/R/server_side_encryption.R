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
#' @title ServerSideEncryption
#'
#' @description ServerSideEncryption Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field algorithm  character [optional] The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
#'
#' @field customerAlgorithm  character [optional] Specifies the algorithm to use to when encrypting the object (for example, AES256).
#'
#' @field customerKey  character [optional] Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in customerAlgorithm. The key must be sent as an base64 encoded string.
#'
#' @field kmsEncryptionContext  character [optional] Specifies the Amazon Web Services KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
#'
#' @field kmsKeyId  character [optional] Specifies the ID of the symmetric customer managed key to use for object encryption.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
ServerSideEncryption <- R6::R6Class(
  "ServerSideEncryption",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`algorithm` = NULL, `customerAlgorithm` = NULL, `customerKey` = NULL, `kmsEncryptionContext` = NULL, `kmsKeyId` = NULL) {
      if (!is.null(`algorithm`)) {
        stopifnot(is.character(`algorithm`), length(`algorithm`) == 1)
      }
      if (!is.null(`customerAlgorithm`)) {
        stopifnot(is.character(`customerAlgorithm`), length(`customerAlgorithm`) == 1)
      }
      if (!is.null(`customerKey`)) {
        stopifnot(is.character(`customerKey`), length(`customerKey`) == 1)
      }
      if (!is.null(`kmsEncryptionContext`)) {
        stopifnot(is.character(`kmsEncryptionContext`), length(`kmsEncryptionContext`) == 1)
      }
      if (!is.null(`kmsKeyId`)) {
        stopifnot(is.character(`kmsKeyId`), length(`kmsKeyId`) == 1)
      }
    }
  ),
  public = list(
    `algorithm` = NULL,
    `customerAlgorithm` = NULL,
    `customerKey` = NULL,
    `kmsEncryptionContext` = NULL,
    `kmsKeyId` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param algorithm The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).
    #' @param customerAlgorithm Specifies the algorithm to use to when encrypting the object (for example, AES256).
    #' @param customerKey Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in customerAlgorithm. The key must be sent as an base64 encoded string.
    #' @param kmsEncryptionContext Specifies the Amazon Web Services KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.
    #' @param kmsKeyId Specifies the ID of the symmetric customer managed key to use for object encryption.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`algorithm` = NULL, `customerAlgorithm` = NULL, `customerKey` = NULL, `kmsEncryptionContext` = NULL, `kmsKeyId` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(algorithm, customerAlgorithm, customerKey, kmsEncryptionContext, kmsKeyId)
      }
      self$`algorithm` <- `algorithm`
      self$`customerAlgorithm` <- `customerAlgorithm`
      self$`customerKey` <- `customerKey`
      self$`kmsEncryptionContext` <- `kmsEncryptionContext`
      self$`kmsKeyId` <- `kmsKeyId`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(algorithm = self$`algorithm`, customerAlgorithm = self$`customerAlgorithm`, customerKey = self$`customerKey`, kmsEncryptionContext = self$`kmsEncryptionContext`, kmsKeyId = self$`kmsKeyId`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`algorithm`)) {
          sprintf(
            '"algorithm":
            "%s"
                  ',
            self$`algorithm`
          )
        },
        if (!is.null(self$`customerAlgorithm`)) {
          sprintf(
            '"customerAlgorithm":
            "%s"
                  ',
            self$`customerAlgorithm`
          )
        },
        if (!is.null(self$`customerKey`)) {
          sprintf(
            '"customerKey":
            "%s"
                  ',
            self$`customerKey`
          )
        },
        if (!is.null(self$`kmsEncryptionContext`)) {
          sprintf(
            '"kmsEncryptionContext":
            "%s"
                  ',
            self$`kmsEncryptionContext`
          )
        },
        if (!is.null(self$`kmsKeyId`)) {
          sprintf(
            '"kmsKeyId":
            "%s"
                  ',
            self$`kmsKeyId`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param ServerSideEncryptionJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(ServerSideEncryptionJson, validateParams = FALSE) {
      ServerSideEncryptionObject <- jsonlite::fromJSON(ServerSideEncryptionJson)
      self$`algorithm` <- ServerSideEncryptionObject$`algorithm`
      self$`customerAlgorithm` <- ServerSideEncryptionObject$`customerAlgorithm`
      self$`customerKey` <- ServerSideEncryptionObject$`customerKey`
      self$`kmsEncryptionContext` <- ServerSideEncryptionObject$`kmsEncryptionContext`
      self$`kmsKeyId` <- ServerSideEncryptionObject$`kmsKeyId`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
