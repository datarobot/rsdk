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
#' @title LocalFileDataStreamer
#'
#' @description LocalFileDataStreamer Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field async  character [optional] The default behavior (async: true) will still submit the job to the queue and start processing as soon as the upload is started.Setting it to false will postpone submitting the job to the queue until all data has been uploaded.This is helpful if the user is on a bad connection and bottlednecked by the upload speed. Instead of blocking the queue this will allow others to submit to the queue until the upload has finished.
#'
#' @field multipart  character [optional] specify if the data will be uploaded in multiple parts instead of a single file
#'
#' @field type  character Type name for this intake type
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
LocalFileDataStreamer <- R6::R6Class(
  "LocalFileDataStreamer",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`async` = NULL, `multipart` = NULL, `type` = NULL) {
      if (!is.null(`type`)) {
        stopifnot(is.character(`type`), length(`type`) == 1)
      }
      if (!is.null(`async`)) {
        stopifnot(is.logical(`async`), length(`async`) == 1)
      }
      if (!is.null(`multipart`)) {
        stopifnot(is.logical(`multipart`), length(`multipart`) == 1)
      }
    }
  ),
  public = list(
    `async` = NULL,
    `multipart` = NULL,
    `type` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param async The default behavior (async: true) will still submit the job to the queue and start processing as soon as the upload is started.Setting it to false will postpone submitting the job to the queue until all data has been uploaded.This is helpful if the user is on a bad connection and bottlednecked by the upload speed. Instead of blocking the queue this will allow others to submit to the queue until the upload has finished.
    #' @param multipart specify if the data will be uploaded in multiple parts instead of a single file
    #' @param type Type name for this intake type
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`type` = NULL, `async` = NULL, `multipart` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`type`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(async, multipart, type)
      }
      self$`async` <- `async`
      self$`multipart` <- `multipart`
      self$`type` <- `type`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(async = self$`async`, multipart = self$`multipart`, type = self$`type`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`async`)) {
          sprintf(
            '"async":
            %s
                  ',
            tolower(self$`async`)
          )
        },
        if (!is.null(self$`multipart`)) {
          sprintf(
            '"multipart":
            %s
                  ',
            tolower(self$`multipart`)
          )
        },
        if (!is.null(self$`type`)) {
          sprintf(
            '"type":
            "%s"
                  ',
            self$`type`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param LocalFileDataStreamerJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(LocalFileDataStreamerJson, validateParams = FALSE) {
      LocalFileDataStreamerObject <- jsonlite::fromJSON(LocalFileDataStreamerJson)
      self$`async` <- LocalFileDataStreamerObject$`async`
      self$`multipart` <- LocalFileDataStreamerObject$`multipart`
      self$`type` <- LocalFileDataStreamerObject$`type`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
