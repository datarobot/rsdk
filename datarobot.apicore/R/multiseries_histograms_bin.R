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
#' @title MultiseriesHistogramsBin
#'
#' @description MultiseriesHistogramsBin Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field count  integer The value count of the bin
#'
#' @field left  \link{OneOfnumberstring} The inclusive left boundary of the bin.
#'
#' @field right  \link{OneOfnumberstring} The exclusive right boundary of the bin. The last bin has an inclusive right boundary.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
MultiseriesHistogramsBin <- R6::R6Class(
  "MultiseriesHistogramsBin",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`count` = NULL, `left` = NULL, `right` = NULL) {
      if (!is.null(`count`)) {
        stopifnot(is.numeric(`count`), length(`count`) == 1)
      }
      if (!is.null(`left`)) {
        .setPrimitiveProperty(typeList = list("numeric", "character"), propertyData = left)
      }
      if (!is.null(`right`)) {
        .setPrimitiveProperty(typeList = list("numeric", "character"), propertyData = right)
      }
    }
  ),
  public = list(
    `count` = NULL,
    `left` = NULL,
    `right` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param count The value count of the bin
    #' @param left The inclusive left boundary of the bin.
    #' @param right The exclusive right boundary of the bin. The last bin has an inclusive right boundary.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`count` = NULL, `left` = NULL, `right` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`count`, `left`, `right`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(count, left, right)
      }
      self$`count` <- `count`
      self$`left` <- .setPrimitiveProperty(typeList = list("numeric", "character"), propertyData = left)
      self$`right` <- .setPrimitiveProperty(typeList = list("numeric", "character"), propertyData = right)
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(count = self$`count`, left = self$`left`, right = self$`right`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`count`)) {
          sprintf(
            '"count":
            %d
                  ',
            self$`count`
          )
        },
        if (!is.null(self$`left`)) {
          sprintf(
            '"left":
            %s
      ',
            self$`left`
          )
        },
        if (!is.null(self$`right`)) {
          sprintf(
            '"right":
            %s
      ',
            self$`right`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      paste("{", jsoncontent, "}", sep = "")
    },
    #' @description A helper function that deserializes a JSON string into an instance of this class.
    #' @param MultiseriesHistogramsBinJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(MultiseriesHistogramsBinJson, validateParams = FALSE) {
      MultiseriesHistogramsBinObject <- jsonlite::fromJSON(MultiseriesHistogramsBinJson)
      self$`count` <- MultiseriesHistogramsBinObject$`count`
      self$`left` <- .setPrimitiveProperty(typeList = list("numeric", "character"), propertyData = MultiseriesHistogramsBinObject$left)
      self$`right` <- .setPrimitiveProperty(typeList = list("numeric", "character"), propertyData = MultiseriesHistogramsBinObject$right)

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
