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
#' @title SectionCustom
#'
#' @description SectionCustom Class
#'
#' @format An \code{R6Class} generator object
#'
#' @field description  character [optional] Section description
#'
#' @field highlightedText  character Highlighted text of the section, optionally separated by &#x60;&#x60;\\n&#x60;&#x60; to split paragraphs.
#'
#' @field instructions  \link{InstructionsField} [optional]
#'
#' @field locked  character [optional] Locked section flag
#'
#' @field regularText  character Regular text of the section, optionally separated by &#x60;&#x60;\\n&#x60;&#x60; to split paragraphs.
#'
#' @field sections  list( object ) [optional] List of section objects representing the structure of the document. Each section can have sub-sections that have the same schema as the parent section, e.g. the structure is recursive. The limit of nesting sections is 5. Total number of sections is limited to 500.
#'
#' @field title  character Section Title
#'
#' @field type  character Section with user-defined content. It can be a section title or summary.
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
SectionCustom <- R6::R6Class(
  "SectionCustom",
  lock_objects = FALSE,
  private = list(
    # @description The properties of this object that are required to be set.
    # @description A helper function to handle assist with type validation. This function will validate class parameters with definite
    # types assigned to them, as well as handling validation of parameters with anyOf and oneOf types listed. These types
    # can themselves be other R6 objects.
    validateProps = function(`description` = NULL, `highlightedText` = NULL, `instructions` = NULL, `locked` = NULL, `regularText` = NULL, `sections` = NULL, `title` = NULL, `type` = NULL) {
      if (!is.null(`highlightedText`)) {
        stopifnot(is.character(`highlightedText`), length(`highlightedText`) == 1)
      }
      if (!is.null(`regularText`)) {
        stopifnot(is.character(`regularText`), length(`regularText`) == 1)
      }
      if (!is.null(`title`)) {
        stopifnot(is.character(`title`), length(`title`) == 1)
      }
      if (!is.null(`type`)) {
        stopifnot(is.character(`type`), length(`type`) == 1)
      }
      if (!is.null(`description`)) {
        stopifnot(is.character(`description`), length(`description`) == 1)
      }
      if (!is.null(`instructions`)) {
        stopifnot(R6::is.R6(`instructions`))
      }
      if (!is.null(`locked`)) {
        stopifnot(is.logical(`locked`), length(`locked`) == 1)
      }
      if (!is.null(`sections`) && length(`sections`) > 0) {
        stopifnot(is.vector(`sections`), sapply(`sections`, is.character))
      }
    }
  ),
  public = list(
    `description` = NULL,
    `highlightedText` = NULL,
    `instructions` = NULL,
    `locked` = NULL,
    `regularText` = NULL,
    `sections` = NULL,
    `title` = NULL,
    `type` = NULL,
    #' @description A function used to initialize an instance of this class.
    #' @param description Section description
    #' @param highlightedText Highlighted text of the section, optionally separated by &#x60;&#x60;\\n&#x60;&#x60; to split paragraphs.
    #' @param instructions
    #' @param locked Locked section flag
    #' @param regularText Regular text of the section, optionally separated by &#x60;&#x60;\\n&#x60;&#x60; to split paragraphs.
    #' @param sections List of section objects representing the structure of the document. Each section can have sub-sections that have the same schema as the parent section, e.g. the structure is recursive. The limit of nesting sections is 5. Total number of sections is limited to 500.
    #' @param title Section Title
    #' @param type Section with user-defined content. It can be a section title or summary.
    #' @param validateParams An optional param for auto validating this object's parameters before initialization. Default FALSE.
    #' @param ... Any additional keyword arguments to be passed into this object for initialization.
    initialize = function(`highlightedText` = NULL, `regularText` = NULL, `title` = NULL, `type` = NULL, `description` = NULL, `instructions` = NULL, `locked` = NULL, `sections` = NULL, validateParams = FALSE, ...) {
      local.optional.var <- list(...)
      if (validateParams) {
        lapply(list(`highlightedText`, `regularText`, `title`, `type`), function(param) {
          stopifnot("Required param not set." = !is.null(param))
        })
        private$validateProps(description, highlightedText, instructions, locked, regularText, sections, title, type)
      }
      self$`description` <- `description`
      self$`highlightedText` <- `highlightedText`
      self$`instructions` <- `instructions`
      self$`locked` <- `locked`
      self$`regularText` <- `regularText`
      self$`sections` <- `sections`
      self$`title` <- `title`
      self$`type` <- `type`
    },
    #' @description A helper function that provides public access to the private validateProps function. This allows users the ability
    #' to programmatically validate objects before sending them to DataRobot.
    #' checking this objects set properties.
    validate = function() {
      do.call(private$validateProps, list(description = self$`description`, highlightedText = self$`highlightedText`, instructions = self$`instructions`, locked = self$`locked`, regularText = self$`regularText`, sections = self$`sections`, title = self$`title`, type = self$`type`))
    },
    #' @description A helper function that serializes this object into a JSON encoded string.
    toJSON = function() {
      jsoncontent <- c(
        if (!is.null(self$`description`)) {
          sprintf(
            '"description":
            "%s"
                  ',
            self$`description`
          )
        },
        if (!is.null(self$`highlightedText`)) {
          sprintf(
            '"highlightedText":
            "%s"
                  ',
            self$`highlightedText`
          )
        },
        if (!is.null(self$`instructions`)) {
          sprintf(
            '"instructions":
            %s
      ',
            jsonlite::toJSON(self$`instructions`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`locked`)) {
          sprintf(
            '"locked":
            %s
                  ',
            tolower(self$`locked`)
          )
        },
        if (!is.null(self$`regularText`)) {
          sprintf(
            '"regularText":
            "%s"
                  ',
            self$`regularText`
          )
        },
        if (!is.null(self$`sections`)) {
          sprintf(
            '"sections":
            [%s]
                  ',
            paste(unlist(lapply(self$`sections`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`title`)) {
          sprintf(
            '"title":
            "%s"
                  ',
            self$`title`
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
    #' @param SectionCustomJson A JSON encoded string representation of a class instance.
    #' @param validateParams An optional param for auto validating this object's parameters after deserialization. Default FALSE.
    fromJSON = function(SectionCustomJson, validateParams = FALSE) {
      SectionCustomObject <- jsonlite::fromJSON(SectionCustomJson)
      self$`description` <- SectionCustomObject$`description`
      self$`highlightedText` <- SectionCustomObject$`highlightedText`
      self$`instructions` <- InstructionsField$new()$fromJSON(jsonlite::toJSON(SectionCustomObject$instructions, auto_unbox = TRUE, digits = NA, null = "null"))
      self$`locked` <- SectionCustomObject$`locked`
      self$`regularText` <- SectionCustomObject$`regularText`
      self$`sections` <- ApiClient$new()$deserializeObj(SectionCustomObject$`sections`, "array[object]", loadNamespace("datarobot.apicore"))
      self$`title` <- SectionCustomObject$`title`
      self$`type` <- SectionCustomObject$`type`

      if (validateParams) {
        self$validate()
      }

      return(self)
    }
  )
)
