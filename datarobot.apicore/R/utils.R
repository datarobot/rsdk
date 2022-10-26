# Copyright 2021-2022 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.

#' A helper method to return the content of an API response.
#'
#' @return `apiResponse.content` as an R6 object or an S3 list, dependent on the value of the
#' option `datarobot.apicore.returnS3`
#' @export
#' @md
.ReturnResponse <- function(apiResponse.content) {
  if (getOption("datarobot.apicore.returnS3", default = TRUE)) {
    return(.toS3(apiResponse.content))
  }

  return(apiResponse.content)
}

#' A helper method to return an arbitrary object as an S3 object or as is.
#'
#' If the object parameter is an R6 object, it will be converted to a list and all of the R6
#' functions dropped. The R6 class name will be preserved.
#'
#' This requires that the R6 object have the property `lock_objects = FALSE`; otherwise errors will
#' be thrown.
#' @export
#' @md
.toS3 <- function(potential.R6.object) {
  s3.object <- potential.R6.object

  if (R6::is.R6(s3.object)) {
    # drop all functions, keep only fields
    rm(list = utils::lsf.str(s3.object), envir = s3.object)
    # convert all fields recursively
    s3.object <- eapply(s3.object, .toS3)
    # maintain class name
    class(s3.object) <- class(potential.R6.object)[1]
  } else if (is.list(potential.R6.object)) {
    s3.object <- lapply(s3.object, .toS3)
  }
  # TODO matrix/array, data.frame, vectors, factors?
  # TODO sort fields, maybe as an option?
  # TODO Maintain existence of raw response (JSON string) field
  return(s3.object)
}

#' Validate and set complex types
#'
#' A helper method to validate complex types for anyOf and oneOf properties in an R6 object.
#'
#' @param typeList An object vector of R6 class generators.
#' @param propertyData A deserialized JSON string, OR an R6 object instance.
#' @return The R6 input object if it's one of the listed types in `typeList`, or a newly instantiated
#' R6 object based on the input map if that map can be converted into one of the types in `typeList`, or NULL if input is NULL.
#' @export
#' @md
.setComplexProperty <- function(typeList, propertyData) {
  # TODO DSX-2493 convert this from an object vector to a character vector
  stopifnot(sapply(typeList, R6::is.R6Class))
  if (is.null(propertyData)) {
    # base case: if NULL, return NULL
    return(propertyData)
  }
  for (typeOption in typeList) {
    if (R6::is.R6(propertyData) && is(propertyData, typeOption$classname)) {
      return(propertyData)
    }
    property <- tryCatch(
      do.call(typeOption$new, c(propertyData)),
      error = function(e) {
        NULL
      }
    )
    if (!is.null(property)) {
      return(property)
    }
  }
  stop(paste(quote(propertyData), "is not one of", paste0(typeList, collapse = "|")))
}

#' Validate and set primitive types
#'
#' A helper method to validate simple types for anyOf and oneOf properties in an R6 object.
#' @param typeList A character vector of primitive type names, e.g. "numeric" or "logical".
#' @param propertyData An R object, possibly a primitive, output of `jsonlite::fromJSON()`.
#' @return The value of `propertyData` if the value is one of the types listed in `typeList`.
#' A helper method to validate simple types for anyOf and oneOf properties in an R6 object.
#'
#' @export
#' @md
.setPrimitiveProperty <- function(typeList, propertyData) {
  for (typeOption in typeList) {
    checkType <- switch(typeOption,
      "numeric" = is.numeric, # good for both doubles and integers
      "logical" = is.logical,
      "array" = is.array,
      "character" = is.character
    )
    isPrimitive <- tryCatch(
      checkType(propertyData),
      error = function(e) {
        NULL
      }
    )
    if (is.null(propertyData) || isPrimitive) {
      return(propertyData)
    }
  }
  stop(paste(propertyData, "is not one of", paste0(typeList, collapse = "|")))
}

.isR6ClassGeneratorName <- function(generatorName) {
  tryCatch(R6::is.R6Class(eval(str2lang(generatorName))),
    error = function(x) FALSE
  )
}

#' Validate and set mixed complex and primitive types
#'
#' A helper method to validate mixed types for anyOf and oneOf properties in an R6 object.
#'
#' @param typeList A character vector of property types. The list is guaranteed to be ordered with complex types before primitive types.
#' @param propertyData A deserialized JSON string, OR an R6 object instance.
#' @return If `typeOption` is a complex type, this function will return an R6 object that was provided if it's one of the listed types in `typeList`,
#' or a newly instantiated R6 object based on the input map if that map can be converted into one of the types in `typeList`.
#' If `typeOption` is a primitive, this function will return `propertyData` iff the value is one of the types listed in `typeList`.
#' A helper method to validate mixed types for anyOf and oneOf properties in an R6 object.
#'
#' @export
#' @md
.setMixedProperty <- function(typeList, propertyData) {
  stopifnot(sapply(typeList, is.character))
  if (is.null(propertyData)) {
    # base case: if NULL, return NULL
    return(propertyData)
  }
  for (typeOption in typeList) {
    # since the implicit guarantee is that complex types are first, let's test for complex properties first.
    if (.isR6ClassGeneratorName(typeOption)) {
      # this is probably a complex property
      property <- tryCatch(
        # setComplexProperty takes a vector of R6 generator classes
        .setComplexProperty(c(eval(str2lang(typeOption))), propertyData),
        error = function(e) {
          NULL
        }
      )
      if (!is.null(property)) {
        return(property)
      }
    } else {
      # this is probably a primitive property
      property <- tryCatch(
        .setPrimitiveProperty(list(typeOption), propertyData),
        error = function(e) {
          NULL
        }
      )
      if (!is.null(property) && property) {
        return(propertyData)
      }
    }
  }
  stop(paste(quote(x), "is not one of", paste0(typeList, collapse = "|")))
}

#' RFC 3339 datetime format
#'
#' The DataRobot API returns dates in RFC 3339 format. Since this comes from a
#' Python datetime object, we assume that the period returned is in the format
#' "%Y-%m-%dT%H:%M:%OSZ".
#' @family API datetime functions
RFC3339DateTimeFormat <- "%Y-%m-%dT%H:%M:%OSZ"

#' ParseRFC3339Timestamp
#'
#' The DataRobot APIs returns dates in RFC 3339 format.
#'
#' @param timestampstring character. Timestamp in RFC 3339 format.
#' @returns The input timestamp as a POSIXt
#' @family API datetime functions
#' @export
ParseRFC3339Timestamp <- function(timestampstring) {
  # in R < 4, as.POSIXct does not handle NULL correctly, so we have to handle it here
  if (is.null(timestampstring)) {
    empty <- list()
    class(empty) <- c("POSIXct", "POSIXt")
    return(empty)
  }
  return(as.POSIXct(timestampstring,
    format = RFC3339DateTimeFormat,
    tz = "UTC"
  ))
}

#' Type check
#'
#' This function is a backport of the [base::isa()] function in R 4.x to permit 3.x compatibility.
#'
#' @inheritParams base::isa
#' @md
isa <- function(x, what) {
  if (isS4(x)) {
    methods::is(x, what)
  } else {
    all(class(x) %in% what)
  }
}
