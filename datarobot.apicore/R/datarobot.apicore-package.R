# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.

#' @section Package options:
#'
#' The `datarobot.apicore` package uses the following [options()] to configure behavior:
#'
#' \itemize{
#' \item `datarobot.apicore.returnS3`: logical. If `TRUE`, API responses will be returned to calling functions as S3 lists instead of R6 objects. See the helper methods [.ReturnResponse()] and [.toS3()] for more information. Default is `TRUE`, which we recommend for consistency if you have been using the datarobot client prior to 2.27.0.
#'
#' }
#' @docType package
#' @keywords internal
#' @name datarobot.apicore
#' @md
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
