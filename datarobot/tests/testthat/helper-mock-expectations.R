# Copyright 2023 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.

skip_if_too_new_installed <- function(pkg, maximum_version = NULL) {
  # Similar to testthat::skip_if_not_installed, but checks
  # for maximum package version
  if (!requireNamespace(pkg, quietly = TRUE)) {
    skip(paste0(pkg, " cannot be loaded"))
  }

  if (!is.null(maximum_version)) {
    installed_version <- utils::packageVersion(pkg)
    if (installed_version > maximum_version) {
      skip(paste0(
        "Installed ", pkg, " is version ", installed_version, "; ",
        "but ", maximum_version, " or older is required"
      ))
    }
  }

  return(invisible(TRUE))
}
