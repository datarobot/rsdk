# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
Deprecated <- function(message, deprecatedInVersion, removedInVersion) {
  .Deprecated(msg = sprintf(
    "%s has been deprecated in %s, will be removed in %s.",
    message, deprecatedInVersion, removedInVersion
  ))
}

DeprecatedByGeneratedCode <- function() {
  .Defunct(package = "datarobot",
           msg = "This function definition will be removed shortly; it is superceded by an API wrapper function with the same name. It will be removed shortly");
}
