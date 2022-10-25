# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
#' @export
#'
"[.listSubclass" <- function(x, i, ...) {
  structure(NextMethod("["), class = class(x))
}
