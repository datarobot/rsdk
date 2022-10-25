# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.

as.dataRobotDrivers <- function(elements) {
  elements[, c("id", "canonicalName", "className", "baseNames", "creator")]
}


as.dataRobotDriver <- function(inList) {
  outList <- inList
  class(outList) <- "dataRobotDriver"
  outList
}
