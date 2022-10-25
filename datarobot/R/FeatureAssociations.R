# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
as.DataRobotFeatureAssociationMatrixDetails <- function(inList) {
  outList <- inList
  outList$values <- as.data.frame(outList$values)
  colnames(outList$values) <- c("feature1", "feature2", "relativeFreq")
  outList
}
