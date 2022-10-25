# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.

PredictJobRoute <- function(projectId, predictJobId) {
  return(UrlJoin("projects", projectId, "predictJobs", predictJobId))
}

as.dataRobotPredictJobStatus <- function(inList) {
  outList <- inList
  idIndex <- which(names(outList) == "id")
  names(outList)[idIndex] <- "predictJobId"
  return(outList)
}
