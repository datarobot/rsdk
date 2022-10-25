# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.

# Process the missing values report
#
# Input is:
# list(list(missingCount = <missingCount>,
#           tasks = list(<taskId> = list(descriptions = list(<descriptions>),
#                                        name = <name>),
#           type = <type>,
#           feature = <featureName>,
#           missingPercentage = <missingPercentage>), ...)
# Output is:
# list(<featureName> = list(type = <featureType>,
#                           missingCount = <missingCount>,
#                           missingPercentage = <missingPercentage>,
#                           tasks = list(list(name = <Task1Name>,
#                                             descriptions = c(<Task1Description>, ...),
#                                             id = <taskId>), ...)), ...)
as.dataRobotMissingValuesReport <- function(inList) {
  features <- lapply(inList, `[[`, "feature")
  for (i in seq_along(inList)) {
    inList[[i]]$feature <- NULL # Drop feature from within list
    for (j in seq_along(inList[[i]]$tasks)) {
      inList[[i]]$tasks[[j]]$id <- names(inList[[i]]$tasks)[[j]]
      inList[[i]]$tasks[[j]]$name <- as.character(inList[[i]]$tasks[[j]]$name)
      inList[[i]]$tasks[[j]]$descriptions <- as.character(inList[[i]]$tasks[[j]]$descriptions)
    }
    inList[[i]]$tasks <- unname(inList[[i]]$tasks)
  }
  stats::setNames(inList, features)
}
