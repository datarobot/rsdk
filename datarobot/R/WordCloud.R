# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.

as.dataRobotWordCloud <- function(indf) {
  # clean up DF, then sort
  outdf <- reorderColumns(indf, c("ngram" = 1, "frequency" = 2, "coefficient" = 3))
  outdf[with(outdf, order(outdf$frequency, decreasing = TRUE)), ]
}
