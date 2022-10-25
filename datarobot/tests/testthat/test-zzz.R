# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
library(testthat)

describe("The dr list", {
  it("should not contain the DatetimePartitioningDataForOpenApi object", {
    # DSX-1991
    expect_false("DatetimePartitioningDataForOpenApi" %in% names(dr))
  })

  it("should contain the InfrastructureApi object", {
    # DSX-1991
    expect_true("InfrastructureApi" %in% names(dr))
  })
})
