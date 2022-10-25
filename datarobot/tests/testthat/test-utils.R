# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
library(testthat)

describe("%notin%", {
  it("should be the complement of %in%", {
    testSeq <- seq(1:5)
    testSubseq <- seq(1:4)
    expect_equal(testSubseq %notin% testSeq, !testSubseq %in% testSeq)
    expect_equal(0 %notin% testSeq, !0 %in% testSeq)
  })
})
