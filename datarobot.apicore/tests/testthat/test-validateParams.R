# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
library(testthat)

describe("validateParams", {
  test_that("validateParams works if validateParams = FALSE", {
    expect_error(Comment$new(content = "ParakeetJones", validateParams = FALSE), NA)
    expect_error(Comment$new(content = NULL, validateParams = FALSE), NA)
  })
  test_that("validateParams fails correctly if validateParams = TRUE", {
    expect_error(Comment$new(content = "ParakeetJones", entityId = "parakeetId", entityType = "parakeet", validateParams = TRUE), NA)
    expect_error(Comment$new(content = "ParakeetJones", entityId = "parakeetId", entityType = "parakeet", mentions = list(1, 2), validateParams = TRUE), "sapply(mentions, is.character) are not all TRUE", fixed = TRUE)
    expect_error(Comment$new(entityId = "parakeetId", entityType = "parakeet", validateParams = TRUE), "Required param not set.", fixed = TRUE)
    expect_error(Comment$new(content = 13, entityId = "parakeetId", entityType = "parakeet", validateParams = TRUE), "is.character(content) is not TRUE", fixed = TRUE)
    expect_error(Comment$new(content = c("bad", "data"), entityId = "parakeetId", entityType = "parakeet", validateParams = TRUE), "length(content) == 1 is not TRUE", fixed = TRUE)
  })
  test_that("validateParams fails correctly if validateParams = TRUE on fromJSON", {
    expect_error(Comment$new()$fromJSON("{\"content\":\"ParakeetJones\",\"entityId\":\"parakeetId\",\"entityType\":\"parakeet\"}", validateParams = TRUE), NA)
    expect_error(Comment$new()$fromJSON("{\"entityId\":\"parakeetId\",\"entityType\":\"parakeet\"}", validateParams = TRUE), NA)
    expect_error(Comment$new()$fromJSON("{\"content\":\"ParakeetJones\",\"entityId\":\"parakeetId\",\"entityType\":\"parakeet\",\"mentions\":[1, 2]}", validateParams = TRUE), "sapply(mentions, is.character) are not all TRUE", fixed = TRUE)
    expect_error(Comment$new()$fromJSON("{\"content\":13,\"entityId\":\"parakeetId\",\"entityType\":\"parakeet\"}", validateParams = TRUE), "is.character(content) is not TRUE", fixed = TRUE)
  })
  test_that("validateParams handles empty lists correctly", {
    expect_error(Comment$new(content = "ParakeetJones", entityId = "parakeetId", entityType = "parakeet", mentions = list(), validateParams = TRUE), NA)
    expect_error(Comment$new(content = "ParakeetJones", entityId = "parakeetId", entityType = "parakeet", mentions = c(), validateParams = TRUE), NA)
    expect_error(Comment$new()$fromJSON("{\"content\":\"ParakeetJones\",\"entityId\":\"parakeetId\",\"entityType\":\"parakeet\",\"mentions\":[]}", validateParams = TRUE), NA)
  })
})
