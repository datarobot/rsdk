# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
library(stubthat)
library(testthat)

describe("GetWordCloud", {
  wordCloudUrl <- UrlJoin(projectUrl, "models", fakeModelId, "wordCloud")
  wordCloudJson <- fileToChar("responses/wordCloud.json")
  completedWordCloudResponse <- httr:::response(
    url = wordCloudUrl,
    status_code = 200L,
    content = charToRaw(wordCloudJson)
  )

  test_that("GetWordCloud succeeds", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(completedWordCloudResponse)
    wordCloud <- with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetWordCloud(fakeProject, fakeModelId)
    )
    expect_equal(getStub$calledTimes(), 1)
    expect_s3_class(wordCloud, "data.frame")
    expect_type(wordCloud$coefficient, "double")
    expect_type(wordCloud$count, "integer")
    expect_type(wordCloud$ngram, "character")
    expect_type(wordCloud$frequency, "double")
    expect_type(wordCloud$isStopword, "logical")
    if (hasName(wordCloud, "variable")) {
      expect_type(wordCloud$variable, "character")
    }
    if (hasName(wordCloud, "class")) {
      expect_type(wordCloud$class, "character")
    }

    expect_equal(names(wordCloud)[1:3], c("ngram", "frequency", "coefficient"))
  })
})
