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

rulesetsUrl <- UrlJoin(projectUrl, "models", fakeModelId, "primeRulesets")
rulesetsJson <- fileToChar("responses/primeRulesets.json")
completedRulesetsResponse <- httr:::response(
  url = rulesetsUrl,
  status_code = 200L,
  content = charToRaw(rulesetsJson)
)

test_that("GetRulesets succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(completedRulesetsResponse)
  primeRulesets <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetRulesets(fakeProject, fakeModelId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_type(primeRulesets, "list")
  expect_type(primeRulesets[[1]]$projectId, "character")
  expect_type(primeRulesets[[1]]$ruleCount, "integer")
  expect_type(primeRulesets[[1]]$parentModelId, "character")
  expect_type(primeRulesets[[1]]$projectId, "character")
  expect_type(primeRulesets[[1]]$score, "double")
})
