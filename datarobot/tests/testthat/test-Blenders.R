# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
library(testthat)
library(stubthat)


modelIds <- c("dummyModelId1", "dummyModelId2")

test_that("Required parameters are present", {
  expect_error(RequestBlend())
  expect_error(RequestBlend(projectId))
  expect_error(RequestBlend(modelsToBlend = modelIds))
})


requestResponse <- httr:::response(
  url = modelUrl,
  status_code = 202L,
  headers = list(location = modelUrl),
  content = raw(0)
)
test_that("RequestBlender succeeds", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(requestResponse)
  jobId <- with_mock(
    "httr::POST" = postStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    {
      expect_message(
        {
          jobId <- RequestBlender(fakeProjectId,
            modelIds,
            blendMethod = BlendMethods$PLS
          )
        },
        "New blender request received"
      )
      jobId
    }
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_type(jobId, "character")
})

blenderTrueJson <- fileToChar("responses/is_blender_eligible_true.json")
blenderFalseJson <- fileToChar("responses/is_blender_eligible_false.json")

test_that("IsBlenderEligible true", {
  requestResponse <- httr:::response(
    url = modelUrl,
    status_code = 200L,
    content = charToRaw(blenderTrueJson)
  )
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(requestResponse)
  blendData <- with_mock(
    "httr::POST" = postStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    IsBlenderEligible(fakeProjectId,
      modelIds,
      blendMethod = BlendMethods$PLS
    )
  )
  expect_equal(postStub$calledTimes(), 1)
  ExpectHasKeys(blendData, c("reason", "blendable"), allowAdditional = FALSE)
  expect_equal(blendData$reason, "")
  expect_true(blendData$blendable)
})

test_that("IsBlenderEligible false", {
  requestResponse <- httr:::response(
    url = modelUrl,
    status_code = 200L,
    content = charToRaw(blenderFalseJson)
  )
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(requestResponse)
  blendData <- with_mock(
    "httr::POST" = postStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    IsBlenderEligible(fakeProjectId,
      modelIds,
      blendMethod = BlendMethods$PLS
    )
  )
  expect_equal(postStub$calledTimes(), 1)
  ExpectHasKeys(blendData, c("reason", "blendable"), allowAdditional = FALSE)
  expect_false(blendData$blendable)
  expect_true(grepl("cannot be blended", blendData$reason), blendData$reason)
})
