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

requestResponse <- httr:::response(
  url = modelUrl,
  status_code = 202L,
  headers = list(location = modelUrl),
  content = raw(0)
)

test_that("SetPredictionThreshold succeeds", {
  patchStub <- stub(httr::PATCH)
  patchStub$onCall(1)$returns(requestResponse)
  with_mock(
    "httr::PATCH" = patchStub$f,
    "datarobot:::DataRobotPATCH" = function(routeString,
                                            addUrl = TRUE,
                                            body = NULL,
                                            returnRawResponse = FALSE, ...) {
      bodyForInspect <<- body
      datarobot:::MakeDataRobotRequest(httr::PATCH, routeString,
        addUrl = addUrl,
        returnRawResponse = returnRawResponse,
        body = body, ...
      )
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    SetPredictionThreshold(fakeModel, 0.6)
  )
  expect_equal(patchStub$calledTimes(), 1)
  expect_equal(bodyForInspect$predictionThreshold, 0.6)
})
