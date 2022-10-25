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


test_that("Required parameters are present", {
  expect_error(UnpauseQueue())
})

pauseUrl <- UrlJoin(projectUrl, "autopilot")
pauseResponse <- httr:::response(
  url = pauseUrl,
  status_code = 202L,
  content = raw(0)
)
test_that("It can pause the queue", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(pauseResponse)
  testReturn <- suppressMessages(with_mock(
    "httr::POST" = postStub$f,
    "datarobot:::DataRobotPOST" = function(routeString,
                                           addUrl = TRUE,
                                           body = NULL,
                                           returnRawResponse = FALSE, ...) {
      bodyForInspect <<- body
      datarobot:::MakeDataRobotRequest(httr::POST, routeString,
        addUrl = addUrl,
        returnRawResponse = returnRawResponse,
        body = body, ...
      )
    },
    "httr::GET" = function(...) stop("Should not be called!"),
    expect_message(UnpauseQueue(fakeProjectId), "unpaused")
  ))
  expect_equal(postStub$calledTimes(), 1)
  expect_equal(as.character(bodyForInspect$command), "start")
})
