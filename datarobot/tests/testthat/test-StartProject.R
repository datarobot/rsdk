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

# StartProject is a combination of SetupProject and SetTarget, which are tested individually,
# so let's just carefully test the integration once, with lots of mocking.

aimUrl <- datarobot:::UrlJoin(projectUrl, ProjectStage$AIM)
postResponse <- httr:::response(
  url = projectUrl,
  status_code = 202L,
  headers = list(location = statusUrl),
  content = raw(0)
)

test_that("It can StartProject", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$expects(url = statusUrl)
  getStub$onCall(1)$returns(httr:::response(
    url = statusUrl,
    status_code = 200L,
    content = charToRaw('{"status": "someStatus"}')
  ))
  getStub$onCall(2)$expects(url = statusUrl)
  getStub$onCall(2)$returns(httr:::response(
    url = statusUrl,
    status_code = 303L,
    headers = list(location = projectUrl),
    content = raw(0)
  ))
  getStub$onCall(3)$expects(url = projectUrl)
  getStub$onCall(3)$returns(httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = charToRaw(fakeProjectJson)
  ))

  getStub$onCall(4)$expects(url = statusUrl)
  getStub$onCall(4)$returns(httr:::response(
    url = statusUrl,
    status_code = 200L,
    content = charToRaw('{"status": "someStatus"}')
  ))

  getStub$onCall(5)$expects(url = statusUrl)
  getStub$onCall(5)$returns(httr:::response(
    url = statusUrl,
    status_code = 303L,
    headers = list(location = projectUrl),
    content = raw(0)
  ))

  getStub$onCall(6)$expects(url = projectUrl)
  getStub$onCall(6)$returns(httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = raw(0)
  ))

  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(postResponse)

  patchStub <- stub(httr::PATCH)
  patchStub$onCall(1)$expects(url = aimUrl)
  patchStub$onCall(1)$returns(httr:::response(
    url = aimUrl,
    status_code = 202L,
    headers = list(location = statusUrl),
    content = raw(0)
  ))

  result <- with_mock(
    "httr::PATCH" = patchStub$f,
    "httr::POST" = postStub$f,
    "httr::GET" = getStub$f,
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
    GetProjectStatus = function(...) list(stage = ProjectStage$AIM),
    StartProject(iris, "iris", target = "Species", targetType = "Multiclass")
  )
  expect_equal(getStub$calledTimes(), 6)
  expect_equal(postStub$calledTimes(), 1)
  expect_equal(patchStub$calledTimes(), 1)
  expect_equal(result$projectName, fakeProject$projectName)
  expect_equal(as.character(bodyForInspect$target), "Species")
  expect_equal(as.character(bodyForInspect$targetType), "Multiclass")
})

test_that("It can StartProject with workerCount", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$expects(url = statusUrl)
  getStub$onCall(1)$returns(httr:::response(
    url = statusUrl,
    status_code = 200L,
    content = charToRaw('{"status": "someStatus"}')
  ))
  getStub$onCall(2)$expects(url = statusUrl)
  getStub$onCall(2)$returns(httr:::response(
    url = statusUrl,
    status_code = 303L,
    headers = list(location = projectUrl),
    content = raw(0)
  ))
  getStub$onCall(3)$expects(url = projectUrl)
  getStub$onCall(3)$returns(httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = charToRaw(fakeProjectJson)
  ))

  getStub$onCall(4)$expects(url = statusUrl)
  getStub$onCall(4)$returns(httr:::response(
    url = statusUrl,
    status_code = 200L,
    content = charToRaw('{"status": "someStatus"}')
  ))

  getStub$onCall(5)$expects(url = statusUrl)
  getStub$onCall(5)$returns(httr:::response(
    url = statusUrl,
    status_code = 303L,
    headers = list(location = projectUrl),
    content = raw(0)
  ))

  getStub$onCall(6)$expects(url = projectUrl)
  getStub$onCall(6)$returns(httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = raw(0)
  ))

  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(postResponse)

  patchStub <- stub(httr::PATCH)
  patchStub$onCall(1)$expects(url = aimUrl)
  patchStub$onCall(1)$returns(httr:::response(
    url = aimUrl,
    status_code = 202L,
    headers = list(location = statusUrl),
    content = raw(0)
  ))
  patchStub$onCall(2)$returns(httr:::response(
    url = projectUrl,
    status_code = 202L,
    headers = list(location = projectUrl),
    content = raw(0)
  ))

  bodyForInspect <- list()
  result <- with_mock(
    "httr::PATCH" = patchStub$f,
    "httr::POST" = postStub$f,
    "httr::GET" = getStub$f,
    "datarobot:::DataRobotPATCH" = function(routeString,
                                            addUrl = TRUE,
                                            body = NULL,
                                            returnRawResponse = FALSE, ...) {
      bodyForInspect <<- append(bodyForInspect, body)
      datarobot:::MakeDataRobotRequest(httr::PATCH, routeString,
        addUrl = addUrl,
        returnRawResponse = returnRawResponse,
        body = body, ...
      )
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetProjectStatus = function(...) list(stage = ProjectStage$AIM),
    StartProject(iris, "iris",
      target = "Species",
      targetType = "Multiclass", workerCount = 10
    )
  )
  expect_equal(getStub$calledTimes(), 6)
  expect_equal(postStub$calledTimes(), 1)
  expect_equal(patchStub$calledTimes(), 2)
  expect_equal(result$projectName, fakeProject$projectName)
  expect_equal(as.character(bodyForInspect$target), "Species")
  expect_equal(as.character(bodyForInspect$targetType), "Multiclass")
  expect_equal(as.integer(bodyForInspect$workerCount), 10)
})


test_that("It can StartProject with max workerCount", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$expects(url = statusUrl)
  getStub$onCall(1)$returns(httr:::response(
    url = statusUrl,
    status_code = 200L,
    content = charToRaw('{"status": "someStatus"}')
  ))
  getStub$onCall(2)$expects(url = statusUrl)
  getStub$onCall(2)$returns(httr:::response(
    url = statusUrl,
    status_code = 303L,
    headers = list(location = projectUrl),
    content = raw(0)
  ))
  getStub$onCall(3)$expects(url = projectUrl)
  getStub$onCall(3)$returns(httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = charToRaw(fakeProjectJson)
  ))

  getStub$onCall(4)$expects(url = statusUrl)
  getStub$onCall(4)$returns(httr:::response(
    url = statusUrl,
    status_code = 200L,
    content = charToRaw('{"status": "someStatus"}')
  ))

  getStub$onCall(5)$expects(url = statusUrl)
  getStub$onCall(5)$returns(httr:::response(
    url = statusUrl,
    status_code = 303L,
    headers = list(location = projectUrl),
    content = raw(0)
  ))

  getStub$onCall(6)$expects(url = projectUrl)
  getStub$onCall(6)$returns(httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = raw(0)
  ))

  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(postResponse)

  patchStub <- stub(httr::PATCH)
  patchStub$onCall(1)$expects(url = aimUrl)
  patchStub$onCall(1)$returns(httr:::response(
    url = aimUrl,
    status_code = 202L,
    headers = list(location = statusUrl),
    content = raw(0)
  ))
  patchStub$onCall(2)$returns(httr:::response(
    url = projectUrl,
    status_code = 202L,
    headers = list(location = projectUrl),
    content = raw(0)
  ))

  bodyForInspect <- list()
  result <- with_mock(
    "httr::PATCH" = patchStub$f,
    "httr::POST" = postStub$f,
    "httr::GET" = getStub$f,
    "datarobot:::DataRobotPATCH" = function(routeString,
                                            addUrl = TRUE,
                                            body = NULL,
                                            returnRawResponse = FALSE, ...) {
      bodyForInspect <<- append(bodyForInspect, body)
      datarobot:::MakeDataRobotRequest(httr::PATCH, routeString,
        addUrl = addUrl,
        returnRawResponse = returnRawResponse,
        body = body, ...
      )
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetProjectStatus = function(...) list(stage = ProjectStage$AIM),
    StartProject(iris, "iris",
      target = "Species",
      targetType = "Multiclass", workerCount = "max"
    )
  )
  expect_equal(getStub$calledTimes(), 6)
  expect_equal(postStub$calledTimes(), 1)
  expect_equal(patchStub$calledTimes(), 2)
  expect_equal(result$projectName, fakeProject$projectName)
  expect_equal(as.character(bodyForInspect$target), "Species")
  expect_equal(as.character(bodyForInspect$targetType), "Multiclass")
  expect_equal(as.integer(bodyForInspect$workerCount), -1)
})


test_that("It can StartProject without a project name", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$expects(url = statusUrl)
  getStub$onCall(1)$returns(httr:::response(
    url = statusUrl,
    status_code = 200L,
    content = charToRaw('{"status": "someStatus"}')
  ))
  getStub$onCall(2)$expects(url = statusUrl)
  getStub$onCall(2)$returns(httr:::response(
    url = statusUrl,
    status_code = 303L,
    headers = list(location = projectUrl),
    content = raw(0)
  ))
  getStub$onCall(3)$expects(url = projectUrl)
  getStub$onCall(3)$returns(httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = charToRaw(fakeProjectJson)
  ))

  getStub$onCall(4)$expects(url = statusUrl)
  getStub$onCall(4)$returns(httr:::response(
    url = statusUrl,
    status_code = 200L,
    content = charToRaw('{"status": "someStatus"}')
  ))

  getStub$onCall(5)$expects(url = statusUrl)
  getStub$onCall(5)$returns(httr:::response(
    url = statusUrl,
    status_code = 303L,
    headers = list(location = projectUrl),
    content = raw(0)
  ))

  getStub$onCall(6)$expects(url = projectUrl)
  getStub$onCall(6)$returns(httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = raw(0)
  ))

  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(postResponse)

  patchStub <- stub(httr::PATCH)
  patchStub$onCall(1)$expects(url = aimUrl)
  patchStub$onCall(1)$returns(httr:::response(
    url = aimUrl,
    status_code = 202L,
    headers = list(location = statusUrl),
    content = raw(0)
  ))

  result <- with_mock(
    "httr::PATCH" = patchStub$f,
    "httr::POST" = postStub$f,
    "httr::GET" = getStub$f,
    "datarobot:::DataRobotPOST" = function(routeString,
                                           addUrl = TRUE,
                                           body = NULL,
                                           returnRawResponse = FALSE, ...) {
      postForInspect <<- body
      datarobot:::MakeDataRobotRequest(httr::POST, routeString,
        addUrl = addUrl,
        returnRawResponse = returnRawResponse,
        body = body, ...
      )
    },
    "datarobot:::DataRobotPATCH" = function(routeString,
                                            addUrl = TRUE,
                                            body = NULL,
                                            returnRawResponse = FALSE, ...) {
      patchForInspect <<- body
      datarobot:::MakeDataRobotRequest(httr::PATCH, routeString,
        addUrl = addUrl,
        returnRawResponse = returnRawResponse,
        body = body, ...
      )
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetProjectStatus = function(...) list(stage = ProjectStage$AIM),
    StartProject(iris, target = "Species", targetType = "Multiclass")
  )
  expect_equal(getStub$calledTimes(), 6)
  expect_equal(postStub$calledTimes(), 1)
  expect_equal(patchStub$calledTimes(), 1)
  expect_equal(as.character(postForInspect$projectName), "iris")
  expect_true(grepl("autoSavedDF", capture.output(postForInspect$file)))
  expect_equal(as.character(patchForInspect$target), "Species")
  expect_equal(as.character(patchForInspect$targetType), "Multiclass")
})

test_that("It can StartProject and wait", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$expects(url = statusUrl)
  getStub$onCall(1)$returns(httr:::response(
    url = statusUrl,
    status_code = 200L,
    content = charToRaw('{"status": "someStatus"}')
  ))
  getStub$onCall(2)$expects(url = statusUrl)
  getStub$onCall(2)$returns(httr:::response(
    url = statusUrl,
    status_code = 303L,
    headers = list(location = projectUrl),
    content = raw(0)
  ))
  getStub$onCall(3)$expects(url = projectUrl)
  getStub$onCall(3)$returns(httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = charToRaw(fakeProjectJson)
  ))

  getStub$onCall(4)$expects(url = statusUrl)
  getStub$onCall(4)$returns(httr:::response(
    url = statusUrl,
    status_code = 200L,
    content = charToRaw('{"status": "someStatus"}')
  ))

  getStub$onCall(5)$expects(url = statusUrl)
  getStub$onCall(5)$returns(httr:::response(
    url = statusUrl,
    status_code = 303L,
    headers = list(location = projectUrl),
    content = raw(0)
  ))

  getStub$onCall(6)$expects(url = projectUrl)
  getStub$onCall(6)$returns(httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = raw(0)
  ))

  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(postResponse)

  patchStub <- stub(httr::PATCH)
  patchStub$onCall(1)$expects(url = aimUrl)
  patchStub$onCall(1)$returns(httr:::response(
    url = aimUrl,
    status_code = 202L,
    headers = list(location = statusUrl),
    content = raw(0)
  ))

  callCount <- 0
  aimStatus <- list(stage = ProjectStage$AIM)
  notDoneStatus <- list(autopilotDone = FALSE)
  isDoneStatus <- list(autopilotDone = TRUE)
  returnValues <- list(aimStatus, notDoneStatus, notDoneStatus, isDoneStatus)
  MockGetProjectStatus <- function(project) {
    callCount <<- callCount + 1
    returnValues[[callCount]]
  }
  result <- with_mock(
    "httr::PATCH" = patchStub$f,
    "httr::POST" = postStub$f,
    "httr::GET" = getStub$f,
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
    GetProjectStatus = MockGetProjectStatus,
    StartProject(iris, "iris",
      target = "Species",
      targetType = "Multiclass",
      wait = TRUE,
      verbosity = 0
    )
  )
  expect_equal(getStub$calledTimes(), 6)
  expect_equal(postStub$calledTimes(), 1)
  expect_equal(patchStub$calledTimes(), 1)
  expect_equal(callCount, 4)
  expect_equal(result$projectName, fakeProject$projectName)
  expect_equal(as.character(bodyForInspect$target), "Species")
  expect_equal(as.character(bodyForInspect$targetType), "Multiclass")
})


test_that("It can StartProject from a data source", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$expects(url = statusUrl)
  getStub$onCall(1)$returns(httr:::response(
    url = statusUrl,
    status_code = 200L,
    content = charToRaw('{"status": "someStatus"}')
  ))
  getStub$onCall(2)$expects(url = statusUrl)
  getStub$onCall(2)$returns(httr:::response(
    url = statusUrl,
    status_code = 303L,
    headers = list(location = projectUrl),
    content = raw(0)
  ))
  getStub$onCall(3)$expects(url = projectUrl)
  getStub$onCall(3)$returns(httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = charToRaw(fakeProjectJson)
  ))

  getStub$onCall(4)$expects(url = statusUrl)
  getStub$onCall(4)$returns(httr:::response(
    url = statusUrl,
    status_code = 200L,
    content = charToRaw('{"status": "someStatus"}')
  ))

  getStub$onCall(5)$expects(url = statusUrl)
  getStub$onCall(5)$returns(httr:::response(
    url = statusUrl,
    status_code = 303L,
    headers = list(location = projectUrl),
    content = raw(0)
  ))

  getStub$onCall(6)$expects(url = projectUrl)
  getStub$onCall(6)$returns(httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = raw(0)
  ))

  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(postResponse)

  patchStub <- stub(httr::PATCH)
  patchStub$onCall(1)$expects(url = aimUrl)
  patchStub$onCall(1)$returns(httr:::response(
    url = aimUrl,
    status_code = 202L,
    headers = list(location = statusUrl),
    content = raw(0)
  ))

  result <- with_mock(
    "httr::PATCH" = patchStub$f,
    "httr::POST" = postStub$f,
    "httr::GET" = getStub$f,
    "datarobot:::DataRobotPOST" = function(routeString,
                                           addUrl = TRUE,
                                           body = NULL,
                                           returnRawResponse = FALSE, ...) {
      postForInspect <<- body
      datarobot:::MakeDataRobotRequest(httr::POST, routeString,
        addUrl = addUrl,
        returnRawResponse = returnRawResponse,
        body = body, ...
      )
    },
    "datarobot:::DataRobotPATCH" = function(routeString,
                                            addUrl = TRUE,
                                            body = NULL,
                                            returnRawResponse = FALSE, ...) {
      patchForInspect <<- body
      datarobot:::MakeDataRobotRequest(httr::PATCH, routeString,
        addUrl = addUrl,
        returnRawResponse = returnRawResponse,
        body = body, ...
      )
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetProjectStatus = function(...) list(stage = ProjectStage$AIM),
    StartProject(fakeDataSource,
      username = fakeUsername,
      password = fakePassword,
      target = "target"
    )
  )
  expect_equal(getStub$calledTimes(), 6)
  expect_equal(postStub$calledTimes(), 1)
  expect_equal(patchStub$calledTimes(), 1)
  expect_equal(as.character(postForInspect$dataSourceId), fakeDataSourceId)
  expect_equal(as.character(postForInspect$projectName), "fakeDataSource")
  expect_equal(as.character(postForInspect$user), fakeUsername)
  expect_equal(as.character(postForInspect$password), fakePassword)
  expect_equal(as.character(patchForInspect$target), "target")
})

test_that("It can StartProject and wait", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$expects(url = statusUrl)
  getStub$onCall(1)$returns(httr:::response(
    url = statusUrl,
    status_code = 200L,
    content = charToRaw('{"status": "someStatus"}')
  ))
  getStub$onCall(2)$expects(url = statusUrl)
  getStub$onCall(2)$returns(httr:::response(
    url = statusUrl,
    status_code = 303L,
    headers = list(location = projectUrl),
    content = raw(0)
  ))
  getStub$onCall(3)$expects(url = projectUrl)
  getStub$onCall(3)$returns(httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = charToRaw(fakeProjectJson)
  ))

  getStub$onCall(4)$expects(url = statusUrl)
  getStub$onCall(4)$returns(httr:::response(
    url = statusUrl,
    status_code = 200L,
    content = charToRaw('{"status": "someStatus"}')
  ))

  getStub$onCall(5)$expects(url = statusUrl)
  getStub$onCall(5)$returns(httr:::response(
    url = statusUrl,
    status_code = 303L,
    headers = list(location = projectUrl),
    content = raw(0)
  ))

  getStub$onCall(6)$expects(url = projectUrl)
  getStub$onCall(6)$returns(httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = raw(0)
  ))

  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(postResponse)

  patchStub <- stub(httr::PATCH)
  patchStub$onCall(1)$expects(url = aimUrl)
  patchStub$onCall(1)$returns(httr:::response(
    url = aimUrl,
    status_code = 202L,
    headers = list(location = statusUrl),
    content = raw(0)
  ))

  callCount <- 0
  aimStatus <- list(stage = ProjectStage$AIM)
  notDoneStatus <- list(autopilotDone = FALSE)
  isDoneStatus <- list(autopilotDone = TRUE)
  returnValues <- list(aimStatus, notDoneStatus, notDoneStatus, isDoneStatus)
  MockGetProjectStatus <- function(project) {
    callCount <<- callCount + 1
    returnValues[[callCount]]
  }
  result <- with_mock(
    "httr::PATCH" = patchStub$f,
    "httr::POST" = postStub$f,
    "httr::GET" = getStub$f,
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
    GetProjectStatus = MockGetProjectStatus,
    StartProject(iris, "iris",
      target = "Species",
      targetType = "Multiclass",
      wait = TRUE,
      verbosity = 0
    )
  )
  expect_equal(getStub$calledTimes(), 6)
  expect_equal(postStub$calledTimes(), 1)
  expect_equal(patchStub$calledTimes(), 1)
  expect_equal(callCount, 4)
  expect_equal(result$projectName, fakeProject$projectName)
  expect_equal(as.character(bodyForInspect$target), "Species")
  expect_equal(as.character(bodyForInspect$targetType), "Multiclass")
})
