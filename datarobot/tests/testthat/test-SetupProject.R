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

postResponse <- httr:::response(
  url = projectUrl,
  status_code = 202L,
  headers = list(location = statusUrl),
  content = raw(0)
)

MakeGetStub <- function() {
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
  getStub
}

withSetupProjectMocks <- function(..., projectsUrl = rootProjectsUrl) {
  # Set up stubs for POST and GET. We expect POST is called once (to request creating the project),
  # and GET is called three times:
  #  (1) get status of the async request (not done yet)
  #  (2) get status of the async request (now we're done)
  #  (3) get project data

  postStub <- stub(httr::POST)
  postStub$onCall(1)$expects(url = projectsUrl)
  postStub$onCall(1)$returns(postResponse)

  getStub <- MakeGetStub()
  result <- with_mock(
    "httr::POST" = postStub$f,
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ...
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_equal(getStub$calledTimes(), 3)
  result
}


test_that("Specify projectName", {
  testReturn <- withSetupProjectMocks(
    SetupProject(iris, fakeProject$projectName)
  )
  expect_equal(fakeProject, testReturn)
})

test_that("No fakeProject$projectName", {
  testReturn <- withSetupProjectMocks(
    SetupProject(iris)
  )
  expect_equal(fakeProject, testReturn)
})


irisCustomDF <- structure(iris, class = c("customDFClass", "data.frame"))
test_that("No warning with DF subclass", {
  withSetupProjectMocks(
    expect_warning(SetupProject(irisCustomDF, fakeProject$projectName), regexp = NA)
  )
})

test_that("Call succeeds with DF subclass)", {
  testReturn <- withSetupProjectMocks(
    SetupProject(irisCustomDF, fakeProject$projectName)
  )
  expect_equal(fakeProject, testReturn)
})

test_that("Specify valid csv file name", {
  testReturn <- withSetupProjectMocks(SetupProject(
    "Boston_autoSavedDF.csv",
    fakeProject$projectName
  ))
  expect_equal(fakeProject, testReturn)
})

test_that("SetupProject fails with blank iris value", {
  expect_error(withSetupProjectMocks(SetupProject("")), "No file named")
})

test_that("SetupProject fails with invalid iris type", {
  expect_error(withSetupProjectMocks(SetupProject(TRUE)), "not a valid data")
})

test_that("Read timeout default", {
  postStubFunction <- function(...) {
    # Make sure the timeout option is specified:
    expect_true(any(unlist(Map(function(x) identical(x, httr::timeout(60 * 60)), list(...)))))
    postResponse
  }
  getStub <- MakeGetStub()
  result <- with_mock(
    "httr::POST" = postStubFunction,
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    SetupProject("Boston_autoSavedDF.csv", fakeProject$projectName)
  )
})

test_that("Specify fakeProject$projectName", {
  testReturn <- withSetupProjectMocks(
    SetupProject(iris, fakeProject$projectName)
  )
  expect_equal(fakeProject, testReturn)
})

test_that("SetupProject fails with blank iris value", {
  expect_error(withSetupProjectMocks(SetupProject("")), "No file named")
})


server <- "dummyServer"
database <- "dummyDatabase"
table <- "dummyTable"
user <- "dummyUser"
password <- "dummyPassword"
encryptedPassword <- "dummyEncryptedPassword"
url <- "dummyURL"

test_that("isURL http true", {
  expect_true(isURL("http://path_to_urlsource"))
})

test_that("isURL file true", {
  expect_true(isURL("file://path_to_urlsource"))
})

test_that("isURL http false", {
  expect_false(isURL("not_a_path_to_urlsource"))
})

test_that("isURL file false", {
  expect_false(isURL("filename"))
})


test_that("Specify parameters", {
  testReturn <- expect_warning(withSetupProjectMocks(
    SetupProjectFromHDFS(url),
    projectsUrl = datarobot:::UrlJoin(fakeEndpoint, "hdfsProjects")
  ), "deprecated")
  expect_equal(fakeProject, testReturn)
})

test_that("SetupProject fails with incomplete parameters", {
  expect_error(
    withSetupProjectMocks(
      suppressWarnings(
        SetupProjectFromHDFS()
      ),
      projectsUrl = datarobot:::UrlJoin(fakeEndpoint, "hdfsProjects")
    )
  )
})

test_that("SetupProject fails if both password and encryptedPassword are specified", {
  expect_error(
    withSetupProjectMocks(
      suppressWarnings(
        SetupProjectFromHDFS(server, database, table, user,
          password = password, encryptedPassword = encryptedPassword
        ),
        projectsUrl = datarobot:::UrlJoin(fakeEndpoint, "hdfsProjects")
      )
    )
  )
})


test_that("Specify parameters", {
  testReturn <- withSetupProjectMocks(
    SetupProjectFromDataSource(fakeDataSourceId, fakeUsername, fakePassword)
  )
  expect_equal(fakeProject, testReturn)
})

test_that("Specify parameters with data source", {
  testReturn <- withSetupProjectMocks(
    SetupProjectFromDataSource(fakeDataSource, fakeUsername, fakePassword)
  )
  expect_equal(fakeProject, testReturn)
})


test_that("Specify valid url", {
  testReturn <- withSetupProjectMocks(SetupProject(
    "http://Boston_autoSavedDF.csv",
    fakeProject$projectName
  ))
  expect_equal(fakeProject, testReturn)
})


test_that("ProjectFromJobResponse works with a URL", {
  getProjectJson <- fileToChar("responses/GetProject.json")
  getProjectResponse <- httr:::response(
    url = projectUrl,
    status_code = 200L,
    content = charToRaw(getProjectJson)
  )
  testReturn <- with_mock(
    "httr::POST" = function(...) {
      stop("Should not be called!")
    },
    "datarobot::WaitForAsyncReturn" = function(...) {
      ParseReturnResponse(getProjectResponse)
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ProjectFromJobResponse(postResponse)
  )
  expect_type(testReturn, "list")
  ExpectHasKeys(testReturn, c("projectName", "projectId", "fileName", "created"))
  expect_type(testReturn$projectName, "character")
  expect_true(IsId(testReturn$projectId))
  expect_type(testReturn$fileName, "character")
  expect_type(testReturn$created, "character")
})
