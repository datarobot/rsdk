# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
library(stubthat)

originalURL <- Sys.getenv("DATAROBOT_API_ENDPOINT")
originalToken <- Sys.getenv("DATAROBOT_API_TOKEN")
originalSSLPref <- Sys.getenv("DataRobot_SSL_Verify")

testURL <- "https://endpoint/api/v2"
testURL2 <- gsub("https", "http", testURL, fixed = TRUE)
serverVersion <- list(
  major = packageVersion("datarobot")$major,
  versionString = "dummy_version_string",
  minor = packageVersion("datarobot")$minor
)

versionResponse <- httr:::response(
  url = testURL,
  status_code = 200L,
  content = charToRaw(jsonlite::toJSON(serverVersion))
)
projectResponse <- httr:::response(
  url = UrlJoin(testURL, "projects"),
  status_code = 200L,
  content = charToRaw(fileToChar("responses/ListProjects.json"))
)


test_that("We error when required arguments aren't given", {
  Sys.unsetenv("DATAROBOT_API_TOKEN")
  Sys.unsetenv("DATAROBOT_API_ENDPOINT")
  expect_error(ConnectToDataRobot())
  expect_error(ConnectToDataRobot(endpoint = testURL))
  expect_error(ConnectToDataRobot(token = fakeToken))
  expect_error(ConnectToDataRobot(endpoint = testURL, username = fakeUsername))
  expect_error(ConnectToDataRobot(endpoint = testURL, password = fakePassword))
  expect_error(ConnectToDataRobot(
    endpoint = testURL, username = dummUser,
    password = fakePassword
  )) # username/password no longer supported
})


test_that("We error when redundant arguments are given", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(projectResponse)
  getStub$onCall(2)$returns(versionResponse)
  with_mock(
    "httr::GET" = getStub$f,
    expect_error(ConnectToDataRobot(testURL,
      username = fakeUsername, password = fakePassword,
      token = fakeToken
    )),
    expect_error(ConnectToDataRobot(testURL,
      configPath = configPath,
      token = fakeToken
    ))
  )
})


test_that("ConnectWithFile works with appropriate config file", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(projectResponse)
  getStub$onCall(2)$returns(versionResponse)
  testReturn <- suppressMessages(with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() testURL,
    "datarobot:::Token" = function() fakeToken,
    ConnectToDataRobot(configPath = "drconfig.yaml")
  ))
  expect_equal(testReturn, TRUE)
  expect_equal(Sys.getenv("DATAROBOT_API_ENDPOINT"), testURL)
  expect_equal(Sys.getenv("DATAROBOT_API_TOKEN"), "DummyToken")
})


test_that("With non-null Token", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(projectResponse)
  getStub$onCall(2)$returns(versionResponse)
  testReturn <- suppressMessages(with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() testURL,
    "datarobot:::Token" = function() fakeToken,
    ConnectToDataRobot(
      endpoint = testURL,
      token = fakeToken
    )
  ))
  expect_equal(testReturn, TRUE)
  expect_equal(Sys.getenv("DATAROBOT_API_ENDPOINT"), testURL)
  expect_equal(Sys.getenv("DATAROBOT_API_TOKEN"), fakeToken)
})


test_that("With env vars", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(projectResponse)
  getStub$onCall(2)$returns(versionResponse)
  Sys.setenv(DataRobot_endpoint = testURL)
  Sys.setenv(DataRobot_Token = fakeToken)
  testReturn <- suppressMessages(with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() testURL,
    "datarobot:::Token" = function() fakeToken,
    ConnectToDataRobot()
  ))
  expect_equal(testReturn, TRUE)
})


test_that("GET fails as expected with status_code > 400", {
  with_mock(
    "httr::GET" = function(url, ...) {
      httr:::response(
        url = testURL,
        status_code = 404L,
        content = raw(0)
      )
    },
    expect_error(
      ConnectToDataRobot(endpoint = testURL, token = fakeToken),
      "Authorization request denied."
    )
  )
})


test_that("ConnectWithFile expect error on wrong version", {
  serverVersion <- list(
    major = packageVersion("datarobot")$major + 1,
    versionString = "dummy_version_string",
    minor = packageVersion("datarobot")$minor
  )
  versionResponse <- httr:::response(
    url = testURL,
    status_code = 303L,
    content = charToRaw(jsonlite::toJSON(serverVersion))
  )
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(projectResponse)
  getStub$onCall(2)$returns(versionResponse)
  testReturn <- suppressMessages(with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() testURL,
    "datarobot:::Token" = function() fakeToken,
    expect_error(ConnectToDataRobot(configPath = "drconfig.yaml"))
  ))
})


test_that("ConnectWithFile expect warning on wrong subversion", {
  serverVersion <- list(
    major = packageVersion("datarobot")$major,
    versionString = "dummy_version_string",
    minor = packageVersion("datarobot")$minor - 1
  )
  versionResponse <- httr:::response(
    url = testURL,
    status_code = 303L,
    content = charToRaw(jsonlite::toJSON(serverVersion))
  )
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(projectResponse)
  getStub$onCall(2)$returns(versionResponse)
  testReturn <- suppressMessages(with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() testURL,
    "datarobot:::Token" = function() fakeToken,
    expect_warning(ConnectToDataRobot(configPath = "drconfig.yaml"))
  ))
})


test_that("ConnectWithToken expect error with wrong url", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(projectResponse)
  getStub$onCall(2)$returns(versionResponse)
  expectedErrorMsg <- paste("Specified endpoint ", testURL2, " is not correct.",
    "\nWas redirected to ", testURL,
    sep = ""
  )
  testReturn <- suppressMessages(with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() testURL,
    "datarobot:::Token" = function() fakeToken,
    expect_error(ConnectToDataRobot(
      endpoint = testURL2,
      token = fakeToken
    ),
    expectedErrorMsg,
    fixed = TRUE
    )
  ))
})


test_that("ConnectWithToken can set ssl verification", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(projectResponse)
  getStub$onCall(2)$returns(versionResponse)
  expect_equal(Sys.getenv("DataRobot_SSL_Verify", "TRUE"), "TRUE")
  testReturn <- suppressWarnings(suppressMessages(with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() testURL,
    "datarobot:::Token" = function() fakeToken,
    ConnectToDataRobot(endpoint = testURL, token = fakeToken, sslVerify = FALSE)
  )))
  expect_equal(
    Sys.getenv("DataRobot_SSL_Verify", "TRUE"),
    "FALSE"
  )
})


Sys.setenv(DataRobot_endpoint = originalURL)
Sys.setenv(DataRobot_Token = originalToken)
Sys.setenv(DataRobot_SSL_Verify = originalSSLPref)
