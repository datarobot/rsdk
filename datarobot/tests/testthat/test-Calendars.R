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

getCalendarUrl <- UrlJoin("calendar", fakeCalendarId)
createCalendarUrl <- UrlJoin("calendars", "fileUpload")
listCalendarsUrl <- "calendar"
postCalendarUrl <- UrlJoin("calendar", fakeCalendarId)

ExpectCalendarShape <- function(calendar) {
  expectedCols <- c(
    "name", "created", "calendarStartDate", "calendarEndDate",
    "numEventTypes", "source", "projectIds", "id"
  )
  expect_s3_class(calendar, "dataRobotCalendar")

  expect_null(calendar$Id, info = "We explicitly map $Id to $id for consistency")
  expect_type(calendar$id, "character")

  ExpectHasKeys(calendar, expectedCols)
}

test_that("it can get a calendar", {
  getStub <- stub(httr::GET)
  getCalendarJson <- fileToChar("responses/getCalendar.json")
  calendarResponse <- httr:::response(
    url = getCalendarUrl,
    status_code = 200L,
    content = charToRaw(getCalendarJson)
  )
  getStub$onCall(1)$returns(calendarResponse)
  calendar <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetCalendar(fakeCalendarId)
  )
  expect_equal(getStub$calledTimes(), 1)

  ExpectCalendarShape(calendar)
})

test_that("it can list all calendars", {
  getStub <- stub(httr::GET)
  listCalendarsJson <- fileToChar("responses/listCalendars.json")
  calendarResponse <- httr:::response(
    url = listCalendarsUrl,
    status_code = 200L,
    content = charToRaw(listCalendarsJson)
  )
  getStub$onCall(1)$returns(calendarResponse)
  calendars <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListCalendars()
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_equal(length(calendars), 23)
  expect_s3_class(calendars, "listOfCalendars")
  lapply(calendars, ExpectCalendarShape)
})

describe("CreateCalendar()", {
  test_that("succeeds with a filename for a CSV", {
    postStub <- stub(httr::POST)
    createCalendarJson <- fileToChar("responses/createCalendar.json")
    createCalendarResponse <- httr:::response(
      url = createCalendarUrl,
      status_code = 202L,
      content = charToRaw(createCalendarJson)
    )
    postStub$onCall(1)$returns(createCalendarResponse)

    calendar <- with_mock(
      "httr::POST" = postStub$f,
      "httr::GET" = function() stop("Should not be called!"),
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(createCalendarResponse)
      },
      "datarobot:::UploadData" = function(file) file,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      CreateCalendar("calendar.csv")
    )
    expect_equal(postStub$calledTimes(), 1)
    ExpectCalendarShape(calendar)
  })

  test_that("succeeds with a data frame", {
    postStub <- stub(httr::POST)
    createCalendarJson <- fileToChar("responses/createCalendar.json")
    createCalendarResponse <- httr:::response(
      url = createCalendarUrl,
      status_code = 202L,
      content = charToRaw(createCalendarJson)
    )
    postStub$onCall(1)$returns(createCalendarResponse)

    calendarFileDF <- data.frame(list())
    calendar <- with_mock(
      "httr::POST" = postStub$f,
      "httr::GET" = function() stop("Should not be called!"),
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(createCalendarResponse)
      },
      "datarobot:::UploadData" = function(file) file,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      CreateCalendar(calendarFileDF)
    )
    expect_equal(postStub$calledTimes(), 1)
    ExpectCalendarShape(calendar)
  })

  test_that("fails when multiSeriesIdColumn has more than one value", {
    expect_error(CreateCalendar("calendar.csv",
      multiSeriesIdColumn = c("series-id", "series-id2")
    ))
    expect_error(CreateCalendar("calendar.csv",
      multiSeriesIdColumn = list("series-id", "series-id2")
    ))
  })

  test_that("passes a length-1 JSON array for the multiSeriesIdColumn parameter", {

    #' A helper method that extracts the payload from the DataRobotPOST
    #' so that we can ensure we are calling it with the expected arguments
    extractRequestPayload <- function(f) {
      calendar <- jsonlite::fromJSON(fileToChar("responses/createCalendar.json"))
      createCalendarJson <- fileToChar("responses/createCalendar.json")
      createCalendarResponse <- httr:::response(
        url = createCalendarUrl,
        status_code = 202L,
        content = charToRaw(createCalendarJson)
      )

      requestPayload <- list()
      with_mock(
        "datarobot::DataRobotPOST" = function(routeString, body = NULL, ...) {
          requestPayload <<- body
          return(httr:::response(
            url = createCalendarUrl,
            status_code = 202L,
            headers = list(location = getCalendarUrl),
            content = raw(0)
          ))
        },
        "datarobot::WaitForAsyncReturn" = function(...) {
          ParseReturnResponse(createCalendarResponse)
        },
        "datarobot:::UploadData" = function(file) file,
        "datarobot:::Endpoint" = function() fakeEndpoint,
        "datarobot:::Token" = function() fakeToken,
        f
      )

      return(requestPayload)
    }

    testRequest <- function(input, expectedPayload) {
      createCalendarPayload <- extractRequestPayload(
        CreateCalendar("calendar.csv", multiSeriesIdColumn = input)
      )
      expect_equal(
        createCalendarPayload$multiseriesIdColumns,
        expectedPayload
      )
    }

    expectedJsonArray <- jsonlite::toJSON("Store")

    testRequest("Store", expectedJsonArray)
    testRequest(c("Store"), expectedJsonArray)
    testRequest(list("Store"), expectedJsonArray)
  })
})

test_that("it can rename a calendar", {
  patchStub <- stub(httr::PATCH)
  createCalendarJson <- fileToChar("responses/createCalendar.json")
  calendarResponse <- httr:::response(
    url = postCalendarUrl,
    status_code = 202L,
    content = charToRaw(createCalendarJson)
  )
  patchStub$onCall(1)$returns(calendarResponse)
  getStub <- stub(httr::GET)
  getCalendarJson <- fileToChar("responses/getCalendar.json")
  calendarResponse <- httr:::response(
    url = getCalendarUrl,
    status_code = 200L,
    content = charToRaw(getCalendarJson)
  )
  getStub$onCall(1)$returns(calendarResponse)
  calendar <- with_mock(
    "httr::GET" = getStub$f,
    "httr::PATCH" = patchStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    UpdateCalendar(fakeCalendarId, "ThrowawayName")
  )
  expect_equal(patchStub$calledTimes(), 1)
  expect_equal(getStub$calledTimes(), 1)
  ExpectCalendarShape(calendar)
})

test_that("It can delete a calendar", {
  response <- with_mock(
    "datarobot:::DataRobotDELETE" = function(routeString,
                                             addUrl = TRUE,
                                             body = NULL,
                                             returnRawResponse = FALSE,
                                             ...) {
      routeForInspect <<- routeString
      ""
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    DeleteCalendar(fakeCalendar)
  )
  expect_null(response)
  expect_equal(routeForInspect, paste0("calendars/", fakeCalendarId, "/"))
})

test_that("it can get a calendar from a project", {
  getStub <- stub(httr::GET)
  getCalendarJson <- fileToChar("responses/getCalendar.json")
  calendarResponse <- httr:::response(
    url = getCalendarUrl,
    status_code = 200L,
    content = charToRaw(getCalendarJson)
  )
  getStub$onCall(1)$returns(calendarResponse)
  calendar <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot::GetDatetimePartition" = function(...) {
      list(calendarId = fakeCalendarId)
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetCalendarFromProject(fakeProject)
  )
  expect_equal(getStub$calledTimes(), 1)
  ExpectCalendarShape(calendar)
})
