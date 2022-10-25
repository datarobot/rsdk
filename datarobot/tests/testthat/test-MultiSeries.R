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

getMultiseriesJson <- fileToChar("responses/GetMultiseries.json")
getMultiseriesUrl <- UrlJoin(projectUrl, "multiseriesProperties")
multiseriesRequestResponse <- httr:::response(
  url = getMultiseriesUrl,
  status_code = 200L,
  content = charToRaw(getMultiseriesJson)
)

postCrossSeriesModelUrl <- UrlJoin(projectUrl, "crossSeriesProperties")
statusUrl <- UrlJoin(fakeEndpoint, "status", fakeJobId)
crossSeriesRequestResponse <- httr:::response(
  url = postCrossSeriesModelUrl,
  status_code = 202L,
  headers = list(location = statusUrl),
  content = raw(0)
)

getCrossSeriesJson <- fileToChar("responses/GetCrossSeries.json")
getCrossSeriesUrl <- UrlJoin(projectUrl, "crossSeriesProperties")
crossSeriesGetResponse <- httr:::response(
  url = getCrossSeriesUrl,
  status_code = 303L,
  content = charToRaw(getCrossSeriesJson)
)


test_that("RequestMultiSeriesDetection works", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(multiseriesRequestResponse)
  properties <- with_mock(
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
    "datarobot::WaitForAsyncReturn" = function(...) {
      ParseReturnResponse(multiseriesRequestResponse)
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    {
      expect_warning(
        RequestMultiSeriesDetection(fakeProject, fakeDateColumn),
        "Multiple potential multiseries id columns were detected"
      )
    }
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_type(properties, "list")
  ExpectHasKeys(properties, c(
    "timeSeriesEligible", "crossSeriesEligible",
    "crossSeriesEligibilityReason", "timeUnit", "timeStep"
  ))
  expect_true(properties$timeSeriesEligible)
  expect_null(properties$crossSeriesEligible)
  expect_null(properties$crossSeriesEligibilityReason)
  expect_null(properties$timeUnit)
  expect_null(properties$timeStep)
  expect_equal(bodyForInspect$datetimePartitionColumn, fakeDateColumn)
})


test_that("RequestMultiSeriesDetection can manually specify a Series ID", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(multiseriesRequestResponse)
  properties <- with_mock(
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
    "datarobot::WaitForAsyncReturn" = function(...) {
      ParseReturnResponse(multiseriesRequestResponse)
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    RequestMultiSeriesDetection(
      fakeProject, fakeDateColumn,
      fakeMultiIdColumn
    )
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_type(properties, "list")
  ExpectHasKeys(properties, c(
    "timeSeriesEligible", "crossSeriesEligible",
    "crossSeriesEligibilityReason", "timeUnit", "timeStep"
  ))
  expect_true(properties$timeSeriesEligible)
  expect_null(properties$crossSeriesEligible)
  expect_null(properties$crossSeriesEligibilityReason)
  expect_type(properties$timeUnit, "character")
  expect_type(properties$timeStep, "integer")
  expect_equal(bodyForInspect$datetimePartitionColumn, fakeDateColumn)
  expect_equal(bodyForInspect$multiseriesIdColumns, list(fakeMultiIdColumn))
})


test_that("RequestMultiSeriesDetection errors with more than one series ID", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(multiseriesRequestResponse)
  expect_error(
    with_mock(
      "httr::POST" = postStub$f,
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(multiseriesRequestResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      RequestMultiSeriesDetection(
        fakeProject,
        fakeDateColumn,
        list(
          fakeMultiIdColumn,
          "other-fake-col"
        )
      )
    ),
    "Currently only one multiseries id column is supported."
  )
})


test_that("RequestMultiSeriesDetection works with list input", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(crossSeriesRequestResponse)
  properties <- with_mock(
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
    "datarobot::WaitForAsyncReturn" = function(...) {
      ParseReturnResponse(crossSeriesGetResponse)
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    RequestCrossSeriesDetection(fakeProject,
      dateColumn = fakeDateColumn,
      crossSeriesGroupByColumns = fakeCrossIdColumn
    )
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_type(properties, "list")
  ExpectHasKeys(properties, c(
    "timeSeriesEligible", "crossSeriesEligible",
    "crossSeriesEligibilityReason", "timeUnit", "timeStep"
  ))
  expect_true(properties$timeSeriesEligible)
  expect_true(properties$crossSeriesEligible)
  expect_type(properties$crossSeriesEligibilityReason, "character")
  expect_null(properties$timeUnit)
  expect_null(properties$timeStep)
  expect_equal(bodyForInspect$datetimePartitionColumn, fakeDateColumn)
})


test_that("RequestCrossSeriesDetection can manually specify a Series ID", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(crossSeriesRequestResponse)
  properties <- with_mock(
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
    "datarobot::WaitForAsyncReturn" = function(...) {
      ParseReturnResponse(crossSeriesGetResponse)
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    RequestCrossSeriesDetection(fakeProject,
      dateColumn = fakeDateColumn,
      multiseriesIdColumns = fakeMultiIdColumn,
      crossSeriesGroupByColumns = fakeCrossIdColumn
    )
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_type(properties, "list")
  ExpectHasKeys(properties, c(
    "timeSeriesEligible", "crossSeriesEligible",
    "crossSeriesEligibilityReason", "timeUnit", "timeStep"
  ))
  expect_true(properties$timeSeriesEligible)
  expect_true(properties$crossSeriesEligible)
  expect_type(properties$crossSeriesEligibilityReason, "character")
  expect_null(properties$timeUnit)
  expect_null(properties$timeStep)
  expect_equal(bodyForInspect$datetimePartitionColumn, fakeDateColumn)
  expect_equal(bodyForInspect$multiseriesIdColumn, fakeMultiIdColumn)
  expect_equal(bodyForInspect$crossSeriesGroupByColumns, list(fakeCrossIdColumn))
})


test_that("RequestCrossSeriesDetection errors with more than one series ID", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(crossSeriesRequestResponse)
  expect_error(
    with_mock(
      "httr::POST" = postStub$f,
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(crossSeriesGetResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      RequestCrossSeriesDetection(fakeProject,
        dateColumn = fakeDateColumn,
        multiseriesIdColumns = list("1", "2"),
        crossSeriesGroupByColumns = fakeCrossIdColumn
      )
    ),
    "Currently only one multiseries id column is supported."
  )
})


test_that("RequestCrossSeriesDetection errors with more than one cross series group", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(crossSeriesRequestResponse)
  expect_error(
    with_mock(
      "httr::POST" = postStub$f,
      "datarobot::WaitForAsyncReturn" = function(...) {
        ParseReturnResponse(crossSeriesGetResponse)
      },
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      RequestCrossSeriesDetection(fakeProject,
        dateColumn = fakeDateColumn,
        multiseriesIdColumns = fakeMultiIdColumn,
        crossSeriesGroupByColumns = list("1", "2")
      )
    ),
    "Currently only one cross series group by column is supported."
  )
})


test_that("RequestCrossSeriesDetection works with list input", {
  postStub <- stub(httr::POST)
  postStub$onCall(1)$returns(crossSeriesRequestResponse)
  properties <- with_mock(
    "httr::POST" = postStub$f,
    "datarobot::WaitForAsyncReturn" = function(...) {
      ParseReturnResponse(crossSeriesGetResponse)
    },
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    RequestCrossSeriesDetection(fakeProject,
      dateColumn = fakeDateColumn,
      multiseriesIdColumns = list(fakeMultiIdColumn),
      crossSeriesGroupByColumns = list(fakeCrossIdColumn)
    )
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_type(properties, "list")
  ExpectHasKeys(properties, c(
    "timeSeriesEligible", "crossSeriesEligible",
    "crossSeriesEligibilityReason", "timeUnit", "timeStep"
  ))
  expect_true(properties$timeSeriesEligible)
  expect_true(properties$crossSeriesEligible)
  expect_type(properties$crossSeriesEligibilityReason, "character")
  expect_null(properties$timeUnit)
  expect_null(properties$timeStep)
})


test_that("GetMultiSeriesProperties works", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(multiseriesRequestResponse)
  properties <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetMultiSeriesProperties(fakeProject, fakeDateColumn, fakeMultiIdColumn)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_type(properties, "list")
  ExpectHasKeys(properties, c(
    "timeSeriesEligible", "crossSeriesEligible",
    "crossSeriesEligibilityReason", "timeUnit", "timeStep"
  ))
  expect_true(properties$timeSeriesEligible)
  expect_null(properties$crossSeriesEligible)
  expect_null(properties$crossSeriesEligibilityReason)
  expect_type(properties$timeUnit, "character")
  expect_type(properties$timeStep, "integer")
})


test_that("GetMultiSeriesProperties errors with more than one series ID", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(multiseriesRequestResponse)
  expect_error(
    with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetMultiSeriesProperties(
        fakeProject,
        fakeDateColumn,
        list(fakeMultiIdColumn, "other-fake-col")
      )
    ),
    "Currently only one multiseries id column is supported."
  )
})


test_that("GetMultiSeriesProperties works with list input", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(multiseriesRequestResponse)
  properties <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetMultiSeriesProperties(
      fakeProject, fakeDateColumn,
      list(fakeMultiIdColumn)
    )
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_type(properties, "list")
  ExpectHasKeys(properties, c(
    "timeSeriesEligible", "crossSeriesEligible",
    "crossSeriesEligibilityReason", "timeUnit", "timeStep"
  ))
  expect_true(properties$timeSeriesEligible)
  expect_null(properties$crossSeriesEligible)
  expect_null(properties$crossSeriesEligibilityReason)
  expect_type(properties$timeUnit, "character")
  expect_type(properties$timeStep, "integer")
})


test_that("GetMultiSeriesProperties works when there's no detected multiseries", {
  getStub <- stub(httr::GET)
  getMultiseriesJson <- fileToChar("responses/GetNoMultiseries.json")
  getMultiseriesUrl <- UrlJoin(projectUrl, "multiseriesProperties")
  multiseriesRequestResponse <- httr:::response(
    url = getMultiseriesUrl,
    status_code = 200L,
    content = charToRaw(getMultiseriesJson)
  )
  getStub$onCall(1)$returns(multiseriesRequestResponse)
  properties <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetMultiSeriesProperties(
      fakeProject, fakeDateColumn,
      list(fakeMultiIdColumn)
    )
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_type(properties, "list")
  ExpectHasKeys(properties, c(
    "timeSeriesEligible", "crossSeriesEligible",
    "crossSeriesEligibilityReason", "timeUnit", "timeStep"
  ))
  expect_false(properties$timeSeriesEligible)
  expect_null(properties$crossSeriesEligible)
  expect_null(properties$crossSeriesEligibilityReason)
  expect_null(properties$timeUnit)
  expect_null(properties$timeStep)
})


test_that("GetMultiSeriesProperties works with cross series groupby", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(multiseriesRequestResponse)
  getStub$onCall(2)$returns(crossSeriesGetResponse)
  properties <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetMultiSeriesProperties(fakeProject,
      dateColumn = fakeDateColumn,
      multiseriesIdColumns = fakeMultiIdColumn,
      crossSeriesGroupByColumns = fakeCrossIdColumn
    )
  )
  expect_equal(getStub$calledTimes(), 2)
  expect_type(properties, "list")
  ExpectHasKeys(properties, c(
    "timeSeriesEligible", "crossSeriesEligible",
    "crossSeriesEligibilityReason", "timeUnit", "timeStep"
  ))
  expect_true(properties$timeSeriesEligible)
  expect_true(properties$crossSeriesEligible)
  expect_type(properties$crossSeriesEligibilityReason, "character")
  expect_type(properties$timeUnit, "character")
  expect_type(properties$timeStep, "integer")
})


test_that("GetMultiSeriesProperties errors with more than one multiseries ID, with groupby", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(multiseriesRequestResponse)
  getStub$onCall(2)$returns(crossSeriesGetResponse)
  expect_error(
    with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetMultiSeriesProperties(fakeProject,
        dateColumn = fakeDateColumn,
        multiseriesIdColumns = list("1", "2"),
        crossSeriesGroupByColumns = fakeCrossIdColumn
      )
    ),
    "Currently only one multiseries id column is supported."
  )
})


test_that("GetMultiSeriesProperties errors with more than one cross series groupby", {
  getStub <- stub(httr::GET)
  getMultiseriesJson <- fileToChar("responses/GetMultiseries.json")
  getStub$onCall(1)$returns(multiseriesRequestResponse)
  getStub$onCall(2)$returns(crossSeriesGetResponse)
  expect_error(
    with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      GetMultiSeriesProperties(fakeProject,
        dateColumn = fakeDateColumn,
        multiseriesIdColumns = fakeMultiIdColumn,
        crossSeriesGroupByColumns = list("1", "2")
      )
    ),
    "Currently only one cross series group by column is supported."
  )
})


test_that("GetMultiSeriesProperties works with list input for cross series groupby", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(multiseriesRequestResponse)
  getStub$onCall(2)$returns(crossSeriesGetResponse)
  properties <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetMultiSeriesProperties(fakeProject,
      dateColumn = fakeDateColumn,
      multiseriesIdColumns = list(fakeMultiIdColumn),
      crossSeriesGroupByColumns = list(fakeCrossIdColumn)
    )
  )
  expect_equal(getStub$calledTimes(), 2)
  expect_type(properties, "list")
  ExpectHasKeys(properties, c(
    "timeSeriesEligible", "crossSeriesEligible",
    "crossSeriesEligibilityReason", "timeUnit", "timeStep"
  ))
  expect_true(properties$timeSeriesEligible)
  expect_true(properties$crossSeriesEligible)
  expect_type(properties$crossSeriesEligibilityReason, "character")
  expect_type(properties$timeUnit, "character")
  expect_type(properties$timeStep, "integer")
})


test_that("GetMultiSeriesProperties works when there's no detected cross series", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(multiseriesRequestResponse)
  getStub$onCall(2)$returns(crossSeriesGetResponse)
  properties <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetMultiSeriesProperties(fakeProject,
      dateColumn = fakeDateColumn,
      multiseriesIdColumns = fakeMultiIdColumn,
      crossSeriesGroupByColumns = "not_detectable"
    )
  )
  expect_equal(getStub$calledTimes(), 2)
  expect_type(properties, "list")
  ExpectHasKeys(properties, c(
    "timeSeriesEligible", "crossSeriesEligible",
    "crossSeriesEligibilityReason", "timeUnit", "timeStep"
  ))
  expect_true(properties$timeSeriesEligible)
  expect_false(properties$crossSeriesEligible)
  expect_type(properties$crossSeriesEligibilityReason, "character")
  expect_type(properties$timeUnit, "character")
  expect_type(properties$timeStep, "integer")
})


test_that("ValidateMultiSeriesProperties verifies a working multiseries", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(multiseriesRequestResponse)
  properties <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetMultiSeriesProperties(
      fakeProject, fakeDateColumn,
      list(fakeMultiIdColumn)
    )
  )
  verification <- ValidateMultiSeriesProperties(properties)
  expect_true(verification)
})


test_that("ValidateMultiSeriesProperties verifies a failed series - returns FALSE", {
  getStub <- stub(httr::GET)
  getMultiseriesJson <- fileToChar("responses/GetNoMultiseries.json")
  getMultiseriesUrl <- UrlJoin(projectUrl, "multiseriesProperties")
  multiseriesRequestResponse <- httr:::response(
    url = getMultiseriesUrl,
    status_code = 200L,
    content = charToRaw(getMultiseriesJson)
  )
  getStub$onCall(1)$returns(multiseriesRequestResponse)
  properties <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetMultiSeriesProperties(
      fakeProject, fakeDateColumn,
      list(fakeMultiIdColumn)
    )
  )
  verification <- ValidateMultiSeriesProperties(properties, error = FALSE)
  expect_false(verification)
})


test_that("ValidateMultiSeriesProperties verifies a failed series - raises error", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(multiseriesRequestResponse)
  properties <- list(timeSeriesEligible = FALSE, crossSeriesEligible = FALSE)
  expect_error(
    ValidateMultiSeriesProperties(properties, error = TRUE),
    "multiseries id columns are not eligible"
  )
})

test_that("ValidateMultiSeriesProperties verifies a failed cross series", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(multiseriesRequestResponse)
  properties <- list(timeSeriesEligible = TRUE, crossSeriesEligible = FALSE)
  expect_error(
    ValidateMultiSeriesProperties(properties, error = TRUE),
    "cross-series group-by column is not eligible"
  )
})


test_that("ValidateMultiSeriesProperties raises error with no list", {
  expect_error(ValidateMultiSeriesProperties("foo", error = TRUE), "not a list")
})


test_that("ValidateMultiSeriesProperties raises error with no timeSeriesEligble key", {
  expect_error(
    ValidateMultiSeriesProperties(list("foo" = 1), error = TRUE),
    "do not contain timeSeriesEligible key"
  )
})


test_that("ValidateMultiSeriesProperties raises error with no crossSeriesEligible key", {
  expect_error(
    ValidateMultiSeriesProperties(list(
      "timeSeriesEligible" = TRUE,
      "foo" = 1
    ), error = TRUE),
    "do not contain crossSeriesEligible key"
  )
})
