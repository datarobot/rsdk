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

test_that("it can list drivers", {
  getStub <- stub(httr::GET)
  listDriverUrl <- UrlJoin("externalDataDrivers")
  listDriverJson <- fileToChar("responses/listDrivers.json")
  driverResponse <- httr:::response(
    url = listDriverUrl,
    status_code = 200L,
    content = charToRaw(listDriverJson)
  )
  getStub$onCall(1)$returns(driverResponse)
  drivers <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListDrivers()
  )
  expect_equal(getStub$calledTimes(), 1)
  expectedCols <- c("id", "canonicalName", "className", "baseNames", "creator")
  expect_true(all(expectedCols %in% names(drivers)))
})

test_that("it can get a driver", {
  getStub <- stub(httr::GET)
  listDriverUrl <- UrlJoin("externalDataDrivers", fakeDriverId)
  listDriverJson <- fileToChar("responses/getDriver.json")
  driverResponse <- httr:::response(
    url = listDriverUrl,
    status_code = 200L,
    content = charToRaw(listDriverJson)
  )
  getStub$onCall(1)$returns(driverResponse)
  driver <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetDriver(fakeDriverId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(driver, "dataRobotDriver")
  expectedCols <- c("id", "canonicalName", "className", "baseNames", "creator")
  expect_true(all(expectedCols %in% names(driver)))
})
