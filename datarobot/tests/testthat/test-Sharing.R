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

describe("GetSharingPath", {
  test_that("it can get a path for data stores", {
    expectedPath <- UrlJoin("externalDataStores", fakeDataStoreId, "accessControl")
    expect_equal(GetSharingPath(fakeDataStore), expectedPath)
  })

  test_that("it can get a path for data sources", {
    expectedPath <- UrlJoin("externalDataSources", fakeDataSourceId, "accessControl")
    expect_equal(GetSharingPath(fakeDataSource), expectedPath)
  })

  test_that("it can get a path for projects", {
    expectedPath <- UrlJoin("projects", fakeProjectId, "accessControl")
    expect_equal(GetSharingPath(fakeProject), expectedPath)
  })

  test_that("it cannot get a path for foo", {
    expect_error(GetSharingPath(fakeFoo), "Objects of class dataRobotFoo cannot be shared.")
  })
})


describe("ListSharingAccess", {
  getAccessUrl <- UrlJoin("externalDataSources", fakeDataStoreId, "accessControl")
  getAccessJson <- fileToChar("responses/getAccess.json")
  accessResponse <- httr:::response(
    url = getAccessUrl,
    status_code = 200L,
    content = charToRaw(getAccessJson)
  )

  test_that("it can share data stores", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(accessResponse)
    access <- with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      ListSharingAccess(fakeDataStore)
    )
    expect_equal(getStub$calledTimes(), 1)
    expect_type(access, "list")
    ExpectHasKeys(access[[1]], c("username", "userId", "role", "canShare"))
  })

  test_that("it can share data sources", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(accessResponse)
    access <- with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      ListSharingAccess(fakeDataSource)
    )
    expect_equal(getStub$calledTimes(), 1)
    expect_type(access, "list")
    ExpectHasKeys(access[[1]], c("username", "userId", "role", "canShare"))
  })

  test_that("it can share projects", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(accessResponse)
    access <- with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      ListSharingAccess(fakeProject)
    )
    expect_equal(getStub$calledTimes(), 1)
    expect_type(access, "list")
    ExpectHasKeys(access[[1]], c("username", "userId", "role", "canShare"))
  })

  test_that("it cannot share foo", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(accessResponse)
    expect_error(with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      ListSharingAccess(fakeFoo), "Objects of class foo cannot be shared."
    ))
    expect_equal(getStub$calledTimes(), 0)
  })
})


describe("ValidateAccessList", {
  test_that("Errors if not a list", {
    expect_error(ValidateAccessList(iris), "Must specify access via an access list")
  })
  test_that("Errors if does not have username", {
    badList <- list(list(role = "USER"))
    expect_error(
      ValidateAccessList(badList),
      "Access list is malformed: Does not contain `username`."
    )
  })
  test_that("Errors if does not have role", {
    badList <- list(list(username = "user@user.com"))
    expect_error(
      ValidateAccessList(badList),
      "Access list is malformed: Does not contain `role`."
    )
  })
  test_that("is fine with both parameters", {
    goodList <- list(list(role = "USER", username = "user@user.com"))
    expect_true(ValidateAccessList(goodList))
  })
  test_that("Errors if the role is wrong", {
    badList <- list(list(role = "BOGUS", username = "user@user.com"))
    expect_error(ValidateAccessList(badList), "not a valid role")
  })
  test_that("Errors if does not have either", {
    badList <- list(list())
    expect_error(
      ValidateAccessList(badList),
      "Access list is malformed: Does not contain `username`."
    )
  })
  test_that("canShare key is preserved", {
    goodList <- list(list(role = "USER", username = "user@user.com", canShare = FALSE))
    expect_true(ValidateAccessList(goodList))
  })
  test_that("Errors if not a list II", {
    badList <- list(list(role = "USER", username = "user@user.com"), iris)
    expect_error(
      ValidateAccessList(badList),
      "Access list is malformed: Does not contain `username`."
    )
  })
  test_that("Errors if does not have username II", {
    badList <- list(list(role = "USER", username = "user@user.com"), list(role = "USER"))
    expect_error(
      ValidateAccessList(badList),
      "Access list is malformed: Does not contain `username`."
    )
  })
  test_that("Errors if does not have role II", {
    badList <- list(
      list(role = "USER", username = "user@user.com"),
      list(username = "user2@user.com")
    )
    expect_error(
      ValidateAccessList(badList),
      "Access list is malformed: Does not contain `role`."
    )
  })
  test_that("is fine with both parameters II", {
    goodList <- list(
      list(role = "USER", username = "user@user.com"),
      list(role = "USER", username = "user2@user.com")
    )
    expect_true(ValidateAccessList(goodList))
  })
  test_that("Errors if the role is wrong II", {
    badList <- list(
      list(role = "USER", username = "user@user.com"),
      list(role = "BOGUS", username = "user2@user.com")
    )
    expect_error(ValidateAccessList(badList), "not a valid role")
  })
})


describe("FormatAccessList", {
  test_that("It includes validation", {
    badList <- list(
      list(role = "USER", username = "user@user.com"),
      list(role = "BOGUS", username = "user2@user.com")
    )
    expect_error(FormatAccessList(fakeDataStore, badList), "not a valid role")
  })

  test_that("It suppresses extra keys", {
    badList <- list(
      list(role = "USER", username = "user@user.com", canShare = TRUE, extraKey = 1),
      list(role = "USER", username = "user2@user.com", canShare = TRUE)
    )
    goodList <- list(
      list(role = "USER", username = "user@user.com", canShare = TRUE),
      list(role = "USER", username = "user2@user.com", canShare = TRUE)
    )
    goodList <- lapply(goodList, function(a) lapply(a, jsonlite::unbox))
    expect_equal(FormatAccessList(fakeDataStore, badList), list(data = goodList))
  })

  test_that("It transforms into list-of-lists", {
    badList <- list(role = "USER", username = "user@user.com", canShare = TRUE)
    goodList <- list(badList)
    goodList <- lapply(goodList, function(a) lapply(a, jsonlite::unbox))
    expect_equal(FormatAccessList(fakeDataStore, badList), list(data = goodList))
  })

  test_that("canShare preserved for non-projects", {
    accessList <- list(role = "USER", username = "user@user.com", canShare = TRUE)
    accessList <- FormatAccessList(fakeDataStore, accessList)
    expect_true(as.logical(accessList$data[[1]]$canShare))
  })

  test_that("canShare dropped for projects", {
    accessList <- list(role = "USER", username = "user@user.com", canShare = TRUE)
    accessList <- FormatAccessList(fakeProject, accessList)
    expect_null(accessList$data[[1]]$canShare)
  })
})


describe("UpdateAccess", {
  patchAccessUrl <- GetSharingPath(fakeDataStore)
  patchAccessJson <- fileToChar("responses/getDataStore.json")
  accessResponse <- httr:::response(
    url = patchAccessUrl,
    status_code = 202L,
    content = charToRaw(patchAccessJson)
  )

  test_that("it can update access to a data store", {
    patchStub <- stub(httr::PATCH)
    patchStub$onCall(1)$returns(accessResponse)
    accessList <- list(username = "test@test.com", role = "USER", canShare = TRUE)
    access <- with_mock(
      "httr::PATCH" = patchStub$f,
      "httr::GET" = function(...) stop("Should not be called!"),
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
      UpdateAccess(fakeDataStore, accessList)
    )
    expect_equal(patchStub$calledTimes(), 1)
    expect_null(access)
    expect_equal(bodyForInspect$data, list(lapply(accessList, jsonlite::unbox)))
  })

  test_that("it can update access to a data store II", {
    patchStub <- stub(httr::PATCH)
    patchStub$onCall(1)$returns(accessResponse)
    accessList <- list(
      list(username = "test@test.com", role = "USER", canShare = TRUE),
      list(username = "test2@test.com", role = "USER", canShare = FALSE)
    )
    access <- with_mock(
      "httr::PATCH" = patchStub$f,
      "httr::GET" = function(...) stop("Should not be called!"),
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
      UpdateAccess(fakeDataStore, accessList)
    )
    expect_equal(patchStub$calledTimes(), 1)
    expect_null(access)
    accessList <- lapply(accessList, function(a) lapply(a, jsonlite::unbox))
    expect_equal(bodyForInspect$data, accessList)
  })

  test_that("it still validates", {
    badList <- list(
      list(username = "test@test.com", role = "USER", canShare = TRUE),
      list(username = "test2@test.com", role = "BOGUS", canShare = FALSE)
    )
    expect_error(with_mock(
      "httr::PATCH" = function(...) stop("Should not be called!"),
      "httr::GET" = function(...) stop("Should not be called!"),
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      UpdateAccess(fakeDataStore, badList)
    ), "not a valid role")
  })

  test_that("it can update access to a project", {
    patchStub <- stub(httr::PATCH)
    patchStub$onCall(1)$returns(accessResponse)
    accessList <- list(username = "test@test.com", role = "USER")
    access <- with_mock(
      "httr::PATCH" = patchStub$f,
      "httr::GET" = function(...) stop("Should not be called!"),
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
      UpdateAccess(fakeProject, accessList)
    )
    expect_equal(patchStub$calledTimes(), 1)
    expect_null(access)
    expect_equal(bodyForInspect$data, list(lapply(accessList, jsonlite::unbox)))
  })
})


describe("Share", {
  patchAccessUrl <- GetSharingPath(fakeDataStore)
  patchAccessJson <- fileToChar("responses/getDataStore.json")
  accessResponse <- httr:::response(
    url = patchAccessUrl,
    status_code = 202L,
    content = charToRaw(patchAccessJson)
  )
  getStub <- stub(httr::GET)
  getAccessUrl <- UrlJoin("externalDataStores", fakeDataStoreId, "accessControl")
  getAccessJson <- fileToChar("responses/getAccess.json")
  accessGetResponse <- httr:::response(
    url = getAccessUrl,
    status_code = 200L,
    content = charToRaw(getAccessJson)
  )

  test_that("it can share a data store", {
    patchStub <- stub(httr::PATCH)
    patchStub$onCall(1)$returns(accessResponse)
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(accessGetResponse)
    access <- with_mock(
      "httr::PATCH" = patchStub$f,
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
      Share(fakeDataStore, "test@test.com")
    )
    expect_equal(patchStub$calledTimes(), 1)
    expect_equal(getStub$calledTimes(), 1)
    expect_null(access)
    accessList <- list(list(username = "test@test.com", role = "CONSUMER"))
    accessList <- lapply(accessList, function(a) lapply(a, jsonlite::unbox))
    expect_true(accessList %in% bodyForInspect$data)
  })

  test_that("it can share a data store wih non-default role", {
    patchStub <- stub(httr::PATCH)
    patchStub$onCall(1)$returns(accessResponse)
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(accessGetResponse)
    access <- with_mock(
      "httr::PATCH" = patchStub$f,
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
      Share(fakeDataStore, "test@test.com", role = "OWNER")
    )
    expect_equal(patchStub$calledTimes(), 1)
    expect_equal(getStub$calledTimes(), 1)
    expect_null(access)
    accessList <- list(username = "test@test.com", role = "OWNER")
    accessList <- lapply(accessList, jsonlite::unbox)
    expect_true(list(accessList) %in% bodyForInspect$data)
  })

  test_that("it can share a data store wih non-default role and canShare", {
    patchStub <- stub(httr::PATCH)
    patchStub$onCall(1)$returns(accessResponse)
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(accessGetResponse)
    access <- with_mock(
      "httr::PATCH" = patchStub$f,
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
      Share(fakeDataStore, "test@test.com", role = "OWNER", canShare = FALSE)
    )
    expect_equal(patchStub$calledTimes(), 1)
    expect_equal(getStub$calledTimes(), 1)
    expect_null(access)
    accessList <- list(username = "test@test.com", role = "OWNER", canShare = FALSE)
    accessList <- lapply(accessList, jsonlite::unbox)
    expect_true(list(accessList) %in% bodyForInspect$data)
  })

  test_that("it can revoke with role = NULL", {
    patchStub <- stub(httr::PATCH)
    patchStub$onCall(1)$returns(accessResponse)
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(accessGetResponse)
    access <- with_mock(
      "httr::PATCH" = patchStub$f,
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
      Share(fakeDataStore, "test@test.com", role = NULL)
    )
    expect_equal(patchStub$calledTimes(), 1)
    expect_equal(getStub$calledTimes(), 1)
    expect_null(access)
    accessList <- list(username = "test@test.com", role = NULL)
    accessList <- lapply(accessList, jsonlite::unbox)
    expect_true(list(accessList) %in% bodyForInspect$data)
  })

  test_that("it still validates", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(accessGetResponse)
    expect_error(
      with_mock(
        "httr::PATCH" = function(...) stop("Should not be called!"),
        "httr::GET" = getStub$f,
        "datarobot:::Endpoint" = function() fakeEndpoint,
        "datarobot:::Token" = function() fakeToken,
        Share(fakeDataStore, username = "test@test.com", role = "BOGUS")
      ),
      "not a valid role"
    )
  })

  test_that("it can only share with a single user", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(accessGetResponse)
    expect_error(
      with_mock(
        "httr::PATCH" = function(...) stop("Should not be called!"),
        "httr::GET" = getStub$f,
        "datarobot:::Endpoint" = function() fakeEndpoint,
        "datarobot:::Token" = function() fakeToken,
        Share(fakeDataStore, username = c(
          "test@test.com",
          "test2@test.com"
        ), role = "USER")
      ),
      "one user at a time"
    )
  })

  test_that("it can't share with someone who is already shared", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(accessGetResponse)
    expect_error(
      with_mock(
        "httr::GET" = getStub$f,
        "httr::PATCH" = function(...) {
          stop(
            "Multiple changes were specified ",
            "for a single user"
          )
        },
        "datarobot:::Endpoint" = function() fakeEndpoint,
        "datarobot:::Token" = function() fakeToken,
        Share(fakeDataStore, username = "test@test.com", role = "USER")
      ),
      "User test@test.com is already shared on this dataRobotDataStore"
    )
  })

  test_that("it can't share with someone who does not exist", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(accessGetResponse)
    expect_error(
      with_mock(
        "httr::GET" = getStub$f,
        "httr::PATCH" = function(...) {
          stop(
            "The following users were not ",
            "found"
          )
        },
        "datarobot:::Endpoint" = function() fakeEndpoint,
        "datarobot:::Token" = function() fakeToken,
        Share(fakeDataStore, username = "test@test.com", role = "USER")
      ),
      "User test@test.com was not found"
    )
  })

  test_that("it can share a project", {
    patchStub <- stub(httr::PATCH)
    patchStub$onCall(1)$returns(accessResponse)
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(accessGetResponse)
    access <- with_mock(
      "httr::PATCH" = patchStub$f,
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
      Share(fakeProject, "test@test.com")
    )
    expect_equal(patchStub$calledTimes(), 1)
    expect_equal(getStub$calledTimes(), 1)
    expect_null(access)
    accessList <- list(list(username = "test@test.com", role = "USER"))
    accessList <- lapply(accessList, function(a) lapply(a, jsonlite::unbox))
    expect_true(accessList %in% bodyForInspect$data)
  })

  test_that("it can share a calendar", {
    patchStub <- stub(httr::PATCH)
    patchStub$onCall(1)$returns(accessResponse)
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(accessGetResponse)
    access <- with_mock(
      "httr::PATCH" = patchStub$f,
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
      Share(fakeCalendar, "test@test.com")
    )
    expect_equal(patchStub$calledTimes(), 1)
    expect_equal(getStub$calledTimes(), 1)
    expect_null(access)
    accessList <- list(list(username = "test@test.com", role = "READ_ONLY"))
    accessList <- lapply(accessList, function(a) lapply(a, jsonlite::unbox))
    expect_true(accessList %in% bodyForInspect$users)
  })
})
