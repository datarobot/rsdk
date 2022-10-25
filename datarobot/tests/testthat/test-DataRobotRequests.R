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


describe("DataRobotAddHeaders", {
  originalUserAgentSuffix <- Sys.getenv("DataRobot_User_Agent_Suffix")

  test_that("User-Agent header is in correct format", {
    with_mock(
      "Sys.info" = function() list(sysname = "foo", release = "1", machine = "x86"),
      "httr::add_headers" = function(...) list(...),
      {
        expectedAgent <- sprintf(
          "DataRobotRClient/%s (foo 1 x86)",
          packageVersion(packageName())
        )
        headers <- DataRobotAddHeaders()
        expect_equal(headers, list("User-Agent" = expectedAgent))
      }
    )
  })

  test_that("We add a suffix to User-Agent header", {
    with_mock(
      "httr::add_headers" = function(...) list(...),
      {
        # Capture the headers before adding a suffix so we can test that we append correctly.
        expected <- DataRobotAddHeaders()
        suffix <- "Bumblebee (5.2.1)"
        SaveUserAgentSuffix(suffix)
        headers <- DataRobotAddHeaders()
        expect_true(startsWith(headers[["User-Agent"]], expected[["User-Agent"]]))
        expect_true(endsWith(headers[["User-Agent"]], sprintf(" %s", suffix)))
      }
    )
  })

  Sys.setenv(DataRobot_User_Agent_Suffix = originalUserAgentSuffix)
})


describe("TryingToSubmitNull", {
  test_that("it identifies NULL", {
    expect_true(TryingToSubmitNull(list(a = 1, b = NULL, c = 3)))
  })

  test_that("it identifies nested NULL", {
    expect_true(TryingToSubmitNull(list(
      a = list(b = 1, c = 2),
      d = list(e = 1, f = NULL),
      g = 3
    )))
  })

  test_that("it identifies not NULL", {
    expect_false(TryingToSubmitNull(list(a = 1, b = 2, c = 3)))
  })

  test_that("it identifies not nested NULL", {
    expect_false(TryingToSubmitNull(list(
      a = list(b = 1, c = 2),
      d = list(e = 1, f = 2),
      g = 3
    )))
  })
})


describe("ResponseIsRedirection", {
  test_that("it identifies redirection", {
    expect_true(ResponseIsRedirection(303))
  })

  test_that("it identifies non-redirection", {
    expect_false(ResponseIsRedirection(200))
  })
})


describe("ParseReturnResponse", {
  test_that("it returns NA if there's NA content", {
    with_mock("httr::content" = function(x, ...) x, {
      expect_equal(ParseReturnResponse(NA), NA)
    })
  })

  test_that("it returns blank if content is blank", {
    with_mock("httr::content" = function(x, ...) x, {
      expect_equal(ParseReturnResponse(""), "")
    })
  })

  test_that("it raises an error if there's an error", {
    with_mock(
      "httr::content" = function(x, ...) x,
      "jsonlite::fromJSON" = function(...) stop("in the name of love!"),
      {
        expect_error(ParseReturnResponse("text"), "Expected JSON, received:")
      }
    )
  })

  test_that("it returns parsed JSON", {
    with_mock(
      "httr::content" = function(x, ...) x,
      "jsonlite::fromJSON" = function(x, ...) x,
      {
        expect_equal(ParseReturnResponse("text"), "text")
      }
    )
  })
})


describe("UrlJoin", {
  test_that("it raises an error on query parameters", {
    expect_error(UrlJoin("a", "b?query=1"), "should not contain query parameters")
  })

  test_that("it joins URL parameters", {
    expect_equal(UrlJoin("a", "b"), "a/b/")
  })

  test_that("it removes trailing slashes", {
    expect_equal(UrlJoin("a/", "b/"), "a/b/")
  })
})


describe("CheckUrl", {
  test_that("it identifies a URL", {
    expect_true(CheckUrl("http://app.datarobot.com/url"))
  })

  test_that("it identifies a non-URL", {
    expect_error(CheckUrl("notaurl"), "URL invalid")
  })
})


describe("BuildPath", {
  test_that("it requires a token", {
    with_mock("datarobot:::Token" = function() "", {
      expect_error(BuildPath("projects"), "User authentication required. See ConnectToDataRobot documentation")
    })
  })

  test_that("it requires an endpoint", {
    with_mock("datarobot:::Endpoint" = function() "", {
      expect_error(BuildPath("projects"), "User authentication required. See ConnectToDataRobot documentation")
    })
  })

  test_that("it pre-pends the endpoint on to the route if addURL is TRUE", {
    path <- with_mock(
      "datarobot::Token" = function() "token",
      "datarobot::Endpoint" = function() "endpoint", {
      BuildPath("projects", addUrl = TRUE)
    })
    expect_equal(path$fullPath, "endpoint/projects/")
  })

  test_that("it does not pre-pend the endpoint on to the route if addURL is FALSE", {
    path <- with_mock(
      "datarobot::Token" = function() "token",
      "datarobot::Endpoint" = function() "endpoint", {
      BuildPath("/projects", addUrl = FALSE)
    })
    expect_equal(path$fullPath, "/projects")
  })

  test_that("it returns the token in the authHead", {
    path <- with_mock(
      "datarobot::Token" = function() "token",
      "datarobot::Endpoint" = function() "endpoint", {
      BuildPath("projects")
    })
    expect_equal(path$authHead, "Token token")
  })
})


describe("StopIfResponseIsError", {
  test_that("error on NULL", {
    expect_error(StopIfResponseIsError(NULL), "No HTTR response object")
  })

  test_that("no error on 2XX", {
    expect_true(StopIfResponseIsError(200))
  })

  test_that("no error on 3XX", {
    expect_true(StopIfResponseIsError(303))
  })

  test_that("error on 4XX", {
    with_mock("datarobot:::ParseReturnResponse" = function(x) {
      list(
        errors = list(error = "error"),
        message = "missing"
      )
    }, {
      expect_error(StopIfResponseIsError(404), "(404)")
    })
  })
})


describe("MakeDataRobotRequest", {
  test_that("MakeDataRobotRequest works", {
    getStub <- stub(httr::GET)
    projectResponse <- httr:::response(
      url = "url",
      status_code = 200L,
      content = charToRaw('{"payload": "alpha"}')
    )
    getStub$onCall(1)$returns(projectResponse)
    response <- with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      MakeDataRobotRequest(httr::GET, "lol")
    )
    expect_s3_class(response, "response")
  })

  test_that("MakeDataRobotRequest will parse the raw response object", {
    getStub <- stub(httr::GET)
    projectResponse <- httr:::response(
      url = "url",
      status_code = 200L,
      content = charToRaw('{"payload": "alpha"}')
    )
    getStub$onCall(1)$returns(projectResponse)
    response <- with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      MakeDataRobotRequest(httr::GET, "lol", returnRawResponse = FALSE)
    )
    expect_equal(response, list(payload = "alpha"))
  })

  test_that("MakeDataRobotRequest respects the request method", {
    response <- with_mock(
      "httr::POST" = function(...) "I did a POST!",
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      MakeDataRobotRequest(httr::POST, "lol", stopOnError = FALSE)
    )
    expect_equal(response, "I did a POST!")
  })

  test_that("MakeDataRobotRequest passes args", {
    args <- with_mock(
      "httr::POST" = function(...) list(...),
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      MakeDataRobotRequest(httr::POST, "lol", stopOnError = FALSE)
    )
    expect_equal(args$url, UrlJoin(fakeEndpoint, "lol"))
    expect_equal(args$config$headers[["Authorization"]], paste("Token", fakeToken))
    expect_equal(args$config$headers[["User-Agent"]], DataRobotGetDefaultHeader())
  })

  test_that("MakeDataRobotRequest respects encode", {
    args <- with_mock(
      "httr::POST" = function(...) list(...),
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      MakeDataRobotRequest(httr::POST,
        routeString = "lol",
        encode = "foo",
        stopOnError = FALSE
      )
    )
    expect_equal(args$encode, "foo")
  })

  test_that("MakeDataRobotRequest respects addURL = FALSE", {
    args <- with_mock(
      "httr::POST" = function(...) list(...),
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      MakeDataRobotRequest(httr::POST,
        routeString = "/lol",
        addUrl = FALSE,
        stopOnError = FALSE
      )
    )
    expect_equal(args$url, "/lol")
  })

  test_that("MakeDataRobotRequest can submit timeout", {
    expectedTimeoutInSeconds <- 2
    args <- with_mock(
      "httr::POST" = function(...) list(...),
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      MakeDataRobotRequest(httr::POST,
        routeString = "lol",
        timeout = expectedTimeoutInSeconds,
        stopOnError = FALSE
      )
    )
    actualTimeoutInMilliseconds <- args[[3]]$options$timeout_ms
    expect_equal(actualTimeoutInMilliseconds, expectedTimeoutInSeconds * 1000)
  })

  test_that("MakeDataRobotRequest can use body parameters", {
    args <- with_mock(
      "httr::POST" = function(...) list(...),
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      MakeDataRobotRequest(httr::POST,
        routeString = "lol",
        body = list(param = 1),
        stopOnError = FALSE
      )
    )
    expect_equal(args$body, list(param = 1))
  })

  test_that("MakeDataRobotRequest can use query parameters", {
    args <- with_mock(
      "httr::GET" = function(...) list(...),
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      MakeDataRobotRequest(httr::GET,
        routeString = "lol",
        query = list(param = 1),
        stopOnError = FALSE
      )
    )
    expect_equal(args$query, list(param = 1))
  })

  test_that("MakeDataRobotRequest can submit an intentional NULL parameter", {
    args <- with_mock(
      "httr::POST" = function(...) list(...),
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      MakeDataRobotRequest(httr::POST,
        routeString = "lol",
        body = list(param = 1, intentionalNull = NULL),
        stopOnError = FALSE
      )
    )
    expect_equal(jsonlite::fromJSON(args$body), list(param = 1, intentionalNull = NULL))
  })

  test_that("MakeDataRobotRequest simplifies the dataframe", {
    getStub <- stub(httr::GET)
    projectResponse <- httr:::response(
      url = "url",
      status_code = 200L,
      content = charToRaw('{"fish": [{"number": 1,
                                                                       "color": "red"},
                                                                       {"number": 2,
                                                                       "color": "blue"}]}')
    )
    getStub$onCall(1)$returns(projectResponse)
    response <- with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      MakeDataRobotRequest(httr::GET,
        routeString = "lol",
        returnRawResponse = FALSE,
        simplifyDataFrame = TRUE
      )
    )
    expect_type(response, "list")
    expect_s3_class(response$fish, "data.frame")
    ExpectHasKeys(response$fish, c("number", "color"))
  })

  test_that("MakeDataRobotRequest can avoid simplifying the dataframe", {
    getStub <- stub(httr::GET)
    projectResponse <- httr:::response(
      url = "url",
      status_code = 200L,
      content = charToRaw('{"fish": [{"number": 1,
                                                                       "color": "red"},
                                                                       {"number": 2,
                                                                       "color": "blue"}]}')
    )
    getStub$onCall(1)$returns(projectResponse)
    response <- with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      MakeDataRobotRequest(httr::GET,
        routeString = "lol",
        returnRawResponse = FALSE,
        simplifyDataFrame = FALSE
      )
    )
    expect_type(response, "list")
    expect_type(response$fish, "list")
    ExpectHasKeys(response$fish[[1]], c("number", "color"))
  })

  test_that("MakeDataRobotRequest can trigger dataRobotCurlDebugMode", {
    originalDebugMode <- getOption("dataRobotCurlDebugMode")
    options(dataRobotCurlDebugMode = TRUE)
    args <- with_mock(
      "httr::GET" = function(...) list(...),
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      MakeDataRobotRequest(httr::GET,
        routeString = "lol",
        stopOnError = FALSE
      )
    )
    expect_true(any(unlist(lapply(
      args,
      function(a) {
        isTRUE(try(a$options$verbose, silent = TRUE))
      }
    ))))
    options(dataRobotCurlDebugMode = FALSE)
    args <- with_mock(
      "httr::GET" = function(...) list(...),
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      MakeDataRobotRequest(httr::GET,
        routeString = "lol",
        stopOnError = FALSE
      )
    )
    expect_false(any(unlist(lapply(
      args,
      function(a) {
        isTRUE(try(a$options$verbose, silent = TRUE))
      }
    ))))
    options(dataRobotCurlDebugMode = originalDebugMode)
  })

  test_that("MakeDataRobotRequest can followLocation", {
    args <- with_mock(
      "httr::GET" = function(...) list(...),
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      MakeDataRobotRequest(httr::GET,
        routeString = "lol",
        stopOnError = FALSE,
        followLocation = TRUE
      )
    )
    expect_false(any(unlist(lapply(
      args,
      function(a) {
        try(a$options$followlocation, silent = TRUE) == 0
      }
    ))))
  })

  test_that("MakeDataRobotRequest can ignore followLocation", {
    args <- with_mock(
      "httr::GET" = function(...) list(...),
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      MakeDataRobotRequest(httr::GET,
        routeString = "lol",
        stopOnError = FALSE,
        followLocation = FALSE
      )
    )
    expect_true(any(unlist(lapply(
      args,
      function(a) {
        try(a$options$followlocation, silent = TRUE) == 0
      }
    ))))
  })

  test_that("MakeDataRobotRequest can download a file", {
    args <- with_mock(
      "httr::GET" = function(...) list(...),
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      MakeDataRobotRequest(httr::GET,
        routeString = "lol",
        stopOnError = FALSE,
        as = "file",
        filename = "cat_pic.png"
      )
    )
    args <- Filter(
      Negate(is.null),
      lapply(args, function(a) {
        tryCatch(a$output, error = function(e) NULL)
      })
    )
    expect_equal(length(args), 1)
    args <- args[[1]]
    expect_s3_class(args, "write_function")
    expect_s3_class(args, "write_disk")
    expect_equal(args$path, "cat_pic.png")
  })

  test_that("MakeDataRobotRequest can stop on an error", {
    getStub <- stub(httr::GET)
    projectResponse <- httr:::response(
      url = "url",
      status_code = 422L,
      content = charToRaw('{"fish": 1}')
    )
    getStub$onCall(1)$returns(projectResponse)
    expect_error(with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      MakeDataRobotRequest(httr::GET,
        routeString = "lol",
        stopOnError = TRUE
      )
    ), "422")
  })

  test_that("MakeDataRobotRequest can ignore stop on an error", {
    getStub <- stub(httr::GET)
    projectResponse <- httr:::response(
      url = "url",
      status_code = 422L,
      content = charToRaw('{"fish": 1}')
    )
    getStub$onCall(1)$returns(projectResponse)
    response <- with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      MakeDataRobotRequest(httr::GET,
        routeString = "lol",
        stopOnError = FALSE
      )
    )
    expect_s3_class(response, "response")
    expect_equal(httr::status_code(response), 422)
  })
})
