# Copyright 2022 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
library(testthat)
library(withr)

describe("CallApi", {
  # testthat v3 deprecates use of with_mock and local_mock
  # and there is no way without importing a different mockery
  # other than subclassing the R6 class and rewriting the
  # methods to be mocked
  ApiClientMock <- R6::R6Class(
    "ApiClientMock",
    inherit = ApiClient,
    public = list(
      Execute = function(url, method, queryParams, headerParams, body, encode = "raw", ...) {
        # Execute is called by CallApi
        httr:::response(
          url = url,
          status_code = 200L,
          headers = list("Deprecation" = "true")
        )
      }
    )
  )

  it("will issue a warning if the Deprecation header is found in a response", {
    client <- ApiClientMock$new()
    testthat::expect_warning(
      client$CallApi("/hello", "GET", c(), c(), body = NULL),
      regexp = "The resource you are trying to access will be or is deprecated."
    )
  })
})

describe("Py2DeprecationUrl function", {
  client <- ApiClient$new()

  it("will extract the domain from the basePath", {
    # this is expected behavior for all of the SDKs
    client$basePath <- "https://test.datarobot.com/api/v2"
    expect_equal(
      "https://test.datarobot.com/docs/release/deprecations-and-migrations/python2.html",
      client$.__enclos_env__$private$Py2DeprecationUrl()
    )
  })

  it("will work even if basePath has a trailing slash", {
    # this is a common mistake in DR configuration
    client$basePath <- "https://test.datarobot.com/api/v2/"
    expect_equal(
      "https://test.datarobot.com/docs/release/deprecations-and-migrations/python2.html",
      client$.__enclos_env__$private$Py2DeprecationUrl()
    )
  })

  it("will work if basePath is not datarobot.com", {
    client$basePath <- "https://datarobot.mytest.org/api/v2"
    expect_equal(
      "https://datarobot.mytest.org/docs/release/deprecations-and-migrations/python2.html",
      client$.__enclos_env__$private$Py2DeprecationUrl()
    )
  })
})

describe("ResponseHasDeprecationHeader function", {
  # These tests are based on the syntax in expired IETF draft doc
  # https://tools.ietf.org/id/draft-dalal-deprecation-header-03.html#syntax
  apiClient <- ApiClient$new()
  # for convenience sake
  ResponseHasDeprecationHeader <- apiClient$.__enclos_env__$private$ResponseHasDeprecationHeader

  it("handles header with simple string true", {
    resp <- httr:::response(
      headers = list("Deprecation" = "true")
    )

    expect_true(ResponseHasDeprecationHeader(resp))
  })

  it("handles header with IMF-fixdate timestamp", {
    resp <- httr:::response(
      headers = list("Deprecation" = "Sun, 11 Nov 2018 23:59:59 GMT")
    )

    expect_true(ResponseHasDeprecationHeader(resp))
  })

  it("handles no header", {
    resp <- httr:::response()
    expect_false(ResponseHasDeprecationHeader(resp))
  })
})
