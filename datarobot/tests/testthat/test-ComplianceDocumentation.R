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

test_that("it can create compliance documentation", {
  postStub <- stub(httr::POST)
  createComplianceDocUrl <- UrlJoin(projectUrl, "models", fakeModelId, "complianceDocs")
  createComplianceDocResponse <- httr:::response(
    url = createComplianceDocUrl,
    status_code = 202L,
    content = raw(0)
  )
  postStub$onCall(1)$returns(createComplianceDocResponse)
  response <- with_mock(
    `httr::POST` = postStub$f,
    `httr::GET` = function() stop("Should not be called!"),
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
    `datarobot:::Endpoint` = function() fakeEndpoint,
    `datarobot:::Token` = function() fakeToken,
    `datarobot:::JobIdFromResponse` = identity,
    CreateComplianceDocumentation(fakeModel)
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_equal(bodyForInspect, list())
})

test_that("it can create compliance documentation from a templateID", {
  postStub <- stub(httr::POST)
  createComplianceDocUrl <- UrlJoin(projectUrl, "models", fakeModelId, "complianceDocs")
  createComplianceDocResponse <- httr:::response(
    url = createComplianceDocUrl,
    status_code = 202L,
    content = raw(0)
  )
  postStub$onCall(1)$returns(createComplianceDocResponse)
  response <- with_mock(
    `httr::POST` = postStub$f,
    `httr::GET` = function() stop("Should not be called!"),
    "datarobot:::DataRobotPOST" = function(routeString,
                                           addUrl = TRUE,
                                           body = NULL,
                                           returnRawResponse = FALSE, ...) {
      bodyForInspect <<- body
      datarobot:::MakeDataRobotRequest(httr::POST, routeString,
        addUrl = TRUE,
        returnRawResponse = returnRawResponse,
        body = body, ...
      )
    },
    `datarobot:::Endpoint` = function() fakeEndpoint,
    `datarobot:::Token` = function() fakeToken,
    `datarobot:::JobIdFromResponse` = identity,
    CreateComplianceDocumentation(fakeModel, templateId = fakeTemplateId)
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_equal(bodyForInspect, list(templateId = fakeTemplateId))
})


test_that("it can download compliance documentation", {
  getStub <- stub(httr::GET)
  getComplianceDocUrl <- UrlJoin(projectUrl, "models", fakeModelId, "complianceDocs")
  getComplianceDocTxt <- fileToChar("responses/downloadComplianceDoc.json")
  getComplianceDocResponse <- httr:::response(
    url = getComplianceDocUrl,
    status_code = 200L,
    content = charToRaw(getComplianceDocTxt)
  )
  getStub$onCall(1)$returns(getComplianceDocResponse)
  with_mock(
    `httr::POST` = function() stop("Should not be called!"),
    `httr::GET` = getStub$f,
    "datarobot:::DataRobotGET" = function(routeString,
                                          addUrl = TRUE,
                                          body = NULL,
                                          returnRawResponse = FALSE, ...) {
      paramsForInspect <<- list(...)
      datarobot:::MakeDataRobotRequest(httr::GET, routeString,
        addUrl = addUrl,
        returnRawResponse = returnRawResponse,
        body = body, ...
      )
    },
    `datarobot:::Endpoint` = function() fakeEndpoint,
    `datarobot:::Token` = function() fakeToken,
    DownloadComplianceDocumentation(fakeModel, filename = fakeFilePath, create = FALSE)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_equal(paramsForInspect$as, "file")
  expect_equal(paramsForInspect$filename, fakeFilePath)
})

test_that("it can download compliance docs, creating in the process", {
  postStub <- stub(httr::POST)
  complianceDocUrl <- UrlJoin(projectUrl, "models", fakeModelId, "complianceDocs")
  createComplianceDocResponse <- httr:::response(
    url = complianceDocUrl,
    status_code = 202L,
    content = raw(0)
  )
  postStub$onCall(1)$returns(createComplianceDocResponse)
  getStub <- stub(httr::GET)
  getComplianceDocTxt <- fileToChar("responses/downloadComplianceDoc.json")
  getComplianceDocResponse <- httr:::response(
    url = complianceDocUrl,
    status_code = 200L,
    content = charToRaw(getComplianceDocTxt)
  )
  getStub$onCall(1)$returns(getComplianceDocResponse)
  with_mock(
    `httr::POST` = postStub$f,
    `httr::GET` = getStub$f,
    "datarobot:::DataRobotGET" = function(routeString,
                                          addUrl = TRUE,
                                          body = NULL,
                                          returnRawResponse = FALSE, ...) {
      paramsForInspect <<- list(...)
      datarobot:::MakeDataRobotRequest(httr::GET, routeString,
        addUrl = addUrl,
        returnRawResponse = returnRawResponse,
        body = body, ...
      )
    },
    `datarobot:::Endpoint` = function() fakeEndpoint,
    `datarobot:::Token` = function() fakeToken,
    `datarobot:::JobIdFromResponse` = identity,
    `datarobot:::WaitForJobToComplete` = function(...) "NOOP",
    DownloadComplianceDocumentation(fakeModel, filename = fakeFilePath)
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_equal(getStub$calledTimes(), 1)
  expect_equal(paramsForInspect$as, "file")
  expect_equal(paramsForInspect$filename, fakeFilePath)
})


test_that("it can list compliance doc templates", {
  getStub <- stub(httr::GET)
  listCDocTemplateUrl <- UrlJoin(fakeEndpoint, "complianceDocTemplates")
  listCDocTemplateJson <- fileToChar("responses/listComplianceDocTemplates.json")
  listCDocTemplateResponse <- httr:::response(
    url = listCDocTemplateUrl,
    status_code = 200L,
    content = charToRaw(listCDocTemplateJson)
  )
  getStub$onCall(1)$returns(listCDocTemplateResponse)
  templates <- with_mock(
    `httr::GET` = getStub$f,
    "datarobot:::DataRobotGET" = function(routeString,
                                          addUrl = TRUE,
                                          body = NULL,
                                          returnRawResponse = FALSE, ...) {
      paramsForInspect <<- list(...)
      datarobot:::MakeDataRobotRequest(httr::GET, routeString,
        addUrl = addUrl,
        returnRawResponse = returnRawResponse,
        body = body, ...
      )
    },
    `datarobot:::Endpoint` = function() fakeEndpoint,
    `datarobot:::Token` = function() fakeToken,
    ListComplianceDocTemplates()
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(templates, "listOfComplianceDocTemplates")
  expect_true(length(templates) > 1)
  expect_equal(paramsForInspect$query$orderBy, "id")
  expect_false(paramsForInspect$simplifyDataFrame)
})

test_that("it can list compliance doc templates with limit, offset, and namePart", {
  getStub <- stub(httr::GET)
  listCDocTemplateUrl <- UrlJoin(fakeEndpoint, "complianceDocTemplates")
  listCDocTemplateJson <- fileToChar("responses/listComplianceDocTemplates.json")
  listCDocTemplateResponse <- httr:::response(
    url = listCDocTemplateUrl,
    status_code = 200L,
    content = charToRaw(listCDocTemplateJson)
  )
  getStub$onCall(1)$returns(listCDocTemplateResponse)
  templates <- with_mock(
    `httr::GET` = getStub$f,
    "datarobot:::DataRobotGET" = function(routeString,
                                          addUrl = TRUE,
                                          body = NULL,
                                          returnRawResponse = FALSE, ...) {
      paramsForInspect <<- list(...)
      datarobot:::MakeDataRobotRequest(httr::GET, routeString,
        addUrl = addUrl,
        returnRawResponse = returnRawResponse,
        body = body, ...
      )
    },
    `datarobot:::Endpoint` = function() fakeEndpoint,
    `datarobot:::Token` = function() fakeToken,
    ListComplianceDocTemplates(namePart = "foo", limit = 3, offset = 1)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(templates, "listOfComplianceDocTemplates")
  expect_true(length(templates) > 1)
  expect_equal(paramsForInspect$query$namePart, "foo")
  expect_equal(paramsForInspect$query$limit, 3)
  expect_equal(paramsForInspect$query$offset, 1)
  expect_false(paramsForInspect$simplifyDataFrame)
})


test_that("it can get the default compliance doc template", {
  getStub <- stub(httr::GET)
  getCDocTemplateUrl <- UrlJoin(fakeEndpoint, "complianceDocTemplates", "default")
  getCDocTemplateJson <- fileToChar("responses/getComplianceDocTemplate.json")
  getCDocTemplateResponse <- httr:::response(
    url = getCDocTemplateUrl,
    status_code = 200L,
    content = charToRaw(getCDocTemplateJson)
  )
  getStub$onCall(1)$returns(getCDocTemplateResponse)
  template <- with_mock(
    `httr::GET` = getStub$f,
    "datarobot:::DataRobotGET" = function(routeString,
                                          addUrl = TRUE,
                                          body = NULL,
                                          returnRawResponse = FALSE, ...) {
      paramsForInspect <<- list(...)
      datarobot:::MakeDataRobotRequest(httr::GET, routeString,
        addUrl = addUrl,
        returnRawResponse = returnRawResponse,
        body = body, ...
      )
    },
    `datarobot:::Endpoint` = function() fakeEndpoint,
    `datarobot:::Token` = function() fakeToken,
    GetComplianceDocTemplate()
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(template, "dataRobotComplianceDocTemplate")
  expect_equal(template$name, "Default")
  expect_type(template$sections, "list")
  expect_equal(paramsForInspect$query, list())
  expect_false(paramsForInspect$simplifyDataFrame)
})

test_that("it can get the default time series compliance doc template", {
  getStub <- stub(httr::GET)
  getCDocTemplateUrl <- UrlJoin(fakeEndpoint, "complianceDocTemplates", "default")
  getCDocTemplateJson <- fileToChar("responses/getComplianceDocTemplate.json")
  getCDocTemplateResponse <- httr:::response(
    url = getCDocTemplateUrl,
    status_code = 200L,
    content = charToRaw(getCDocTemplateJson)
  )
  getStub$onCall(1)$returns(getCDocTemplateResponse)
  template <- with_mock(
    `httr::GET` = getStub$f,
    "datarobot:::DataRobotGET" = function(routeString,
                                          addUrl = TRUE,
                                          body = NULL,
                                          returnRawResponse = FALSE, ...) {
      paramsForInspect <<- list(...)
      datarobot:::MakeDataRobotRequest(httr::GET, routeString,
        addUrl = addUrl,
        returnRawResponse = returnRawResponse,
        body = body, ...
      )
    },
    `datarobot:::Endpoint` = function() fakeEndpoint,
    `datarobot:::Token` = function() fakeToken,
    GetComplianceDocTemplate(type = "timeSeries")
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(template, "dataRobotComplianceDocTemplate")
  expect_equal(template$name, "Default")
  expect_type(template$sections, "list")
  expect_equal(paramsForInspect$query, list(type = "timeSeries"))
  expect_false(paramsForInspect$simplifyDataFrame)
})

test_that("it can get a custom compliance doc template", {
  getStub <- stub(httr::GET)
  getCDocTemplateUrl <- UrlJoin(fakeEndpoint, "complianceDocTemplates", fakeTemplateId)
  getCDocTemplateJson <- fileToChar("responses/getComplianceDocTemplate.json")
  getCDocTemplateResponse <- httr:::response(
    url = getCDocTemplateUrl,
    status_code = 200L,
    content = charToRaw(getCDocTemplateJson)
  )
  getStub$onCall(1)$returns(getCDocTemplateResponse)
  template <- with_mock(
    `httr::GET` = getStub$f,
    "datarobot:::DataRobotGET" = function(routeString,
                                          addUrl = TRUE,
                                          body = NULL,
                                          returnRawResponse = FALSE, ...) {
      paramsForInspect <<- list(...)
      datarobot:::MakeDataRobotRequest(httr::GET, routeString,
        addUrl = addUrl,
        returnRawResponse = returnRawResponse,
        body = body, ...
      )
    },
    `datarobot:::Endpoint` = function() fakeEndpoint,
    `datarobot:::Token` = function() fakeToken,
    GetComplianceDocTemplate(fakeTemplateId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(template, "dataRobotComplianceDocTemplate")
  expect_type(template$sections, "list")
  expect_equal(paramsForInspect$query, list())
  expect_false(paramsForInspect$simplifyDataFrame)
})


test_that("it can download the default compliance doc template", {
  getStub <- stub(httr::GET)
  getCDocTemplateUrl <- UrlJoin(fakeEndpoint, "complianceDocTemplates", "default")
  getCDocTemplateJson <- fileToChar("responses/getComplianceDocTemplate.json")
  getCDocTemplateResponse <- httr:::response(
    url = getCDocTemplateUrl,
    status_code = 200L,
    content = charToRaw(getCDocTemplateJson)
  )
  getStub$onCall(1)$returns(getCDocTemplateResponse)
  with_mock(
    `httr::GET` = getStub$f,
    "datarobot:::DataRobotGET" = function(routeString,
                                          addUrl = TRUE,
                                          body = NULL,
                                          returnRawResponse = FALSE, ...) {
      paramsForInspect <<- list(...)
      datarobot:::MakeDataRobotRequest(httr::GET, routeString,
        addUrl = addUrl,
        returnRawResponse = returnRawResponse,
        body = body, ...
      )
    },
    `datarobot:::Endpoint` = function() fakeEndpoint,
    `datarobot:::Token` = function() fakeToken,
    DownloadComplianceDocTemplate(fakeFilePath)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_equal(paramsForInspect$query, list())
  expect_equal(paramsForInspect$as, "file")
  expect_equal(paramsForInspect$filename, fakeFilePath)
})

test_that("it can download the default time series compliance doc template", {
  getStub <- stub(httr::GET)
  getCDocTemplateUrl <- UrlJoin(fakeEndpoint, "complianceDocTemplates", "default")
  getCDocTemplateJson <- fileToChar("responses/getComplianceDocTemplate.json")
  getCDocTemplateResponse <- httr:::response(
    url = getCDocTemplateUrl,
    status_code = 200L,
    content = charToRaw(getCDocTemplateJson)
  )
  getStub$onCall(1)$returns(getCDocTemplateResponse)
  with_mock(
    `httr::GET` = getStub$f,
    "datarobot:::DataRobotGET" = function(routeString,
                                          addUrl = TRUE,
                                          body = NULL,
                                          returnRawResponse = FALSE, ...) {
      paramsForInspect <<- list(...)
      datarobot:::MakeDataRobotRequest(httr::GET, routeString,
        addUrl = addUrl,
        returnRawResponse = returnRawResponse,
        body = body, ...
      )
    },
    `datarobot:::Endpoint` = function() fakeEndpoint,
    `datarobot:::Token` = function() fakeToken,
    DownloadComplianceDocTemplate(fakeFilePath, type = "timeSeries")
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_equal(paramsForInspect$query, list(type = "timeSeries"))
  expect_equal(paramsForInspect$as, "file")
  expect_equal(paramsForInspect$filename, fakeFilePath)
})

test_that("it can download a custom compliance doc template", {
  getStub <- stub(httr::GET)
  getCDocTemplateUrl <- UrlJoin(fakeEndpoint, "complianceDocTemplates", fakeTemplateId)
  getCDocTemplateJson <- fileToChar("responses/getComplianceDocTemplate.json")
  getCDocTemplateResponse <- httr:::response(
    url = getCDocTemplateUrl,
    status_code = 200L,
    content = charToRaw(getCDocTemplateJson)
  )
  getStub$onCall(1)$returns(getCDocTemplateResponse)
  with_mock(
    `httr::GET` = getStub$f,
    "datarobot:::DataRobotGET" = function(routeString,
                                          addUrl = TRUE,
                                          body = NULL,
                                          returnRawResponse = FALSE, ...) {
      paramsForInspect <<- list(...)
      datarobot:::MakeDataRobotRequest(httr::GET, routeString,
        addUrl = addUrl,
        returnRawResponse = returnRawResponse,
        body = body, ...
      )
    },
    `datarobot:::Endpoint` = function() fakeEndpoint,
    `datarobot:::Token` = function() fakeToken,
    DownloadComplianceDocTemplate(fakeFilePath, fakeTemplateId)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_equal(paramsForInspect$query, list())
  expect_equal(paramsForInspect$as, "file")
  expect_equal(paramsForInspect$filename, fakeFilePath)
})

test_that("it can download a custom compliance doc template from an object", {
  getStub <- stub(httr::GET)
  getCDocTemplateUrl <- UrlJoin(fakeEndpoint, "complianceDocTemplates", fakeTemplateId)
  getCDocTemplateJson <- fileToChar("responses/getComplianceDocTemplate.json")
  getCDocTemplateResponse <- httr:::response(
    url = getCDocTemplateUrl,
    status_code = 200L,
    content = charToRaw(getCDocTemplateJson)
  )
  getStub$onCall(1)$returns(getCDocTemplateResponse)
  with_mock(
    `httr::GET` = getStub$f,
    "datarobot:::DataRobotGET" = function(routeString,
                                          addUrl = TRUE,
                                          body = NULL,
                                          returnRawResponse = FALSE, ...) {
      paramsForInspect <<- list(...)
      datarobot:::MakeDataRobotRequest(httr::GET, routeString,
        addUrl = addUrl,
        returnRawResponse = returnRawResponse,
        body = body, ...
      )
    },
    `datarobot:::Endpoint` = function() fakeEndpoint,
    `datarobot:::Token` = function() fakeToken,
    DownloadComplianceDocTemplate(fakeFilePath, fakeTemplate)
  )
  expect_equal(getStub$calledTimes(), 1)
  expect_equal(paramsForInspect$query, list())
  expect_equal(paramsForInspect$as, "file")
  expect_equal(paramsForInspect$filename, fakeFilePath)
})


test_that("it can upload a custom compliance documentation template from filename", {
  postStub <- stub(httr::POST)
  uploadCDocTemplateUrl <- UrlJoin(fakeEndpoint, "complianceDocTemplates")
  uploadCDocTemplateResponse <- httr:::response(
    url = uploadCDocTemplateUrl,
    status_code = 202L,
    content = raw(0)
  )
  postStub$onCall(1)$returns(uploadCDocTemplateResponse)
  with_mock(
    `httr::POST` = postStub$f,
    `httr::GET` = function() stop("Should not be called!"),
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
    `jsonlite::fromJSON` = function(...) {
      list(sections = "mock data")
    },
    `datarobot:::Endpoint` = function() fakeEndpoint,
    `datarobot:::Token` = function() fakeToken,
    UploadComplianceDocTemplate("test", fakeFilePath)
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_equal(bodyForInspect$name, "test")
  expect_equal(bodyForInspect$sections, "mock data")
})

test_that("it can upload a custom compliance documentation template from sections", {
  postStub <- stub(httr::POST)
  uploadCDocTemplateUrl <- UrlJoin(fakeEndpoint, "complianceDocTemplates")
  uploadCDocTemplateResponse <- httr:::response(
    url = uploadCDocTemplateUrl,
    status_code = 202L,
    content = raw(0)
  )
  postStub$onCall(1)$returns(uploadCDocTemplateResponse)
  sections <- list("mock data")
  with_mock(
    `httr::POST` = postStub$f,
    `httr::GET` = function() stop("Should not be called!"),
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
    `jsonlite::fromJSON` = function(...) {
      list(sections = "mock data")
    },
    `datarobot:::Endpoint` = function() fakeEndpoint,
    `datarobot:::Token` = function() fakeToken,
    UploadComplianceDocTemplate("test", sections = sections)
  )
  expect_equal(postStub$calledTimes(), 1)
  expect_equal(bodyForInspect$name, "test")
  expect_equal(bodyForInspect$sections, list("mock data"))
})


test_that("it can update a compliance documentation template", {
  patchStub <- stub(httr::PATCH)
  updateCDocTemplateUrl <- UrlJoin(fakeEndpoint, "complianceDocTemplates", fakeTemplateId)
  updateCDocTemplateResponse <- httr:::response(
    url = updateCDocTemplateUrl,
    status_code = 202L,
    content = raw(0)
  )
  patchStub$onCall(1)$returns(updateCDocTemplateResponse)
  getStub <- stub(httr::GET)
  getCDocTemplateUrl <- UrlJoin(fakeEndpoint, "complianceDocTemplates", fakeTemplateId)
  getCDocTemplateJson <- fileToChar("responses/getComplianceDocTemplate.json")
  getCDocTemplateResponse <- httr:::response(
    url = getCDocTemplateUrl,
    status_code = 200L,
    content = charToRaw(getCDocTemplateJson)
  )
  getStub$onCall(1)$returns(getCDocTemplateResponse)
  template <- with_mock(
    `httr::PATCH` = patchStub$f,
    `httr::GET` = getStub$f,
    `httr::POST` = function() stop("Should not be called!"),
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
    `datarobot:::Endpoint` = function() fakeEndpoint,
    `datarobot:::Token` = function() fakeToken,
    UpdateComplianceDocTemplate(fakeTemplateId,
      name = "new name",
      sections = list("new sections")
    )
  )
  expect_equal(patchStub$calledTimes(), 1)
  expect_equal(getStub$calledTimes(), 1)
  expect_s3_class(template, "dataRobotComplianceDocTemplate")
  expect_equal(bodyForInspect$name, "new name")
  expect_equal(bodyForInspect$sections, list("new sections"))
})


test_that("it can delete a compliance documentation template", {
  deleteStub <- stub(httr::DELETE)
  deleteCDocTemplateUrl <- UrlJoin(fakeEndpoint, "complianceDocTemplates", fakeTemplateId)
  deleteCDocTemplateResponse <- httr:::response(
    url = deleteCDocTemplateUrl,
    status_code = 204L,
    content = raw(0)
  )
  deleteStub$onCall(1)$returns(deleteCDocTemplateResponse)
  with_mock(
    `httr::DELETE` = deleteStub$f,
    `datarobot:::Endpoint` = function() fakeEndpoint,
    `datarobot:::Token` = function() fakeToken,
    DeleteComplianceDocTemplate(fakeTemplateId)
  )
  expect_equal(deleteStub$calledTimes(), 1)
})
