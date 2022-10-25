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


blueprintId <- "dummy_blueprint_Id"
modelId <- "dummy_model_Id"
blueprintUrl <- UrlJoin(projectUrl, "blueprints", blueprintId)

blueprintListJson <- fileToChar("responses/ListBlueprints.json")
blueprintListResponse <- httr:::response(
  url = blueprintUrl,
  status_code = 200L,
  content = charToRaw(blueprintListJson)
)
emptyBlueprintListResponse <- httr:::response(
  url = blueprintUrl,
  status_code = 200L,
  content = charToRaw("[]")
)
expectedKeys <- c(
  "projectId", "processes", "blueprintId", "modelType", "blueprintCategory",
  "supportsMonotonicConstraints", "monotonicIncreasingFeaturelistId",
  "monotonicDecreasingFeaturelistId", "recommendedFeaturelistId"
)

test_that("ListBlueprints succeeds with a fake project", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(blueprintListResponse)
  blueprints <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListBlueprints(fakeProject)
  )
  expect_s3_class(blueprints, "listOfBlueprints")
  ExpectHasKeys(blueprints[[1]], expectedKeys)
})

test_that("ListBlueprints succeeds with a fake project ID", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(blueprintListResponse)
  blueprints <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    ListBlueprints(fakeProjectId)
  )
  expect_s3_class(blueprints, "listOfBlueprints")
  ExpectHasKeys(blueprints[[1]], expectedKeys)
})


describe("Blueprint summary", {
  test_that("ListBlueprints can be summarized", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(blueprintListResponse)
    blueprintSummary <- with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      summary(ListBlueprints(fakeProjectId))
    )
    expect_type(blueprintSummary, "list")
    ExpectHasKeys(blueprintSummary, c("generalSummary", "detailedSummary"))
    expect_type(blueprintSummary$generalSummary, "character")
    expect_s3_class(blueprintSummary$detailedSummary, "data.frame")
    ExpectHasKeys(
      blueprintSummary$detailedSummary,
      c("modelType", "expandedModel", "blueprintId")
    )
    expect_equal(nrow(blueprintSummary$detailedSummary), 6)
  })

  test_that("ListBlueprints can be summarized with nList", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(blueprintListResponse)
    blueprintSummary <- with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      summary(ListBlueprints(fakeProjectId), nList = 10)
    )
    expect_equal(nrow(blueprintSummary$detailedSummary), 10)
  })

  test_that("ListBlueprints can be summarized with nList > blueprint count", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(blueprintListResponse)
    blueprintSummary <- with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      summary(ListBlueprints(fakeProjectId), nList = 1000)
    )
    expect_equal(nrow(blueprintSummary$detailedSummary), 95)
  })

  test_that("ListBlueprints can be summarized when there are no blueprints", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(emptyBlueprintListResponse)
    blueprintSummary <- with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      summary(ListBlueprints(fakeProjectId))
    )
    expect_equal(blueprintSummary, list())
  })
})


describe("Blueprint as.data.frame", {
  test_that("ListBlueprints can be converted to data.frame", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(blueprintListResponse)
    blueprintDf <- with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      as.data.frame(ListBlueprints(fakeProjectId))
    )
    expect_s3_class(blueprintDf, "data.frame")
    expect_equal(nrow(blueprintDf), 95)
    ExpectHasKeys(blueprintDf, c("projectId", "modelType", "expandedModel", "blueprintId"))
    expect_equal(row.names(blueprintDf), as.character(seq(95)))
  })

  test_that("ListBlueprints can be converted to data.frame with row.names", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(blueprintListResponse)
    newRowNames <- paste0("NewName", seq(95))
    blueprintDf <- with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      as.data.frame(ListBlueprints(fakeProjectId),
        row.names = newRowNames
      )
    )
    expect_equal(row.names(blueprintDf), newRowNames)
  })

  test_that("ListBlueprints can be converted to data.frame when there are no blueprints", {
    getStub <- stub(httr::GET)
    getStub$onCall(1)$returns(emptyBlueprintListResponse)
    blueprintDf <- with_mock(
      "httr::GET" = getStub$f,
      "datarobot:::Endpoint" = function() fakeEndpoint,
      "datarobot:::Token" = function() fakeToken,
      as.data.frame(ListBlueprints(fakeProjectId))
    )
    expect_s3_class(blueprintDf, "data.frame")
    expect_equal(nrow(blueprintDf), 0)
    ExpectHasKeys(blueprintDf, c("projectId", "modelType", "expandedModel", "blueprintId"))
  })
})


blueprintOneJson <- fileToChar("responses/blueprintOne.json")
blueprintOneResponse <- httr:::response(
  url = blueprintUrl,
  status_code = 200L,
  content = charToRaw(blueprintOneJson)
)
test_that("GetBlueprint succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(blueprintOneResponse)
  blueprint <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetBlueprint(fakeProject, blueprintId)
  )
  expect_type(blueprint, "list")
  ExpectHasKeys(blueprint, expectedKeys)
})

blueprintChartUrl <- UrlJoin(projectUrl, "blueprints", blueprintId, "blueprintChart")
blueprintChartJson <- fileToChar("responses/blueprintChart.json")
blueprintChartResponse <- httr:::response(
  url = blueprintChartUrl,
  status_code = 200L,
  content = charToRaw(blueprintChartJson)
)
test_that("GetBlueprintChart succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(blueprintChartResponse)
  chartReturn <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetBlueprintChart(fakeProject, blueprintId)
  )
  expect_type(chartReturn, "list")
  ExpectHasKeys(chartReturn, c("nodes", "edges"))
  graphviz <- BlueprintChartToGraphviz(chartReturn)
  expect_type(graphviz, "character")
})


blueprintDoctUrl <- UrlJoin(projectUrl, "blueprints", blueprintId, "blueprintDocs")
blueprintDocJson <- fileToChar("responses/blueprintDoc.json")
blueprintDocResponse <- httr:::response(
  url = blueprintDoctUrl,
  status_code = 200L,
  content = charToRaw(blueprintDocJson)
)
test_that("GetBlueprintDocumentation succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(blueprintDocResponse)
  docReturn <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetBlueprintDocumentation(fakeProject, blueprintId)
  )
  expect_type(docReturn, "list")
  expect_true(length(docReturn) > 0)
  ExpectHasKeys(docReturn[[1]], c(
    "task", "description", "title", "parameters",
    "references", "links"
  ))
})

blueprintChartUrl <- UrlJoin(projectUrl, "blueprints", blueprintId, "blueprintChart")
blueprintChartJson <- fileToChar("responses/modelBlueprintChart.json")
blueprintChartResponse <- httr:::response(
  url = blueprintChartUrl,
  status_code = 200L,
  content = charToRaw(blueprintChartJson)
)
test_that("GetModelBlueprintChart succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(blueprintChartResponse)
  chartReturn <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetModelBlueprintChart(fakeProject, blueprintId)
  )
  expect_type(chartReturn, "list")
  ExpectHasKeys(chartReturn, c("nodes", "edges"))
  graphviz <- BlueprintChartToGraphviz(chartReturn)
  expect_type(graphviz, "character")
})


blueprintDoctUrl <- UrlJoin(projectUrl, "models", modelId, "blueprintDocs")
blueprintDocJson <- fileToChar("responses/modelBlueprintDoc.json")
blueprintDocResponse <- httr:::response(
  url = blueprintDoctUrl,
  status_code = 200L,
  content = charToRaw(blueprintDocJson)
)
test_that("GetModelBlueprintDocumentation succeeds", {
  getStub <- stub(httr::GET)
  getStub$onCall(1)$returns(blueprintDocResponse)
  docReturn <- with_mock(
    "httr::GET" = getStub$f,
    "datarobot:::Endpoint" = function() fakeEndpoint,
    "datarobot:::Token" = function() fakeToken,
    GetModelBlueprintDocumentation(fakeProject, blueprintId)
  )
  expect_type(docReturn, "list")
  expect_true(length(docReturn) > 0)
  ExpectHasKeys(docReturn[[1]], c(
    "task", "description", "title", "parameters",
    "references", "links"
  ))
})
