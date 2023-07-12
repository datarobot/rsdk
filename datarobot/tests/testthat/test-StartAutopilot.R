library(testthat)

# TODO: Add test that we fail appropriately when the status response includes a status
#       field that indicates failure.
# Note: That will require slightly reorganizing the tests, since at the moment
#       `withSetTargetMocks` expects the same assertions for each test.
withSetTargetMocks <- function(..., multiseriesMock = FALSE, crossSeriesGroupByMock = FALSE) {
  # Set up stubs for PATCH and GET. We expect PATCH is called once (to request setting the target),
  # and GET is called three times:
  #  (1) get status of the async request (not done yet)
  #  (2) get status of the async request (now we're done)
  #  (3) get updated project data (we don't use this for anything, but it's part of the async
  #      workflow, so it's expected to happen)
  #
  # Note: This does not include the GET request to confirm that the project status is 'aim'
  #       (ready for target-setting), since that is mocked separately via GetProjectStatus

  patchStub <- stub(httr::PATCH)
  getStub <- stub(httr::GET)

  aimUrl <- datarobot:::UrlJoin(projectUrl, "aim")
  statusUrl <- datarobot:::UrlJoin(fakeEndpoint, "status", "some-status")

  patchStub$onCall(1)$expects(url = aimUrl)
  patchStub$onCall(1)$returns(httr:::response(url = aimUrl,
                                              status_code = 202L,
                                              headers = list(location = statusUrl),
                                              content = raw(0)))

  getStub$onCall(1)$expects(url = statusUrl)
  getStub$onCall(1)$returns(httr:::response(url = statusUrl,
                                            status_code = 200L,
                                            content = charToRaw('{"status": "someStatus"}')))

  getStub$onCall(2)$expects(url = statusUrl)
  getStub$onCall(2)$returns(httr:::response(url = statusUrl,
                                            status_code = 303L,
                                            headers = list(location = projectUrl),
                                            content = raw(0)))

  if (isTRUE(multiseriesMock)) {
    getMultiseriesJson <- fileToChar("responses/GetMultiseries.json")
    getMultiseriesUrl <- UrlJoin(projectUrl, "multiseriesProperties")
    multiseriesRequestResponse <- httr:::response(url = getMultiseriesUrl,
                                                  status_code = 303L,
                                                  content = charToRaw(getMultiseriesJson))
    getStub$onCall(3)$returns(multiseriesRequestResponse)
    getStub$onCall(4)$expects(url = statusUrl)
    getStub$onCall(4)$returns(httr:::response(url = statusUrl,
                                              status_code = 303L,
                                              headers = list(location = projectUrl),
                                              content = raw(0)))
    if (isTRUE(crossSeriesGroupByMock)) {
      getCrossSeriesJson <- fileToChar("responses/GetCrossSeries.json")
      getCrossSeriesUrl <- UrlJoin(projectUrl, "crossSeriesProperties")
      crossSeriesRequestResponse <- httr:::response(url = getCrossSeriesUrl,
                                                    status_code = 303L,
                                                    content = charToRaw(getCrossSeriesJson))
      getStub$onCall(5)$returns(crossSeriesRequestResponse)
      getStub$onCall(6)$expects(url = statusUrl)
      getStub$onCall(6)$returns(httr:::response(url = statusUrl,
                                                status_code = 303L,
                                                headers = list(location = projectUrl),
                                                content = raw(0)))
      getStub$onCall(7)$expects(url = projectUrl)
      getStub$onCall(7)$returns(httr:::response(url = projectUrl,
                                                status_code = 200L,
                                                content = raw(0)))
    } else {
      getStub$onCall(5)$expects(url = projectUrl)
      getStub$onCall(5)$returns(httr:::response(url = projectUrl,
                                                status_code = 200L,
                                                content = raw(0)))
    }
  }
  else {
    getStub$onCall(3)$expects(url = projectUrl)
    getStub$onCall(3)$returns(httr:::response(url = projectUrl,
                                              status_code = 200L,
                                              content = raw(0)))
  }

  postStub <- stub(httr::POST)
  postMultiseriesModelUrl <- UrlJoin(projectUrl, "multiseriesProperties")
  multiseriesRequestResponse <- httr:::response(url = postMultiseriesModelUrl,
                                                status_code = 303L,
                                                headers = list(location = statusUrl),
                                                content = raw(0))
  postStub$onCall(1)$returns(multiseriesRequestResponse)
  if (isTRUE(crossSeriesGroupByMock)) {
    crossSeriesRequestResponse <- httr:::response(url = getCrossSeriesUrl,
                                                  status_code = 303L,
                                                  headers = list(location = statusUrl),
                                                  content = charToRaw(getCrossSeriesJson))
    postStub$onCall(2)$returns(crossSeriesRequestResponse)
  }

  with_mock("httr::PATCH" = patchStub$f,
            "httr::POST" = postStub$f,
            "httr::GET" = getStub$f,
            # Mock patch to be able to record the input so we can test against it
            "datarobot:::DataRobotPATCH" = function(routeString,
                                                    addUrl = TRUE,
                                                    body = NULL,
                                                    returnRawResponse = FALSE, ...) {
              bodyForInspect <<- body
              datarobot:::MakeDataRobotRequest(httr::PATCH, routeString,
                                               addUrl = addUrl,
                                               returnRawResponse = returnRawResponse,
                                               body = body, ...)
            },
            "datarobot:::Endpoint" = function() fakeEndpoint,
            "datarobot:::Token" = function() fakeToken,
            GetProjectStatus = function(...) list(stage = ProjectStage$AIM),
            ...) # Tests get injected here.

  expect_equal(patchStub$calledTimes(), 1)

  if (isTRUE(crossSeriesGroupByMock)) {
    expect_equal(postStub$calledTimes(), 2)
    expect_equal(getStub$calledTimes(), 7)
  } else if (isTRUE(multiseriesMock)) {
    expect_equal(postStub$calledTimes(), 1)
    expect_equal(getStub$calledTimes(), 5)
  } else {
    expect_equal(getStub$calledTimes(), 3)
  }
}

test_that("Required parameters are present", {
  # TODO: Fix this.
  # (These don't use mocks and don't look for specific errors, so are likely to spuriously passs.)
  expect_error(SetTarget())
  expect_error(SetTarget(target = fakeTarget))
  expect_error(SetTarget(fakeProject, target = NULL))
  expect_error(SetTarget(fakeProject, target = fakeTarget, majorityDownsamplingRate = 0.9))
})


test_that("Use projectId only", {
  withSetTargetMocks({
    expect_message(SetTarget(project = fakeProjectId, target = fakeTarget),
    "Autopilot started")
    expect_equal(as.character(bodyForInspect$target), fakeTarget)
  })
})

test_that("Use full project list only", {
  withSetTargetMocks({
    expect_message(SetTarget(project = fakeProject, target = fakeTarget),
    "Autopilot started")
    expect_equal(as.character(bodyForInspect$target), fakeTarget)
  })
})

test_that("Use non-null metric", {
  withSetTargetMocks({
    expect_message(
      SetTarget(project = fakeProject, target = fakeTarget, metric = "testMetric"),
      "Autopilot started")
    expect_equal(as.character(bodyForInspect$metric), "testMetric")
  })
})

test_that("Use non-null weights", {
  withSetTargetMocks({
    expect_message(
      SetTarget(project = fakeProject, target = fakeTarget, weights = "testWeights"),
      "Autopilot started")
    expect_equal(as.character(bodyForInspect$weights), "testWeights")
  })
})

test_that("Use non-null featurelistId", {
  withSetTargetMocks({
    expect_message(
      SetTarget(project = fakeProject, target = fakeTarget, featurelistId = fakeFeaturelistId),
      "Autopilot started")
    expect_equal(as.character(bodyForInspect$featurelistId), fakeFeaturelistId)
  })
})

test_that("Use non-null mode", {
  withSetTargetMocks({
    expect_message(
      SetTarget(project = fakeProject, target = fakeTarget, mode = "testMode"),
      "Autopilot started")
    expect_equal(as.character(bodyForInspect$mode), "testMode")
  })
})

test_that("Use non-null seed", {
  withSetTargetMocks({
    expect_message(
      SetTarget(project = fakeProject, target = fakeTarget, seed = 19),
      "Autopilot started")
    expect_equal(as.numeric(bodyForInspect$seed), 19)
  })
})

test_that("Use non-null positiveClass", {
  withSetTargetMocks({
    expect_message(
      SetTarget(project = fakeProject, target = fakeTarget, positiveClass = "testClass"),
      "Autopilot started")
    expect_equal(as.character(bodyForInspect$positiveClass), "testClass")
  })
})

test_that("Use non-null blueprintThreshold", {
  withSetTargetMocks({
    expect_message(
      SetTarget(project = fakeProject, target = fakeTarget, blueprintThreshold = 8),
      "Autopilot started")
    expect_equal(as.numeric(bodyForInspect$blueprintThreshold), 8)
  })
})

test_that("Use non-null responseCap", {
  withSetTargetMocks({
    expect_message(
      SetTarget(project = fakeProject, target = fakeTarget, responseCap = 0.8),
      "Autopilot started")
    expect_equal(as.numeric(bodyForInspect$responseCap), 0.8)
  })
})

test_that("Use non-null downsampling", {
  withSetTargetMocks({
    expect_message(
      SetTarget(project = fakeProject, target = fakeTarget,
                smartDownsampled = TRUE, majorityDownsamplingRate = 0.9),
      "Autopilot started")
    expect_true(as.logical(bodyForInspect$smartDownsampled))
    expect_equal(as.numeric(bodyForInspect$majorityDownsamplingRate), 0.9)
  })
})

test_that("Use non-null accuracyOptimizedBlueprints", {
  withSetTargetMocks({
    expect_message(
      SetTarget(project = fakeProject, target = fakeTarget,
                accuracyOptimizedBlueprints = TRUE),
      "Autopilot started")
    expect_true(as.logical(bodyForInspect$accuracyOptimizedMb))
  })
})

test_that("Use non-null offset and exposure", {
  withSetTargetMocks({
    expect_message(
      SetTarget(project = fakeProject, target = fakeTarget,
                offset = c("offset_var1", "offset_var2"),
                exposure = "exposure_var"),
      "Autopilot started")
    expect_equal(as.character(bodyForInspect$offset), c("offset_var1", "offset_var2"))
    expect_equal(as.character(bodyForInspect$exposure), "exposure_var")
  })
})

test_that("Use non-null eventsCount", {
  withSetTargetMocks({
    expect_message(
      SetTarget(project = fakeProject, target = fakeTarget,
                eventsCount = "events_count_column"),
      "Autopilot started")
    expect_equal(as.character(bodyForInspect$eventsCount), "events_count_column")
  })
})

test_that("Use monotonic constraints", {
  withSetTargetMocks({
    expect_message(
      SetTarget(project = fakeProject, target = fakeTarget,
                monotonicIncreasingFeaturelistId = "monotonic-flist-up",
                monotonicDecreasingFeaturelistId = "monotonic-flist-down",
                onlyIncludeMonotonicBlueprints = TRUE),
      "Autopilot started")
    expect_equal(as.character(bodyForInspect$monotonicIncreasingFeaturelistId),
                 "monotonic-flist-up")
    expect_equal(as.character(bodyForInspect$monotonicDecreasingFeaturelistId),
                 "monotonic-flist-down")
    expect_true(as.logical(bodyForInspect$onlyIncludeMonotonicBlueprints))
  })
})

test_that("Use actual feature lists for monotonic constraints", {
  withSetTargetMocks({
    expect_message(
      SetTarget(project = fakeProject, target = fakeTarget,
                monotonicIncreasingFeaturelistId = fakeFeaturelist,
                monotonicDecreasingFeaturelistId = fakeFeaturelist,
                onlyIncludeMonotonicBlueprints = TRUE),
      "Autopilot started")
    expect_equal(as.character(bodyForInspect$monotonicIncreasingFeaturelistId),
                 fakeFeaturelistId)
    expect_equal(as.character(bodyForInspect$monotonicDecreasingFeaturelistId),
                 fakeFeaturelistId)
    expect_true(as.logical(bodyForInspect$onlyIncludeMonotonicBlueprints))
  })
})

test_that("Use valid targetTypes", {
  withSetTargetMocks({
    expect_message(
      SetTarget(project = fakeProject, target = fakeTarget, targetType = TargetType$Multiclass),
      "Autopilot started")
    expect_equal(as.character(bodyForInspect$targetType), TargetType$Multiclass)
  })
})

test_that("Fail on invalid targetTypes", {
  expect_error(withSetTargetMocks(
    SetTarget(project = fakeProject, target = fakeTarget, targetType = "MAGIC")),
    paste0("Invalid ", sQuote("TargetType"), ". Must be in ", sQuote("Binary"), ", ",
          sQuote("Multiclass"), ", ", sQuote("Regression"), " but got ", sQuote("MAGIC"),
          " instead."))
})

test_that("Use full autopilot mode", {
  withSetTargetMocks({
    expect_message(
      SetTarget(project = fakeProject, target = fakeTarget, mode = AutopilotMode$FullAuto),
      "Autopilot started")
    expect_equal(as.character(bodyForInspect$mode), AutopilotMode$FullAuto)
  })
})

test_that("Use manual mode", {
  withSetTargetMocks({
    expect_message(
      SetTarget(project = fakeProject, target = fakeTarget, mode = AutopilotMode$Manual),
      "Autopilot started")
    expect_equal(as.character(bodyForInspect$mode), AutopilotMode$Manual)
  })
})

test_that("Use quickrun mode", {
  withSetTargetMocks({
    expect_message(
      SetTarget(project = fakeProject, target = fakeTarget, mode = AutopilotMode$Quick),
      "Autopilot started"
    )
    # Prior to DSX-2417 using Quick would actually use FullAuto but with quickrun == TRUE
    # now we just use Quick mode
    expect_equal(as.character(bodyForInspect$mode), AutopilotMode$Quick)
    expect_null(bodyForInspect$quickrun)
  })
})


partition <- CreateDatetimePartitionSpecification(fakeDateColumn,
                                                  autopilotDataSelectionMethod = NULL,
                                                  validationDuration = NULL,
                                                  holdoutStartDate = NULL,
                                                  holdoutDuration = NULL,
                                                  gapDuration = NULL,
                                                  numberOfBacktests = NULL,
                                                  backtests = NULL)
test_that("Datetime partition with empty backtests", {
  withSetTargetMocks({
    expect_message(
      SetTarget(project = fakeProject, target = fakeTarget, partition = partition),
      "Autopilot started")
    expect_equal(as.character(bodyForInspect$cvMethod), "datetime")
    expect_equal(as.character(bodyForInspect$datetimePartitionColumn), fakeDateColumn)
    expect_false(as.logical(bodyForInspect$useTimeSeries))
    expect_false(as.logical(bodyForInspect$defaultToKnownInAdvance))
  })
})


partition <- CreateDatetimePartitionSpecification(fakeDateColumn,
                                      featureSettings = list(featureName = "Product_offers",
                                                             knownInAdvance = TRUE))
test_that("Datetime partition with feature settings", {
  withSetTargetMocks({
    expect_message(
      SetTarget(project = fakeProject, target = fakeTarget, partition = partition),
      "Autopilot started")
    expect_equal(as.character(bodyForInspect$cvMethod), "datetime")
    expect_equal(as.character(bodyForInspect$datetimePartitionColumn), fakeDateColumn)
    expect_false(as.logical(bodyForInspect$useTimeSeries))
    expect_false(as.logical(bodyForInspect$defaultToKnownInAdvance))
    expect_type(bodyForInspect$featureSettings, "list")
    expect_equal(bodyForInspect$featureSettings[[1]]$featureName, "Product_offers")
    expect_true(bodyForInspect$featureSettings[[1]]$knownInAdvance)
  })
})


partition <- CreateDatetimePartitionSpecification(fakeDateColumn,
                                      featureSettings = list(featureName = "Sales",
                                                             doNotDerive = TRUE))
test_that("Datetime partition with doNotDerive feature setting", {
  withSetTargetMocks({
    expect_message(
      SetTarget(project = fakeProject, target = fakeTarget, partition = partition),
      "Autopilot started")
    expect_equal(as.character(bodyForInspect$cvMethod), "datetime")
    expect_equal(as.character(bodyForInspect$datetimePartitionColumn), fakeDateColumn)
    expect_false(as.logical(bodyForInspect$useTimeSeries))
    expect_false(as.logical(bodyForInspect$defaultToKnownInAdvance))
    expect_type(bodyForInspect$featureSettings, "list")
    expect_equal(bodyForInspect$featureSettings[[1]]$featureName, "Sales")
    expect_true(bodyForInspect$featureSettings[[1]]$doNotDerive)
  })
})


partition <- CreateDatetimePartitionSpecification(fakeDateColumn,
                                                  treatAsExponential = TreatAsExponential$Always,
                                                  differencingMethod = DifferencingMethod$Seasonal,
                                                  periodicities = list(list("timeSteps" = 10,
                                                                            "timeUnit" = "HOUR"),
                                                                       list("timeSteps" = 600,
                                                                            "timeUnit" = "MINUTE"),
                                                                       list("timeSteps" = 7,
                                                                            "timeUnit" = "DAY")))
test_that("Datetime partition with exponential, differencing, and periodicities", {
  withSetTargetMocks({
    expect_message(
      SetTarget(project = fakeProject, target = fakeTarget, mode = AutopilotMode$Quick,
                partition = partition),
      "Autopilot started")
    expect_equal(as.character(bodyForInspect$cvMethod), "datetime")
    expect_equal(as.character(bodyForInspect$datetimePartitionColumn), fakeDateColumn)
    expect_false(as.logical(bodyForInspect$useTimeSeries))
    expect_false(as.logical(bodyForInspect$defaultToKnownInAdvance))
    expect_equal(as.character(bodyForInspect$treatAsExponential), TreatAsExponential$Always)
    expect_equal(as.character(bodyForInspect$differencingMethod), DifferencingMethod$Seasonal)
    expect_equal(bodyForInspect$periodicities[[1]]$timeSteps, 10)
    expect_equal(bodyForInspect$periodicities[[1]]$timeUnit, "HOUR")
    expect_equal(bodyForInspect$periodicities[[2]]$timeSteps, 600)
    expect_equal(bodyForInspect$periodicities[[2]]$timeUnit, "MINUTE")
  })
})


partition <- CreateDatetimePartitionSpecification(fakeDateColumn,
                                                  windowsBasisUnit = "ROW",
                                                  periodicities = list(list("timeSteps" = 10,
                                                                            "timeUnit" = "ROW")))
test_that("Datetime partition with windowsBasisUnit", {
  withSetTargetMocks({
    expect_message(
      SetTarget(project = fakeProject, target = fakeTarget, mode = AutopilotMode$Quick,
                partition = partition),
      "Autopilot started")
    expect_equal(as.character(bodyForInspect$cvMethod), "datetime")
    expect_equal(as.character(bodyForInspect$datetimePartitionColumn), fakeDateColumn)
    expect_false(as.logical(bodyForInspect$useTimeSeries))
    expect_false(as.logical(bodyForInspect$defaultToKnownInAdvance))
    expect_equal(as.character(bodyForInspect$windowsBasisUnit), "ROW")
    expect_equal(bodyForInspect$periodicities[[1]]$timeSteps, 10)
    expect_equal(bodyForInspect$periodicities[[1]]$timeUnit, "ROW")
  })
})

test_that("Datetime partition with invalid partition", {
  with_mock("datarobot:::Endpoint" = function() fakeEndpoint,
            "datarobot:::Token" = function() fakeToken,
            GetProjectStatus = function(...) list(stage = ProjectStage$AIM), {
    expect_error(SetTarget(project = fakeProject, target = fakeTarget, mode = AutopilotMode$Quick,
                partition = list(fakeDateColumn)),
      "must use a valid partition object")
  })
})


partition <- CreateDatetimePartitionSpecification(fakeDateColumn, useTimeSeries = TRUE)
test_that("True time series partition", {
  withSetTargetMocks({
    expect_message(
      SetTarget(project = fakeProject, target = fakeTarget, mode = AutopilotMode$Quick,
                partition = partition),
      "Autopilot started")
    expect_equal(as.character(bodyForInspect$cvMethod), "datetime")
    expect_equal(as.character(bodyForInspect$datetimePartitionColumn), fakeDateColumn)
    expect_true(as.logical(bodyForInspect$useTimeSeries))
    expect_false(as.logical(bodyForInspect$defaultToKnownInAdvance))
  })
})


partition <- CreateDatetimePartitionSpecification(fakeDateColumn, useTimeSeries = TRUE,
                                                  multiseriesIdColumns = fakeMultiIdColumn)
test_that("Multiseries partition", {
  withSetTargetMocks({
    expect_message(
      SetTarget(project = fakeProject, target = fakeTarget, mode = AutopilotMode$Quick,
                partition = partition),
      "Autopilot started")
    expect_equal(as.character(bodyForInspect$cvMethod), "datetime")
    expect_equal(as.character(bodyForInspect$datetimePartitionColumn), fakeDateColumn)
    expect_equal(bodyForInspect$multiseriesIdColumns[[1]], fakeMultiIdColumn)
    expect_true(as.logical(bodyForInspect$useTimeSeries))
    expect_false(as.logical(bodyForInspect$defaultToKnownInAdvance))
  }, multiseriesMock = TRUE)
})


partition <- CreateDatetimePartitionSpecification(fakeDateColumn, useTimeSeries = TRUE,
                                                  multiseriesIdColumns = fakeMultiIdColumn,
                                                  useCrossSeries = TRUE,
                                                  aggregationType = "total")
test_that("Cross series partition", {
  withSetTargetMocks({
    expect_message(
      SetTarget(project = fakeProject, target = fakeTarget, mode = AutopilotMode$Quick,
                partition = partition),
      "Autopilot started")
    expect_equal(as.character(bodyForInspect$cvMethod), "datetime")
    expect_equal(as.character(bodyForInspect$datetimePartitionColumn), fakeDateColumn)
    expect_equal(bodyForInspect$multiseriesIdColumns[[1]], fakeMultiIdColumn)
    expect_true(as.logical(bodyForInspect$useTimeSeries))
    expect_false(as.logical(bodyForInspect$defaultToKnownInAdvance))
    expect_true(as.logical(bodyForInspect$useCrossSeriesFeatures))
    expect_equal(as.character(bodyForInspect$aggregationType), "total")
  }, multiseriesMock = TRUE)
})


partition <- CreateDatetimePartitionSpecification(fakeDateColumn,
                                                  useTimeSeries = TRUE,
                                                  calendar = fakeCalendar)
test_that("Time series partition with calendar", {
  withSetTargetMocks({
    expect_message(
      SetTarget(project = fakeProject, target = fakeTarget, mode = AutopilotMode$Quick,
                partition = partition),
      "Autopilot started")
    expect_equal(as.character(bodyForInspect$cvMethod), "datetime")
    expect_equal(as.character(bodyForInspect$datetimePartitionColumn), fakeDateColumn)
    expect_true(as.logical(bodyForInspect$useTimeSeries))
    expect_false(as.logical(bodyForInspect$defaultToKnownInAdvance))
  })
})


partition <- CreateDatetimePartitionSpecification(fakeDateColumn, useTimeSeries = TRUE,
                                                  multiseriesIdColumns = fakeMultiIdColumn,
                                                  useCrossSeries = TRUE,
                                                  crossSeriesGroupByColumns = fakeCrossIdColumn)
test_that("Cross series partition with crossSeriesGroupByColumns", {
  withSetTargetMocks({
    expect_message(
      SetTarget(project = fakeProject, target = fakeTarget, mode = AutopilotMode$Quick,
                partition = partition),
      "Autopilot started")
    expect_equal(bodyForInspect$multiseriesIdColumns[[1]], fakeMultiIdColumn)
    expect_true(as.logical(bodyForInspect$useTimeSeries))
    expect_false(as.logical(bodyForInspect$defaultToKnownInAdvance))
    expect_true(as.logical(bodyForInspect$useCrossSeriesFeatures))
    expect_equal(as.character(bodyForInspect$crossSeriesGroupByColumns), fakeCrossIdColumn)
  }, multiseriesMock = TRUE, crossSeriesGroupByMock = TRUE)
})
