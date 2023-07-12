IsDateTimePartition <- function(partition) {
  identical(partition$cvMethod, cvMethods$DATETIME)
}

IsMultiSeriesPartition <- function(partition) {
  "multiseriesIdColumns" %in% names(partition) && !is.null(partition$multiseriesIdColumns)
}

IsCrossSeriesGroupByPartition <- function(partition) {
  "crossSeriesGroupByColumns" %in% names(partition) &&
    !is.null(partition$crossSeriesGroupByColumns)
}


#' Set the target variable (and by default, start the DataRobot Autopilot)
#'
#' This function sets the target variable for the project defined by
#' project, starting the process of building models to predict the response
#' variable target.  Both of these parameters - project and target - are
#' required and they are sufficient to start a modeling project with
#' DataRobot default specifications for the other optional parameters.
#'
#' @inheritParams DeleteProject
#' @inheritParams GetValidMetrics
#' @param metric character. Optional. String specifying the model fitting metric
#'   to be optimized; a list of valid options for this parameter, which depends on
#'   both project and target, may be obtained with the function GetValidMetrics.
#' @param weights character. Optional. String specifying the name of the column
#'   from the modeling dataset to be used as weights in model fitting.
#' @param partition partition. Optional. S3 object of class 'partition' whose elements specify
#'   a valid partitioning scheme.  See help for functions CreateGroupPartition,
#'   CreateRandomPartition, CreateStratifiedPartition, CreateUserPartition
#'   and CreateDatetimePartitionSpecification.
#' @param targetType character. Optional. Used to specify the targetType to use for a project.
#'   Valid options are "Binary", "Multiclass", "Regression". Set to "Multiclass" to enable
#'   multiclass modeling. Otherwise, it can help to disambiguate, i.e. telling DataRobot how to
#'   handle a numeric target with a few unique values that could be used for either multiclass
#'   or regression. See \code{TargetType} for an easier way to keep track of the options.
#' @param mode character. Optional. Specifies the autopilot mode used to start the
#'   modeling project; See \code{AutopilotMode} for valid options; \code{AutopilotMode$Quick} is
#'   default.
#' @param seed integer. Optional. Seed for the random number generator used in
#'   creating random partitions for model fitting.
#' @param positiveClass character. Optional. Target variable value corresponding to a positive
#'   response in binary classification problems.
#' @param blueprintThreshold integer. Optional. The maximum time
#'   (in hours) that any modeling blueprint is allowed to run before being
#'   excluded from subsequent autopilot stages.
#' @param responseCap numeric. Optional. Floating point value, between 0.5 and 1.0,
#'   specifying a capping limit for the response variable. The default value
#'   NULL corresponds to an uncapped response, equivalent to responseCap = 1.0.
#' @param featurelistId numeric. Specifies which feature list to use. If NULL (default),
#'   a default featurelist is used.
#' @param smartDownsampled logical. Optional. Whether to use smart downsampling to throw
#'   away excess rows of the majority class. Only applicable to classification and zero-boosted
#'   regression projects.
#' @param majorityDownsamplingRate numeric. Optional. Floating point value, between 0.0 and 100.0.
#'   The percentage of the majority rows that should be kept.  Specify only if using smart
#'   downsampling. May not cause the majority class to become smaller than the minority class.
#' @param accuracyOptimizedBlueprints logical. Optional. When enabled, accuracy optimized
#'  blueprints will run in autopilot for the project. These are longer-running model blueprints
#'  that provide increased accuracy over normal blueprints that run during autopilot.
#' @param offset character. Optional. Vector of the names of the columns containing the offset of
#'   each row.
#' @param exposure character. Optional. The name of a column containing the exposure of each row.
#' @param eventsCount character. Optional. The name of a column specifying the events count.
#' @param monotonicIncreasingFeaturelistId character. Optional. The id of the featurelist
#'   that defines the set of features with a monotonically increasing relationship to the
#'   target. If \code{NULL} (default), no such constraints are enforced. When specified, this
#'   will set a default for the project that can be overridden at model submission time if
#'   desired. The featurelist itself can also be passed as this parameter.
#' @param monotonicDecreasingFeaturelistId character. Optional. The id of the featurelist
#'   that defines the set of features with a monotonically decreasing relationship to the
#'   target. If \code{NULL} (default), no such constraints are enforced. When specified, this
#'   will set a default for the project that can be overridden at model submission time if
#'   desired. The featurelist itself can also be passed as this parameter.
#' @param onlyIncludeMonotonicBlueprints logical. Optional. When TRUE, only blueprints that
#'   support enforcing monotonic constraints will be available in the project or selected for
#'   the autopilot.
#' @param maxWait integer. Specifies how many seconds to wait for the server to finish
#'   analyzing the target and begin the modeling process. If the process takes
#'   longer than this parameter specifies, execution will stop (but the server
#'   will continue to process the request).
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   SetTarget(projectId, "targetFeature")
#'   SetTarget(projectId, "targetFeature", metric = "LogLoss")
#'   SetTarget(projectId, "targetFeature", mode = AutopilotMode$Manual)
#'   SetTarget(projectId, "targetFeature", targetType = TargetType$Multiclass)
#' }
#' @export
SetTarget <- function(project, target, metric = NULL, weights = NULL,
                      partition = NULL, mode = AutopilotMode$Quick, seed = NULL, targetType = NULL,
                      positiveClass = NULL, blueprintThreshold = NULL,
                      responseCap = NULL, featurelistId = NULL,
                      smartDownsampled = NULL, majorityDownsamplingRate = NULL,
                      accuracyOptimizedBlueprints = NULL,
                      offset = NULL, exposure = NULL, eventsCount = NULL,
                      monotonicIncreasingFeaturelistId = NULL,
                      monotonicDecreasingFeaturelistId = NULL,
                      onlyIncludeMonotonicBlueprints = FALSE,
                      maxWait = 600) {
  if (is.null(target)) {
    stop("No target variable specified - cannot start Autopilot")
  }

  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "aim")
  pStat <- GetProjectStatus(projectId)
  stage <- as.character(pStat[which(names(pStat) == "stage")])
  if (!identical(stage, ProjectStage$AIM)) {
    errorMsg <- paste("Autopilot stage is", stage,
                      "but it must be 'aim' to set the target and start a new project")
    stop(strwrap(errorMsg))
  }

  bodyList <- list(target = target)
  bodyList$metric <- metric
  bodyList$weights <- weights
  bodyList$mode <- mode
  bodyList$seed <- seed
  bodyList$positiveClass <- positiveClass
  bodyList$blueprintThreshold <- blueprintThreshold
  bodyList$responseCap <- responseCap
  bodyList$featurelistId <- featurelistId
  bodyList$smartDownsampled <- smartDownsampled
  bodyList$majorityDownsamplingRate <- majorityDownsamplingRate
  bodyList$accuracyOptimizedMb <- accuracyOptimizedBlueprints
  bodyList$offset <- offset
  bodyList$exposure <- exposure
  bodyList$eventsCount <- eventsCount
  if (is.list(monotonicIncreasingFeaturelistId) &&
      "featurelistId" %in% names(monotonicIncreasingFeaturelistId)) {
    monotonicIncreasingFeaturelistId <- monotonicIncreasingFeaturelistId$featurelistId
  }
  bodyList$monotonicIncreasingFeaturelistId <- monotonicIncreasingFeaturelistId
  if (is.list(monotonicDecreasingFeaturelistId) &&
      "featurelistId" %in% names(monotonicDecreasingFeaturelistId)) {
    monotonicDecreasingFeaturelistId <- monotonicDecreasingFeaturelistId$featurelistId
  }
  bodyList$monotonicDecreasingFeaturelistId <- monotonicDecreasingFeaturelistId
  bodyList$onlyIncludeMonotonicBlueprints <- onlyIncludeMonotonicBlueprints
  ValidateParameterIn(targetType, TargetType)
  bodyList$targetType <- targetType

  if (!is.null(partition) && !is(partition, "partition")) {
    partitioningMethods <- c("CreateRandomPartition", "CreateStratifiedPartition",
                             "CreateGroupPartition", "CreateUserPartition",
                             "CreateDatetimePartitionSpecification")
    stop("You must use a valid partition object to specify your partitioning.",
         " See docs for ", paste0(lapply(partitioningMethods, sQuote), collapse = ", "),
         " for examples.")
  }
  if (!is.null(partition)) {
    if (IsDateTimePartition(partition)) {
      partition <- as.dataRobotDatetimePartitionSpecification(partition)
      if (IsMultiSeriesPartition(partition)) {
        properties <- RequestMultiSeriesDetection(project,
                                                  partition$datetimePartitionColumn,
                                                  partition$multiseriesIdColumns,
                                                  maxWait = maxWait)
        if (!is.list(partition$multiseriesIdColumns)) {
          partition$multiseriesIdColumns <- list(partition$multiseriesIdColumns)
        }
        if (IsCrossSeriesGroupByPartition(partition)) {
          properties <- RequestCrossSeriesDetection(project,
                                                    partition$datetimePartitionColumn,
                                                    partition$multiseriesIdColumns,
                                                    partition$crossSeriesGroupByColumns,
                                                    maxWait = maxWait)
          if (!is.list(partition$crossSeriesGroupByColumns)) {
            partition$crossSeriesGroupByColumns <- list(partition$crossSeriesGroupByColumns)
          }
        }
        properties <- as.dataRobotFeatureInfo(properties)
        ValidateMultiSeriesProperties(properties)
      }
    }
    bodyList <- append(bodyList, partition)
  }
  response <- DataRobotPATCH(routeString,
                             body = lapply(bodyList, Unbox),
                             returnRawResponse = TRUE,
                             encode = "json")
  WaitForAsyncReturn(GetRedirectFromResponse(response),
                     addUrl = FALSE,
                     maxWait = maxWait,
                     failureStatuses = "ERROR")
  message("Autopilot started")
}


#' Start a project, set the target, and run autopilot.
#'
#' This function is a convenient shorthand to start a project and set the target.
#' See \code{SetupProject} and \code{SetTarget}.
#'
#' @inheritParams SetTarget
#' @inheritParams SetupProject
#' @inheritParams SetupProjectFromDataSource
#' @inheritParams WaitForAutopilot
#' @inheritParams UpdateProject
#' @param wait logical. If \code{TRUE}, invokes \code{WaitForAutopilot} to block execution until
#'   the autopilot is complete.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   StartProject(iris,
#'                projectName = "iris",
#'                target = "Species",
#'                targetType = TargetType$Multiclass)
#' }
#' @export
StartProject <- function(dataSource, projectName = NULL, target, metric = NULL, weights = NULL,
                         partition = NULL, mode = NULL, seed = NULL, targetType = NULL,
                         positiveClass = NULL, blueprintThreshold = NULL,
                         responseCap = NULL, featurelistId = NULL,
                         smartDownsampled = NULL, majorityDownsamplingRate = NULL,
                         accuracyOptimizedBlueprints = NULL,
                         offset = NULL, exposure = NULL, eventsCount = NULL,
                         monotonicIncreasingFeaturelistId = NULL,
                         monotonicDecreasingFeaturelistId = NULL,
                         onlyIncludeMonotonicBlueprints = FALSE, workerCount = NULL,
                         wait = FALSE, checkInterval = 20, timeout = NULL,
                         username = NULL, password = NULL, verbosity = 1, maxWait = 600) {
  if (is.null(projectName)) { projectName <- deparse(substitute(dataSource)) }
  if (is(dataSource, "dataRobotDataSource")) {
    if (is.null(username) || is.null(password)) {
      stop("Username and password must be defined to start a project with a database.")
    }
    project <- SetupProjectFromDataSource(dataSourceId = dataSource,
                                          username = username,
                                          password = password,
                                          projectName = projectName,
                                          maxWait = maxWait)
  } else {
    project <- SetupProject(dataSource = dataSource, projectName = projectName, maxWait = maxWait)
  }
  SetTarget(project, target = target, metric = metric, weights = weights,
            partition = partition, mode = mode, seed = seed, targetType = targetType,
            positiveClass = positiveClass, blueprintThreshold = blueprintThreshold,
            responseCap = responseCap, featurelistId = featurelistId,
            smartDownsampled = smartDownsampled,
            majorityDownsamplingRate = majorityDownsamplingRate,
            accuracyOptimizedBlueprints = accuracyOptimizedBlueprints,
            offset = offset, exposure = exposure, eventsCount = eventsCount,
            monotonicIncreasingFeaturelistId = monotonicIncreasingFeaturelistId,
            monotonicDecreasingFeaturelistId = monotonicDecreasingFeaturelistId,
            onlyIncludeMonotonicBlueprints = onlyIncludeMonotonicBlueprints,
            maxWait = maxWait)
  if (!is.null(workerCount)) { UpdateProject(project, workerCount = workerCount) }
  if (isTRUE(wait)) { WaitForAutopilot(project, checkInterval = checkInterval,
                                       timeout = timeout, verbosity = verbosity) }
  project
}


#' Starts autopilot on provided featurelist.
#
#' Only one autopilot can be running at the time.
#' That's why any ongoing autopilot on different featurelist will
#' be halted - modeling jobs in queue would not
#' be affected but new jobs would not be added to queue by
#' halted autopilot.
#'
#' There is an error if autopilot is currently running on or has already
#' finished running on the provided featurelist and also if project's target was not selected
#' (via SetTarget).
#'
#' @inheritParams DeleteProject
#' @param featurelistId numeric. Specifies which feature list to use.
#' @param mode character. The desired autopilot mode. Currently only AutopilotMode$FullAuto
#'   is supported.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   featureList <- CreateFeaturelist(projectId, "myFeaturelist", c("feature1", "feature2"))
#'   featurelistId <- featureList$featurelistId
#'   StartNewAutoPilot(projectId, featurelistId)
#' }
#' @export
StartNewAutoPilot <- function(project, featurelistId, mode = AutopilotMode$FullAuto) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "autopilots")
  payload <- list(featurelistId = featurelistId, mode = mode)
  invisible(DataRobotPOST(routeString, body = payload))
}
