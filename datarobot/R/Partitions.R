#' Create a group-based S3 object of class partition for the SetTarget function
#'
#' Group partitioning constructs data partitions such that all records with each
#' level in the column specified by the parameter partitionKeyCols occur
#' together in the same partition.
#'
#' This function is one of several convenience functions provided to simplify the task
#' of starting modeling projects with custom partitioning options. The other
#' functions are \code{CreateRandomPartition}, \code{CreateStratifiedPartition}, and
#' \code{CreateUserPartition}.
#'
#' @param validationType character. String specifying the type of partition
#' generated, either "TVH" or "CV".
#' @param holdoutPct integer. The percentage of data to be used as the holdout subset.
#' @param partitionKeyCols list. List containing a single string specifying
#'    the name of the variable used in defining the group partition.
#' @param reps integer. The number of cross-validation folds to generate; only applicable
#'   when validationType = "CV".
#' @param validationPct integer. The percentage of data to be used as the validation subset.
#' @return An S3 object of class 'partition' including the parameters required
#'   by the SetTarget function to generate a group-based partitioning of
#'   the modeling dataset.
#' @seealso \code{\link{CreateRandomPartition}}, \code{\link{CreateStratifiedPartition}},
#'   \code{\link{CreateUserPartition}}.
#' @examples
#' CreateGroupPartition(validationType = "CV",
#'                      holdoutPct = 20,
#'                      partitionKeyCols = list("groupId"),
#'                      reps = 5)
#' @export
CreateGroupPartition <- function(validationType, holdoutPct, partitionKeyCols,
                                 reps = NULL, validationPct = NULL) {
  if (!is.list(partitionKeyCols)) {
    stop("Please specify partition column name as a list containing a single string.")
  }
  if (length(partitionKeyCols) > 1) {
    stop("Currently only one partition key column is supported.")
  }
  partition <- list(cvMethod = cvMethods$GROUP, validationType = validationType,
                    holdoutPct = holdoutPct,
                    partitionKeyCols = partitionKeyCols)
  ValidatePartition(validationType = validationType,
                    partition = partition,
                    reps = reps,
                    validationPct = validationPct)
}


#' Create a random sampling-based S3 object of class partition for the SetTarget function
#'
#' Random partitioning is supported for either Training/Validation/Holdout
#' ("TVH") or cross-validation ("CV") splits. In either case, the holdout
#' percentage (holdoutPct) must be specified; for the "CV" method, the
#' number of cross-validation folds (reps) must also be specified, while
#' for the "TVH" method, the validation subset percentage (validationPct)
#' must be specified.
#'
#' This function is one of several convenience functions provided to simplify the task
#' of starting modeling projects with custom partitioning options. The other
#' functions are \code{CreateGroupPartition}, \code{CreateStratifiedPartition}, and
#' \code{CreateUserPartition}.
#'
#' @inheritParams CreateGroupPartition
#' @return An S3 object of class partition including the parameters
#'   required by SetTarget to generate a random partitioning of
#'   the modeling dataset.
#' @seealso \code{\link{CreateStratifiedPartition}}, \code{\link{CreateGroupPartition}},
#'   \code{\link{CreateUserPartition}}.
#' @examples
#' CreateRandomPartition(validationType = "CV", holdoutPct = 20, reps = 5)
#' @export
CreateRandomPartition <- function(validationType, holdoutPct, reps = NULL,
                                  validationPct = NULL) {
  partition <- list(cvMethod = cvMethods$RANDOM, validationType = validationType,
                    holdoutPct = holdoutPct)
  ValidatePartition(validationType = validationType,
                    partition = partition,
                    reps = reps,
                    validationPct = validationPct)
}


#' Create a stratified sampling-based S3 object of class partition for the SetTarget function
#'
#' Stratified partitioning is supported for binary classification problems and
#' it randomly partitions the modeling data, keeping the percentage of positive
#' class observations in each partition the same as in the original dataset.
#' Stratified partitioning is supported for either Training/Validation/Holdout
#' ("TVH") or cross-validation ("CV") splits. In either case, the holdout
#' percentage (holdoutPct) must be specified; for the "CV" method, the number
#' of cross-validation folds (reps) must also be specified, while for the "TVH"
#' method, the validation subset percentage (validationPct) must be specified.
#'
#' This function is one of several convenience functions provided to simplify the task
#' of starting modeling projects with custom partitioning options. The other
#' functions are \code{CreateGroupPartition}, \code{CreateRandomPartition}, and
#' \code{CreateUserPartition}.
#'
#' @inheritParams CreateGroupPartition
#' @return An S3 object of class 'partition' including the parameters required
#'   by the SetTarget function to generate a stratified partitioning of the
#'   modeling dataset.
#' @seealso \code{\link{CreateGroupPartition}}, \code{\link{CreateRandomPartition}},
#'   \code{\link{CreateUserPartition}}.
#' @examples
#' CreateStratifiedPartition(validationType = "CV", holdoutPct = 20, reps = 5)
#' @export
CreateStratifiedPartition <- function(validationType, holdoutPct, reps = NULL,
                                      validationPct = NULL) {
  partition <- list(cvMethod = cvMethods$STRATIFIED, validationType = validationType,
                    holdoutPct = holdoutPct)
  ValidatePartition(validationType = validationType,
                    partition = partition,
                    reps = reps,
                    validationPct = validationPct)
}

#' Create a class partition object for use in the SetTarget function representing a
#' user-defined partition.
#'
#' Creates a list object used by the SetTarget function to specify either
#' Training/Validation/Holdout (validationType = "TVH") or cross-validation
#' (validationType = "CV") partitions of the modeling dataset based on the values
#' included in a column from the dataset. In either case, the name of this data
#' column must be specified (as userPartitionCol).
#'
#' For the "TVH" option of cvMethod, no cross-validation is used. Users must specify
#' the trainingLevel and validationLevel; use of a holdoutLevel is always recommended
#' but not required. If no holdoutLevel is used, then the column must contain exactly
#' 2 unique values. If a holdoutLevel is used, the column must contain exactly 3 unique
#' values.
#'
#' For the "CV" option, each value in the column will be used to separate rows into
#' cross-validation folds. Use of a holdoutLevel is optional; if not specified, then
#' no holdout is used.
#'
#' This function is one of several convenience functions provided to simplify the task
#' of starting modeling projects with custom partitioning options. The other
#' functions are \code{CreateGroupPartition}, \code{CreateRandomPartition}, and
#' \code{CreateStratifiedPartition}.
#'
#' @inheritParams CreateGroupPartition
#' @param userPartitionCol character. String naming the data column from the
#' modeling dataset containing the subset designations.
#' @param cvHoldoutLevel character. Data value from userPartitionCol that identifies the
#' holdout subset under the "CV" option.
#' @param trainingLevel character. Data value from userPartitionCol that identifies the
#' training subset under the "TVH" option.
#' @param holdoutLevel character. Data value from userPartitionCol that identifies the
#' holdout subset under both "TVH" and "CV" options. To specify that the project should
#' not use a holdout you can omit this parameter or pass NA directly.
#' @param validationLevel character. Data value from userPartitionCol that identifies the
#' validation subset under the "TVH" option.
#' @return An S3 object of class 'partition' including the parameters required
#' by the SetTarget function to generate a user-specified of the modeling
#' dataset.
#' @seealso \code{\link{CreateGroupPartition}}, \code{\link{CreateRandomPartition}},
#'   \code{\link{CreateStratifiedPartition}}.
#' @examples
#' CreateUserPartition(validationType = "CV", userPartitionCol = "TVHflag", cvHoldoutLevel = NA)
#' @export
CreateUserPartition <- function(validationType, userPartitionCol,
                                cvHoldoutLevel = NULL, trainingLevel = NULL,
                                holdoutLevel = NULL, validationLevel = NULL) {
  if (!is.character(userPartitionCol)) {
    stop("Please specify partition column name as a character string")
  }
  partition <- list(cvMethod = cvMethods$USER, validationType = validationType,
                    userPartitionCol = userPartitionCol)
  if (validationType == "CV") {
    if (is.null(cvHoldoutLevel)) {
      partition$cvHoldoutLevel <- NA
    } else {
      partition$cvHoldoutLevel <- cvHoldoutLevel
    }
  } else if (identical(validationType, "TVH")) {
    if (is.null(trainingLevel)) {
      stop(strwrap("Parameter trainingLevel must be specified for user
                partition with validationType = 'TVH'"))
    } else {
      partition$trainingLevel <- trainingLevel
      partition$holdoutLevel <- holdoutLevel
    }
    if (is.null(validationLevel)) {
      stop(strwrap("Parameter validationLevel must be specified for user
                partition with validationType = 'TVH'"))
    } else {
      partition$validationLevel <- validationLevel
    }
  } else {
    stop(strwrap(paste("validationType", validationType, "not valid")))
  }
  class(partition) <- "partition"
  partition
}


#' Create a list describing backtest parameters
#'
#' Uniquely defines a Backtest used in a DatetimePartitioning
#'
#' Includes only the attributes of a backtest directly controllable by users.  The other attributes
#' are assigned by the DataRobot application based on the project dataset and the user-controlled
#' settings.
#' All durations should be specified with a duration string such as those returned
#' by the ConstructDurationString helper function.
#'
#' @param index integer. The index of the backtest
#' @param gapDuration character. The desired duration of the gap
#'   between training and validation data for the backtest in duration format (ISO8601).
#' @param validationStartDate character. The desired start date of the validation data
#'   for this backtest (RFC 3339 format).
#' @param validationDuration character. The desired end date
#'   of the validation data for this backtest in duration format (ISO8601).
#' @return list with backtest parameters
#' @examples
#' zeroDayDuration <- ConstructDurationString()
#' hundredDayDuration <- ConstructDurationString(days = 100)
#' CreateBacktestSpecification(index = 0,
#'                             gapDuration = zeroDayDuration,
#'                             validationStartDate = "1989-12-01",
#'                             validationDuration = hundredDayDuration)
#' @export
CreateBacktestSpecification <- function(index, gapDuration, validationStartDate,
                                        validationDuration) {
  backtestSpec <- list(index = index, gapDuration = gapDuration,
                       validationStartDate = validationStartDate,
                       validationDuration = validationDuration)
  return(backtestSpec)
}

as.dataRobotBacktestSpecification <- function(inList) {
  elements <- c("index",
                "gapDuration",
                "validationStartDate",
                "validationDuration")
  outList <- ApplySchema(inList, elements)
  return(outList)
}



#' Construct a valid string representing a duration in accordance with ISO8601
#'
#' A duration of six months, 3 days, and 12 hours could be represented as P6M3DT12H.
#'
#' @param years integer. The number of years in the duration.
#' @param months integer. The number of months in the duration.
#' @param days integer. The number of days in the duration.
#' @param hours integer. The number of hours in the duration.
#' @param minutes integer. The number of minutes in the duration.
#' @param seconds integer. The number of seconds in the duration.
#' @return The duration string, specified compatibly with ISO8601.
#' @examples
#' ConstructDurationString()
#' ConstructDurationString(days = 100)
#' ConstructDurationString(years = 10, months = 2, days = 5, seconds = 12)
#' @export
ConstructDurationString <- function(years = 0, months = 0, days = 0,
                                    hours = 0, minutes = 0, seconds = 0) {
  return(paste("P", years, "Y",
               months, "M",
               days, "DT",
               hours, "H",
               minutes, "M",
               seconds, "S", sep = ""))
}

#' Create a list describing datetime partition parameters
#'
#' Uniquely defines a DatetimePartitioning for some project
#'
#' Includes only the attributes of DatetimePartitioning that are directly controllable by users,
#' not those determined by the DataRobot application based on the project dataset and the
#' user-controlled settings.
#' This is the specification that should be passed to SetTarget via the
#' partition parameter.  To see the full partitioning based on the project dataset,
#' GenerateDatetimePartition.
#' All durations should be specified with a duration string such as those returned
#' by the ConstructDurationString helper function.
#'
#' @param datetimePartitionColumn character. The name of the column whose values as dates
#'   are used to assign a row to a particular partition
#' @param autopilotDataSelectionMethod character. Optional. Whether models created
#'   by the autopilot should use "rowCount" or "duration" as their dataSelectionMethod
#' @param validationDuration character. Optional. The default validationDuration for the
#'   backtests
#' @param holdoutStartDate character. The start date of holdout scoring data
#'   (RFC 3339 format). If holdoutStartDate is specified, holdoutDuration must also be specified.
#' @param holdoutDuration character. Optional. The duration of the holdout scoring data.
#'   If holdoutDuration is specified, holdoutStartDate must also be specified.
#' @param disableHoldout logical. Optional. Whether to suppress allocating the holdout fold.
#'   If set to TRUE, holdoutStartDate and holdoutDuration must not be specified.
#' @param gapDuration character. Optional. The duration of the gap between training and
#'   holdout scoring data.
#' @param numberOfBacktests integer. The number of backtests to use.
#' @param backtests list. List of BacktestSpecification the exact specification of backtests to use.
#'   The indexes of the specified backtests should range from 0 to numberOfBacktests - 1.
#'   If any backtest is left unspecified, a default configuration will be chosen.
#' @param useTimeSeries logical. Whether to create a time series project (if TRUE) or an OTV
#'   project which uses datetime partitioning (if FALSE). The default behavior is to create an
#'   OTV project.
#' @param defaultToKnownInAdvance logical. Whether to default to treating features as known in
#'   advance. Defaults to FALSE. Only used for time series project. Known in advance features are
#'   expected to be known for dates in the future when making predictions (e.g., "is this a
#'   holiday").
#' @param featureDerivationWindowStart integer. Optional. Offset into the past to define how far
#'   back relative to the forecast point the feature derivation window should start. Only used for
#'   time series projects. Expressed in terms of the \code{timeUnit} of the
#'   \code{datetimePartitionColumn}.
#' @param featureDerivationWindowEnd integer. Optional. Offset into the past to define how far
#'   back relative to the forecast point the feature derivation window should end. Only used for
#'   time series projects. Expressed in terms of the \code{timeUnit} of the
#'   \code{datetimePartitionColumn}.
#' @param featureSettings list. Optional. A list specifying settings for each feature. For each
#'   feature you would like to set feature settings for, pass the following in a list:
#'   \itemize{
#'     \item featureName character. The name of the feature to set feature settings.
#'     \item knownInAdvance logical. Optional. Whether or not the feature is known in advance.
#'       Used for time series only. Defaults to \code{FALSE}.
#'     \item doNotDerive logical. Optional. If \code{TRUE}, no time series derived features
#'       (e.g., lags) will be automatically engineered from this feature. Used for time series only.
#'       Defaults to \code{FALSE}.
#'   }
#' @param treatAsExponential character. Optional. Defaults to "auto". Used to specify whether to
#'   treat data as exponential trend and apply transformations like log-transform. Use values
#'   from \code{TreatAsExponential} enum.
#' @param differencingMethod character. Optional. Defaults to "auto". Used to specify differencing
#'   method to apply if data is stationary. Use values from \code{DifferencingMethod}.
#' @param periodicities list. Optional. A list of periodicities for different times. Must be
#'   specified as a list of lists, where each list item specifies the `timeSteps` for a
#'   particular `timeUnit`. Should be "ROW" if \code{windowsBasisUnit} is "ROW".
#' @param windowsBasisUnit character. Optional. Indicates which unit is the basis for the feature
#'   derivation window and forecast window. Valid options are a time unit (see \code{TimeUnit})
#'   or "ROW".
#' @param forecastWindowStart integer. Optional. Offset into the future to define how far forward
#'   relative to the forecast point the forecast window should start. Only used for time series
#'   projects. Expressed in terms of the \code{timeUnit} of the \code{datetimePartitionColumn}.
#' @param forecastWindowEnd integer. Optional. Offset into the future to define how far forward
#'   relative to the forecast point the forecast window should end. Only used for time series
#'   projects. Expressed in terms of the \code{timeUnit} of the \code{datetimePartitionColumn}.
#' @param multiseriesIdColumns list. A list of the names of multiseries id columns to define series
#' @param useCrossSeries logical. If \code{TRUE}, cross series features will be included. For
#'   details, see "Calculating features across series" in the time series section of the
#'   DataRobot user guide.
#' @param aggregationType character. Optional. The aggregation type to apply when creating cross
#'   series features. Must be either "total" or "average". See \code{SeriesAggregationType}.
#' @param calendar character. Optional. Either the calendar object or calendar id to use
#'   for this project.
#' @param crossSeriesGroupByColumns character. Optional. Column to split a cross series into
#'   further groups. For example, if every series is sales of an individual product, the cross
#'   series group could be e product category with values like "men's clothing", "sports
#'   equipment", etc. Requires multiseries with \code{useCrossSeries} enabled.
#' @return An S3 object of class 'partition' including the parameters required by the
#'   SetTarget function to generate a datetime partitioning of the modeling dataset.
#' @examples
#' CreateDatetimePartitionSpecification("date_col")
#' CreateDatetimePartitionSpecification("date",
#'                                      featureSettings = list(
#'                                        list("featureName" = "Product_offers",
#'                                             "defaultToKnownInAdvance" = TRUE)))
#' partition <- CreateDatetimePartitionSpecification("dateColumn",
#'                                                 treatAsExponential = TreatAsExponential$Always,
#'                                                 differencingMethod = DifferencingMethod$Seasonal,
#'                                                 periodicities = list(list("timeSteps" = 10,
#'                                                                           "timeUnit" = "HOUR"),
#'                                                                      list("timeSteps" = 600,
#'                                                                           "timeUnit" = "MINUTE"),
#'                                                                      list("timeSteps" = 7,
#'                                                                           "timeUnit" = "DAY")))
#' @export
CreateDatetimePartitionSpecification <- function(datetimePartitionColumn,
                                                 autopilotDataSelectionMethod = NULL,
                                                 validationDuration = NULL,
                                                 holdoutStartDate = NULL,
                                                 holdoutDuration = NULL,
                                                 disableHoldout = NULL,
                                                 gapDuration = NULL,
                                                 numberOfBacktests = NULL,
                                                 backtests = NULL,
                                                 useTimeSeries = FALSE,
                                                 defaultToKnownInAdvance = FALSE,
                                                 featureDerivationWindowStart = NULL,
                                                 featureDerivationWindowEnd = NULL,
                                                 featureSettings = NULL,
                                                 treatAsExponential = NULL,
                                                 differencingMethod = NULL,
                                                 windowsBasisUnit = NULL,
                                                 periodicities = NULL,
                                                 forecastWindowStart = NULL,
                                                 forecastWindowEnd = NULL,
                                                 multiseriesIdColumns = NULL,
                                                 useCrossSeries = NULL,
                                                 aggregationType = NULL,
                                                 crossSeriesGroupByColumns = NULL,
                                                 calendar = NULL) {
  if (is(calendar, "dataRobotCalendar")) {
    calendarId <- ValidateCalendar(calendar)
  } else if (IsId(calendar) || is.null(calendar)) {
    calendarId <- calendar
  } else {
    stop("Invalid calendar specification.")
  }

  partition <- list(cvMethod = cvMethods$DATETIME)
  partition$datetimePartitionColumn <- datetimePartitionColumn
  partition$autopilotDataSelectionMethod <- autopilotDataSelectionMethod
  partition$validationDuration <- validationDuration
  partition$holdoutStartDate <- holdoutStartDate
  partition$holdoutDuration <- holdoutDuration
  partition$disableHoldout <- disableHoldout
  partition$gapDuration <- gapDuration
  partition$numberOfBacktests <- numberOfBacktests
  partition$backtests <- backtests
  partition$useTimeSeries <- useTimeSeries
  partition$defaultToKnownInAdvance <- defaultToKnownInAdvance
  partition$featureDerivationWindowStart <- featureDerivationWindowStart
  partition$featureDerivationWindowEnd <- featureDerivationWindowEnd
  partition$featureSettings <- featureSettings
  partition$treatAsExponential <- treatAsExponential
  partition$differencingMethod <- differencingMethod
  partition$periodicities <- periodicities
  partition$windowsBasisUnit <- windowsBasisUnit
  partition$forecastWindowStart <- forecastWindowStart
  partition$forecastWindowEnd <- forecastWindowEnd
  partition$multiseriesIdColumns <- multiseriesIdColumns
  partition$useCrossSeriesFeatures <- useCrossSeries
  partition$aggregationType <- aggregationType
  partition$calendarId <- calendarId
  partition$crossSeriesGroupByColumns <- crossSeriesGroupByColumns
  class(partition) <- "partition"
  partition
}

as.dataRobotDatetimePartitionSpecification <- function(inList) {
  elements <- c("cvMethod",
                "datetimePartitionColumn",
                "autopilotDataSelectionMethod",
                "validationDuration",
                "holdoutStartDate",
                "holdoutDuration",
                "disableHoldout",
                "gapDuration",
                "numberOfBacktests",
                "backtests",
                "useTimeSeries",
                "defaultToKnownInAdvance",
                "featureDerivationWindowStart",
                "featureDerivationWindowEnd",
                "featureSettings",
                "treatAsExponential",
                "differencingMethod",
                "windowsBasisUnit",
                "periodicities",
                "forecastWindowStart",
                "forecastWindowEnd",
                "multiseriesIdColumns",
                "numberOfKnownInAdvanceFeatures",
                "useCrossSeriesFeatures",
                "aggregationType",
                "calendarId",
                "crossSeriesGroupByColumns")
  outList <- ApplySchema(inList, elements)
  featureSettings <- c("featureName", "knownInAdvance", "doNotDerive")
  if (!is.null(outList$featureSettings) && !is.null(names(outList$featureSettings))) {
    outList$featureSettings <- list(outList$featureSettings)
  }
  outList$featureSettings <- lapply(outList$featureSettings, ApplySchema, featureSettings)
  if (!is.null(outList$backtests)) {
    if (is.list(outList$backtests)) {
    outList$backtests <- lapply(outList$backtests, as.dataRobotBacktestSpecification)
    } else if (is.data.frame(outList$backtests)) {
      outList$backtests <- as.dataRobotBacktestSpecification(outList$backtests)
    }
  }
  outList
}


#' Preview the full partitioning determined by a DatetimePartitioningSpecification
#'
#' Based on the project dataset and the partitioning specification, inspect the full
#' partitioning that would be used if the same specification were passed into SetTarget.
#' This is not intended to be passed to SetTarget.
#'
#' @inheritParams DeleteProject
#' @param spec list. Datetime partition specification returned by
#'   \code{CreateDatetimePartitionSpecification}
#' @return list describing datetime partition with following components
#' \itemize{
#'   \item cvMethod. The type of validation scheme used for the project.
#'   \item projectId character. The id of the project this partitioning applies to.
#'   \item datetimePartitionColumn character. The name of the column whose values
#'     as dates are used to assign a row to a particular partition.
#'   \item dateFormat character. The format (e.g. "%Y-%m-%d %H:%M:%S") by which the
#'     partition column was interpreted (compatible with strftime
#'     [https://docs.python.org/2/library/time.html#time.strftime]).
#'   \item autopilotDataSelectionMethod character. Whether models created
#'     by the autopilot use "rowCount" or "duration" as their dataSelectionMethod.
#'   \item validationDuration character. The validation duration specified when
#'     initializing the partitioning - not directly significant if the backtests have been
#'     modified, but used as the default validationDuration for the backtests.
#'   \item availableTrainingStartDate character. The start date of the available training
#'     data for scoring the holdout.
#'   \item availableTrainingDuration character. The duration of the available training data
#'     for scoring the holdout.
#'   \item availableTrainingRowCount integer. The number of rows in the available training data for
#'     scoring the holdout. Only available when retrieving the partitioning after setting the
#'     target.
#'   \item availableTrainingEndDate character. The end date of the available training data
#'     for scoring the holdout.
#'   \item primaryTrainingStartDate character. The start date of primary training data for
#'     scoring the holdout.
#'   \item primaryTrainingDuration character. The duration of the primary training data for
#'     scoring the holdout.
#'   \item primaryTrainingRowCount integer. The number of rows in the primary training data for
#'     scoring the holdout. Only available when retrieving the partitioning after setting the
#'     target.
#'   \item primaryTrainingEndDate character. The end date of the primary training data for
#'     scoring the holdout.
#'   \item gapStartDate character. The start date of the gap between training and holdout
#'     scoring data.
#'   \item gapDuration character. The duration of the gap between training and holdout
#'     scoring data.
#'   \item gapRowCount integer. The number of rows in the gap between training and holdout scoring
#'     data.
#'   Only available when retrieving the partitioning after setting the target.
#'   \item gapEndDate character. The end date of the gap between training and holdout scoring
#'     data.
#'   \item holdoutStartDate character. The start date of holdout scoring data.
#'   \item holdoutDuration character. The duration of the holdout scoring data.
#'   \item holdoutRowCount integer. The number of rows in the holdout scoring data.
#'     Only available when retrieving the partitioning after setting the target.
#'   \item holdoutEndDate character. The end date of the holdout scoring data.
#'   \item numberOfBacktests integer. the number of backtests used.
#'   \item backtests data.frame. A data frame of partition backtest. Each element represent one
#'     backtest and has the following components:
#'     index, availableTrainingStartDate, availableTrainingDuration, availableTrainingRowCount,
#'     availableTrainingEndDate, primaryTrainingStartDate, primaryTrainingDuration,
#'     primaryTrainingRowCount, primaryTrainingEndDate, gapStartDate,  gapDuration, gapRowCount,
#'     gapEndDate, validationStartDate, validationDuration, validationRowCount,
#'     validationEndDate, totalRowCount.
#'   \item useTimeSeries logical. Whether the project is a time series project (if TRUE) or an OTV
#'     project which uses datetime partitioning (if FALSE).
#'   \item defaultToKnownInAdvance logical. Whether the project defaults to treating
#'     features as known in advance. Known in advance features are time series features that
#'     are expected to be known for dates in the future when making predictions (e.g., "is
#'     this a holiday").
#'   \item featureDerivationWindowStart integer. Offset into the past to define how far
#'     back relative to the forecast point the feature derivation window should start. Only used for
#'     time series projects. Expressed in terms of the \code{timeUnit} of the
#'     \code{datetimePartitionColumn}.
#'   \item featureDerivationWindowEnd integer. Offset into the past to define how far back relative
#'     to the forecast point the feature derivation window should end. Only used for
#'     time series projects. Expressed in terms of the \code{timeUnit} of the
#'     \code{datetimePartitionColumn}.
#'   \item forecastWindowStart integer. Offset into the future to define how far forward relative
#'     to the forecast point the forecast window should start. Only used for time series
#'     projects. Expressed in terms of the \code{timeUnit} of the \code{datetimePartitionColumn}.
#'   \item forecastWindowEnd integer. Offset into the future to define how far forward relative to
#'     the forecast point the forecast window should end. Only used for time series
#'     projects. Expressed in terms of the \code{timeUnit} of the \code{datetimePartitionColumn}.
#'   \item featureSettings list. A list of lists specifying settings for each feature. For each
#'     feature you would like to set feature settings for, pass the following in a list:
#'     \itemize{
#'       \item featureName character. The name of the feature to set feature settings.
#'       \item knownInAdvance logical. Optional. Whether or not the feature is known in advance.
#'         Used for time series only. Defaults to \code{FALSE}.
#'       \item doNotDerive logical. Optional. If \code{TRUE}, no time series derived features
#'         (e.g., lags) will be automatically engineered from this feature. Used for time series
#'         only. Defaults to \code{FALSE}.
#'     }
#'   \item treatAsExponential character. Specifies whether to treat data as exponential trend
#'     and apply transformations like log-transform. Uses values from from
#'     \code{TreatAsExponential}.
#'   \item differencingMethod character. Used to specify differencing method to apply if data is
#'     stationary. Use values from \code{DifferencingMethod}.
#'   \item windowsBasisUnit character. Indicates which unit is the basis for the feature derivation
#'    window and forecast window. Uses values from \code{TimeUnit} and the value "ROW".
#'   \item periodicities list. A list of periodicities for different times, specified as a list of
#'    lists, where each list item specifies the `timeSteps` for a particular `timeUnit`. Will be
#'     "ROW" if \code{windowsBasisUnit} is "ROW".
#'   \item totalRowCount integer. The number of rows in the project dataset. Only available when
#'     retrieving the partitioning after setting the target. Thus it will be NULL for
#'     \code{GenerateDatetimePartition} and populated for \code{GetDatetimePartition}.
#'   \item validationRowCount integer. The number of rows in the validation set.
#'   \item multiseriesIdColumns list. A list of the names of multiseries id columns to define
#'     series.
#'   \item numberOfKnownInAdvanceFeatures integer. The number of known in advance features.
#'   \item useCrossSeriesFeatures logical. Whether or not cross series features are included.
#'   \item aggregationType character. The aggregation type to apply when creating cross series
#'     features. See \code{SeriesAggregationType}.
#'   \item calendarId character. The ID of the calendar used for this project, if any.
#'   }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   partitionSpec <- CreateDatetimePartitionSpecification("date_col")
#'   GenerateDatetimePartition(projectId, partitionSpec)
#' }
#' @export
GenerateDatetimePartition <- function(project, spec) {
  projectId <- ValidateProject(project)
  spec$cvMethod <- NULL
  routeString <- UrlJoin("projects", projectId, "datetimePartitioning")
  postResponse <- DataRobotPOST(routeString, body = spec, encode = "json")
  postResponse$cvMethod <- cvMethods$DATETIME
  as.dataRobotDatetimePartition(postResponse)
}


#' Retrieve the DatetimePartitioning from a project
#'
#' Only available if the project has already set the target as a datetime project.
#'
#' @inheritParams DeleteProject
#' @inherit GenerateDatetimePartition return
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   GetDatetimePartition(projectId)
#' }
#' @export
GetDatetimePartition <- function(project) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "datetimePartitioning")
  part <- DataRobotGET(routeString)
  part$cvMethod <- cvMethods$DATETIME
  as.dataRobotDatetimePartition(part)
}


as.dataRobotDatetimePartition <- function(inList) {
  elements <- c("cvMethod",
                "projectId",
                "datetimePartitionColumn",
                "dateFormat",
                "autopilotDataSelectionMethod",
                "validationDuration",
                "availableTrainingStartDate",
                "availableTrainingDuration",
                "availableTrainingRowCount",
                "availableTrainingEndDate",
                "primaryTrainingStartDate",
                "primaryTrainingDuration",
                "primaryTrainingRowCount",
                "primaryTrainingEndDate",
                "gapStartDate",
                "gapDuration",
                "gapRowCount",
                "gapEndDate",
                "holdoutStartDate",
                "holdoutDuration",
                "holdoutRowCount",
                "holdoutEndDate",
                "numberOfBacktests",
                "backtests",
                "useTimeSeries",
                "defaultToKnownInAdvance",
                "featureDerivationWindowStart",
                "featureDerivationWindowEnd",
                "forecastWindowStart",
                "forecastWindowEnd",
                "featureSettings",
                "treatAsExponential",
                "differencingMethod",
                "windowsBasisUnit",
                "periodicities",
                "totalRowCount",
                "validationRowCount",
                "multiseriesIdColumns",
                "numberOfKnownInAdvanceFeatures",
                "useCrossSeriesFeatures",
                "aggregationType",
                "calendarId")
  outList <- ApplySchema(inList, elements)
  if (!is.null(outList$featureSettings) && !is.null(names(outList$featureSettings))) {
    outList$featureSettings <- list(outList$featureSettings)
  }
  featureSettings <- c("featureName", "knownInAdvance", "doNotDerive")
  outList$featureSettings <- lapply(outList$featureSettings, ApplySchema, featureSettings)
  backtestElements <- c("index", "validationRowCount", "primaryTrainingDuration",
                        "primaryTrainingEndDate", "availableTrainingStartDate",
                        "primaryTrainingStartDate", "validationEndDate",
                        "availableTrainingDuration", "availableTrainingRowCount",
                        "gapEndDate", "validationDuration", "gapStartDate",
                        "availableTrainingEndDate", "primaryTrainingRowCount",
                        "validationStartDate", "totalRowCount", "gapRowCount", "gapDuration")
  outList$backtests <- ApplySchema(outList$backtests, backtestElements)
  outList$isTimeSeries <- isTRUE(outList$useTimeSeries)
  outList$isMultiSeries <- length(outList$multiseriesIdColumns) > 0
  outList$isCrossSeries <- isTRUE(outList$useCrossSeriesFeatures)
  outList
}
