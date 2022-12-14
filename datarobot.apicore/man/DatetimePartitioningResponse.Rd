% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datetime_partitioning_response.R
\docType{class}
\name{DatetimePartitioningResponse}
\alias{DatetimePartitioningResponse}
\title{DatetimePartitioningResponse}
\format{
An \code{R6Class} generator object
}
\description{
DatetimePartitioningResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{aggregationType}}{character \link{optional} For multiseries projects only. The aggregation type to apply when creating cross-series features.}

\item{\code{autopilotDataSelectionMethod}}{character The Data Selection method to be used by autopilot when creating models for datetime-partitioned datasets.}

\item{\code{availableTrainingDuration}}{character The duration of available training duration for scoring the holdout.}

\item{\code{availableTrainingEndDate}}{character The end date of available training data for scoring the holdout.}

\item{\code{availableTrainingStartDate}}{character The start date of available training data for scoring the holdout.}

\item{\code{backtests}}{list( \link{BacktestResponse} ) An array of the configured backtests.}

\item{\code{calendarId}}{character \link{optional} The ID of the calendar to be used in this project.}

\item{\code{calendarName}}{character \link{optional} The name of the calendar used in this project.}

\item{\code{crossSeriesGroupByColumns}}{list( character ) \link{optional} For multiseries projects with cross-series features enabled only. List of columns (currently of length 1). Setting that indicates how to further split series into related groups. For example, if every series is sales of an individual product, the series group-by could be the product category with values like \"men's clothing\", \"sports equipment\", etc.}

\item{\code{dateFormat}}{character The date format of the partition column.}

\item{\code{datetimePartitionColumn}}{character The date column that will be used as a datetime partition column.}

\item{\code{defaultToAPriori}}{character Renamed to `defaultToKnownInAdvance`.}

\item{\code{defaultToDoNotDerive}}{character For time series projects only. Sets whether all features default to being treated as do-not-derive features, excluding them from feature derivation. Individual features can be set to a value different than the default by using the `featureSettings` parameter.}

\item{\code{defaultToKnownInAdvance}}{character For time series projects only. Sets whether all features default to being treated as known in advance features, which are features that are known into the future. Features marked as known in advance must be specified into the future when making predictions. The default is false, all features are not known in advance. Individual features can be set to a value different than the default using the `featureSettings` parameter. See the :ref:`Time Series Overview <time_series_overview>` for more context.}

\item{\code{differencingMethod}}{character For time series projects only. Used to specify which differencing method to apply if the data is stationary. For classification problems `simple` and `seasonal` are not allowed. Parameter `periodicities` must be specified if `seasonal` is chosen. Defaults to `auto`.}

\item{\code{disableHoldout}}{character A boolean value indicating whether date partitioning skipped allocating a holdout fold.}

\item{\code{featureDerivationWindowEnd}}{integer For time series projects only. How many timeUnits of the datetimePartitionColumn into the past relative to the forecast point the feature derivation window should end.}

\item{\code{featureDerivationWindowStart}}{integer For time series projects only. How many timeUnits of the datetimePartitionColumn into the past relative to the forecast point the feature derivation window should begin.}

\item{\code{featureSettings}}{list( \link{FeatureSetting} ) An array specifying per feature settings. Features can be left unspecified.}

\item{\code{forecastWindowEnd}}{integer For time series projects only. How many timeUnits of the datetimePartitionColumn into the future relative to the forecast point the forecast window should end.}

\item{\code{forecastWindowStart}}{integer For time series projects only. How many timeUnits of the datetimePartitionColumn into the future relative to the forecast point the forecast window should start.}

\item{\code{gapDuration}}{character The duration of the gap between the training and holdout scoring data.}

\item{\code{gapEndDate}}{character The end date of the gap between the training and holdout scoring data.}

\item{\code{gapStartDate}}{character The start date of the gap between the training and holdout scoring data.}

\item{\code{holdoutDuration}}{character The duration of the holdout scoring data.}

\item{\code{holdoutEndDate}}{character The end date of holdout scoring data.}

\item{\code{holdoutStartDate}}{character The start date of holdout scoring data.}

\item{\code{isHoldoutModified}}{character \link{optional} A boolean value indicating whether holdout settings (start/end dates) have been modified by user.}

\item{\code{modelSplits}}{integer \link{optional} Sets the cap on the number of jobs per model used when building models to control number of jobs in the queue. Higher number of modelSplits will allow for less downsampling leading to the use of more post-processed data.}

\item{\code{multiseriesIdColumns}}{list( character ) May be used only with time series projects. An array of the column names identifying  the series to which each row of the dataset belongs. Currently only one multiseries ID column is supported. See the :ref:`multiseries <multiseries>` section of the time series documentation for more context.}

\item{\code{numberOfBacktests}}{integer The number of backtests to use. If omitted, defaults to a positive value selected by the server based on the validation and gap durations.}

\item{\code{numberOfDoNotDeriveFeatures}}{integer Number of features that are marked as \"do not derive\".}

\item{\code{numberOfKnownInAdvanceFeatures}}{integer Number of features that are marked as \"known in advance\".}

\item{\code{partitioningExtendedWarnings}}{list( \link{PartitioningExtendedWarning} ) \link{optional} An array of available warnings about potential problems with the chosen partitioning that could cause issues during modeling, although the partitioning may be successfully submitted.}

\item{\code{partitioningWarnings}}{list( \link{PartitioningWarning} ) An array of available warnings about potential problems with the chosen partitioning that could cause issues during modeling, although the partitioning may be successfully submitted.}

\item{\code{periodicities}}{list( \link{Periodicity} ) A list of periodicities for time series projects only. For classification problems periodicities are not allowed. If this is provided, parameter 'differencing_method' will default to 'seasonal' if not provided or 'auto'.}

\item{\code{primaryTrainingDuration}}{character The duration of primary training duration for scoring the holdout.}

\item{\code{primaryTrainingEndDate}}{character The end date of primary training data for scoring the holdout.}

\item{\code{primaryTrainingStartDate}}{character The start date of primary training data for scoring the holdout.}

\item{\code{projectId}}{character The ID of the project.}

\item{\code{treatAsExponential}}{character For time series projects only. Used to specify whether to treat data as exponential trend and apply transformations like log-transform. For classification problems `always` is not allowed. Defaults to `auto`.}

\item{\code{useCrossSeriesFeatures}}{character \link{optional} For multiseries projects only. Indicating whether to use cross-series features.}

\item{\code{useTimeSeries}}{character A boolean value indicating whether a time series project should be created instead of a regular project which uses datetime partitioning.}

\item{\code{validationDuration}}{character The default validation duration for all backtests. If the primary date/time feature in a time series project is irregular, you cannot set a default validation length. Instead, set each duration individually.}

\item{\code{windowsBasisUnit}}{character For time series projects only. Indicates which unit is basis for feature derivation window and forecast window. Valid options are detected time unit or `ROW`. If omitted, the default value is detected time unit.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-DatetimePartitioningResponse-new}{\code{DatetimePartitioningResponse$new()}}
\item \href{#method-DatetimePartitioningResponse-validate}{\code{DatetimePartitioningResponse$validate()}}
\item \href{#method-DatetimePartitioningResponse-toJSON}{\code{DatetimePartitioningResponse$toJSON()}}
\item \href{#method-DatetimePartitioningResponse-fromJSON}{\code{DatetimePartitioningResponse$fromJSON()}}
\item \href{#method-DatetimePartitioningResponse-clone}{\code{DatetimePartitioningResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DatetimePartitioningResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-DatetimePartitioningResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DatetimePartitioningResponse$new(
  autopilotDataSelectionMethod = NULL,
  availableTrainingDuration = NULL,
  availableTrainingEndDate = NULL,
  availableTrainingStartDate = NULL,
  backtests = NULL,
  dateFormat = NULL,
  datetimePartitionColumn = NULL,
  defaultToAPriori = NULL,
  defaultToDoNotDerive = NULL,
  defaultToKnownInAdvance = NULL,
  differencingMethod = NULL,
  disableHoldout = NULL,
  featureDerivationWindowEnd = NULL,
  featureDerivationWindowStart = NULL,
  featureSettings = NULL,
  forecastWindowEnd = NULL,
  forecastWindowStart = NULL,
  gapDuration = NULL,
  gapEndDate = NULL,
  gapStartDate = NULL,
  holdoutDuration = NULL,
  holdoutEndDate = NULL,
  holdoutStartDate = NULL,
  multiseriesIdColumns = NULL,
  numberOfBacktests = NULL,
  numberOfDoNotDeriveFeatures = NULL,
  numberOfKnownInAdvanceFeatures = NULL,
  partitioningWarnings = NULL,
  periodicities = NULL,
  primaryTrainingDuration = NULL,
  primaryTrainingEndDate = NULL,
  primaryTrainingStartDate = NULL,
  projectId = NULL,
  treatAsExponential = NULL,
  useTimeSeries = NULL,
  validationDuration = NULL,
  windowsBasisUnit = NULL,
  aggregationType = NULL,
  calendarId = NULL,
  calendarName = NULL,
  crossSeriesGroupByColumns = NULL,
  isHoldoutModified = NULL,
  modelSplits = NULL,
  partitioningExtendedWarnings = NULL,
  useCrossSeriesFeatures = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{autopilotDataSelectionMethod}}{The Data Selection method to be used by autopilot when creating models for datetime-partitioned datasets.}

\item{\code{availableTrainingDuration}}{The duration of available training duration for scoring the holdout.}

\item{\code{availableTrainingEndDate}}{The end date of available training data for scoring the holdout.}

\item{\code{availableTrainingStartDate}}{The start date of available training data for scoring the holdout.}

\item{\code{backtests}}{An array of the configured backtests.}

\item{\code{dateFormat}}{The date format of the partition column.}

\item{\code{datetimePartitionColumn}}{The date column that will be used as a datetime partition column.}

\item{\code{defaultToAPriori}}{Renamed to `defaultToKnownInAdvance`.}

\item{\code{defaultToDoNotDerive}}{For time series projects only. Sets whether all features default to being treated as do-not-derive features, excluding them from feature derivation. Individual features can be set to a value different than the default by using the `featureSettings` parameter.}

\item{\code{defaultToKnownInAdvance}}{For time series projects only. Sets whether all features default to being treated as known in advance features, which are features that are known into the future. Features marked as known in advance must be specified into the future when making predictions. The default is false, all features are not known in advance. Individual features can be set to a value different than the default using the `featureSettings` parameter. See the :ref:`Time Series Overview <time_series_overview>` for more context.}

\item{\code{differencingMethod}}{For time series projects only. Used to specify which differencing method to apply if the data is stationary. For classification problems `simple` and `seasonal` are not allowed. Parameter `periodicities` must be specified if `seasonal` is chosen. Defaults to `auto`.}

\item{\code{disableHoldout}}{A boolean value indicating whether date partitioning skipped allocating a holdout fold.}

\item{\code{featureDerivationWindowEnd}}{For time series projects only. How many timeUnits of the datetimePartitionColumn into the past relative to the forecast point the feature derivation window should end.}

\item{\code{featureDerivationWindowStart}}{For time series projects only. How many timeUnits of the datetimePartitionColumn into the past relative to the forecast point the feature derivation window should begin.}

\item{\code{featureSettings}}{An array specifying per feature settings. Features can be left unspecified.}

\item{\code{forecastWindowEnd}}{For time series projects only. How many timeUnits of the datetimePartitionColumn into the future relative to the forecast point the forecast window should end.}

\item{\code{forecastWindowStart}}{For time series projects only. How many timeUnits of the datetimePartitionColumn into the future relative to the forecast point the forecast window should start.}

\item{\code{gapDuration}}{The duration of the gap between the training and holdout scoring data.}

\item{\code{gapEndDate}}{The end date of the gap between the training and holdout scoring data.}

\item{\code{gapStartDate}}{The start date of the gap between the training and holdout scoring data.}

\item{\code{holdoutDuration}}{The duration of the holdout scoring data.}

\item{\code{holdoutEndDate}}{The end date of holdout scoring data.}

\item{\code{holdoutStartDate}}{The start date of holdout scoring data.}

\item{\code{multiseriesIdColumns}}{May be used only with time series projects. An array of the column names identifying  the series to which each row of the dataset belongs. Currently only one multiseries ID column is supported. See the :ref:`multiseries <multiseries>` section of the time series documentation for more context.}

\item{\code{numberOfBacktests}}{The number of backtests to use. If omitted, defaults to a positive value selected by the server based on the validation and gap durations.}

\item{\code{numberOfDoNotDeriveFeatures}}{Number of features that are marked as \"do not derive\".}

\item{\code{numberOfKnownInAdvanceFeatures}}{Number of features that are marked as \"known in advance\".}

\item{\code{partitioningWarnings}}{An array of available warnings about potential problems with the chosen partitioning that could cause issues during modeling, although the partitioning may be successfully submitted.}

\item{\code{periodicities}}{A list of periodicities for time series projects only. For classification problems periodicities are not allowed. If this is provided, parameter 'differencing_method' will default to 'seasonal' if not provided or 'auto'.}

\item{\code{primaryTrainingDuration}}{The duration of primary training duration for scoring the holdout.}

\item{\code{primaryTrainingEndDate}}{The end date of primary training data for scoring the holdout.}

\item{\code{primaryTrainingStartDate}}{The start date of primary training data for scoring the holdout.}

\item{\code{projectId}}{The ID of the project.}

\item{\code{treatAsExponential}}{For time series projects only. Used to specify whether to treat data as exponential trend and apply transformations like log-transform. For classification problems `always` is not allowed. Defaults to `auto`.}

\item{\code{useTimeSeries}}{A boolean value indicating whether a time series project should be created instead of a regular project which uses datetime partitioning.}

\item{\code{validationDuration}}{The default validation duration for all backtests. If the primary date/time feature in a time series project is irregular, you cannot set a default validation length. Instead, set each duration individually.}

\item{\code{windowsBasisUnit}}{For time series projects only. Indicates which unit is basis for feature derivation window and forecast window. Valid options are detected time unit or `ROW`. If omitted, the default value is detected time unit.}

\item{\code{aggregationType}}{For multiseries projects only. The aggregation type to apply when creating cross-series features.}

\item{\code{calendarId}}{The ID of the calendar to be used in this project.}

\item{\code{calendarName}}{The name of the calendar used in this project.}

\item{\code{crossSeriesGroupByColumns}}{For multiseries projects with cross-series features enabled only. List of columns (currently of length 1). Setting that indicates how to further split series into related groups. For example, if every series is sales of an individual product, the series group-by could be the product category with values like \"men's clothing\", \"sports equipment\", etc.}

\item{\code{isHoldoutModified}}{A boolean value indicating whether holdout settings (start/end dates) have been modified by user.}

\item{\code{modelSplits}}{Sets the cap on the number of jobs per model used when building models to control number of jobs in the queue. Higher number of modelSplits will allow for less downsampling leading to the use of more post-processed data.}

\item{\code{partitioningExtendedWarnings}}{An array of available warnings about potential problems with the chosen partitioning that could cause issues during modeling, although the partitioning may be successfully submitted.}

\item{\code{useCrossSeriesFeatures}}{For multiseries projects only. Indicating whether to use cross-series features.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DatetimePartitioningResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-DatetimePartitioningResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DatetimePartitioningResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DatetimePartitioningResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-DatetimePartitioningResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DatetimePartitioningResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DatetimePartitioningResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-DatetimePartitioningResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DatetimePartitioningResponse$fromJSON(
  DatetimePartitioningResponseJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{DatetimePartitioningResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DatetimePartitioningResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-DatetimePartitioningResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DatetimePartitioningResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
