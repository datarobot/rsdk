% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/batch_prediction_job_time_series_settings_historical.R
\docType{class}
\name{BatchPredictionJobTimeSeriesSettingsHistorical}
\alias{BatchPredictionJobTimeSeriesSettingsHistorical}
\title{BatchPredictionJobTimeSeriesSettingsHistorical}
\format{
An \code{R6Class} generator object
}
\description{
BatchPredictionJobTimeSeriesSettingsHistorical Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{predictionsEndDate}}{character \link{optional} Used for historical predictions in order to override date to which predictions should be calculated. By default value will be inferred automatically from the dataset.}

\item{\code{predictionsStartDate}}{character \link{optional} Used for historical predictions in order to override date from which predictions should be calculated. By default value will be inferred automatically from the dataset.}

\item{\code{relaxKnownInAdvanceFeaturesCheck}}{character \link{optional} If activated, missing values in the known in advance features are allowed in the forecast window at prediction time. If omitted or false, missing values are not allowed.}

\item{\code{type}}{character Historical mode enables bulk predictions which calculates predictions for all possible forecast points and forecast distances in the dataset within the predictionsStartDate/predictionsEndDate range.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-BatchPredictionJobTimeSeriesSettingsHistorical-new}{\code{BatchPredictionJobTimeSeriesSettingsHistorical$new()}}
\item \href{#method-BatchPredictionJobTimeSeriesSettingsHistorical-validate}{\code{BatchPredictionJobTimeSeriesSettingsHistorical$validate()}}
\item \href{#method-BatchPredictionJobTimeSeriesSettingsHistorical-toJSON}{\code{BatchPredictionJobTimeSeriesSettingsHistorical$toJSON()}}
\item \href{#method-BatchPredictionJobTimeSeriesSettingsHistorical-fromJSON}{\code{BatchPredictionJobTimeSeriesSettingsHistorical$fromJSON()}}
\item \href{#method-BatchPredictionJobTimeSeriesSettingsHistorical-clone}{\code{BatchPredictionJobTimeSeriesSettingsHistorical$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BatchPredictionJobTimeSeriesSettingsHistorical-new"></a>}}
\if{latex}{\out{\hypertarget{method-BatchPredictionJobTimeSeriesSettingsHistorical-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BatchPredictionJobTimeSeriesSettingsHistorical$new(
  type = NULL,
  predictionsEndDate = NULL,
  predictionsStartDate = NULL,
  relaxKnownInAdvanceFeaturesCheck = FALSE,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{type}}{Historical mode enables bulk predictions which calculates predictions for all possible forecast points and forecast distances in the dataset within the predictionsStartDate/predictionsEndDate range.}

\item{\code{predictionsEndDate}}{Used for historical predictions in order to override date to which predictions should be calculated. By default value will be inferred automatically from the dataset.}

\item{\code{predictionsStartDate}}{Used for historical predictions in order to override date from which predictions should be calculated. By default value will be inferred automatically from the dataset.}

\item{\code{relaxKnownInAdvanceFeaturesCheck}}{If activated, missing values in the known in advance features are allowed in the forecast window at prediction time. If omitted or false, missing values are not allowed.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BatchPredictionJobTimeSeriesSettingsHistorical-validate"></a>}}
\if{latex}{\out{\hypertarget{method-BatchPredictionJobTimeSeriesSettingsHistorical-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BatchPredictionJobTimeSeriesSettingsHistorical$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BatchPredictionJobTimeSeriesSettingsHistorical-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-BatchPredictionJobTimeSeriesSettingsHistorical-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BatchPredictionJobTimeSeriesSettingsHistorical$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BatchPredictionJobTimeSeriesSettingsHistorical-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-BatchPredictionJobTimeSeriesSettingsHistorical-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BatchPredictionJobTimeSeriesSettingsHistorical$fromJSON(
  BatchPredictionJobTimeSeriesSettingsHistoricalJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{BatchPredictionJobTimeSeriesSettingsHistoricalJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BatchPredictionJobTimeSeriesSettingsHistorical-clone"></a>}}
\if{latex}{\out{\hypertarget{method-BatchPredictionJobTimeSeriesSettingsHistorical-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BatchPredictionJobTimeSeriesSettingsHistorical$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
