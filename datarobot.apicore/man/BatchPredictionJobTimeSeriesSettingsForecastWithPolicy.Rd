% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/batch_prediction_job_time_series_settings_forecast_with_policy.R
\docType{class}
\name{BatchPredictionJobTimeSeriesSettingsForecastWithPolicy}
\alias{BatchPredictionJobTimeSeriesSettingsForecastWithPolicy}
\title{BatchPredictionJobTimeSeriesSettingsForecastWithPolicy}
\format{
An \code{R6Class} generator object
}
\description{
BatchPredictionJobTimeSeriesSettingsForecastWithPolicy Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{forecastPointPolicy}}{\link{JobRunTimeBasedForecastPointPolicy}}

\item{\code{relaxKnownInAdvanceFeaturesCheck}}{character \link{optional} If activated, missing values in the known in advance features are allowed in the forecast window at prediction time. If omitted or false, missing values are not allowed.}

\item{\code{type}}{character Forecast mode makes predictions using forecastPoint or rows in the dataset without target.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-BatchPredictionJobTimeSeriesSettingsForecastWithPolicy-new}{\code{BatchPredictionJobTimeSeriesSettingsForecastWithPolicy$new()}}
\item \href{#method-BatchPredictionJobTimeSeriesSettingsForecastWithPolicy-validate}{\code{BatchPredictionJobTimeSeriesSettingsForecastWithPolicy$validate()}}
\item \href{#method-BatchPredictionJobTimeSeriesSettingsForecastWithPolicy-toJSON}{\code{BatchPredictionJobTimeSeriesSettingsForecastWithPolicy$toJSON()}}
\item \href{#method-BatchPredictionJobTimeSeriesSettingsForecastWithPolicy-fromJSON}{\code{BatchPredictionJobTimeSeriesSettingsForecastWithPolicy$fromJSON()}}
\item \href{#method-BatchPredictionJobTimeSeriesSettingsForecastWithPolicy-clone}{\code{BatchPredictionJobTimeSeriesSettingsForecastWithPolicy$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BatchPredictionJobTimeSeriesSettingsForecastWithPolicy-new"></a>}}
\if{latex}{\out{\hypertarget{method-BatchPredictionJobTimeSeriesSettingsForecastWithPolicy-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BatchPredictionJobTimeSeriesSettingsForecastWithPolicy$new(
  forecastPointPolicy = NULL,
  type = NULL,
  relaxKnownInAdvanceFeaturesCheck = FALSE,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{type}}{Forecast mode makes predictions using forecastPoint or rows in the dataset without target.}

\item{\code{relaxKnownInAdvanceFeaturesCheck}}{If activated, missing values in the known in advance features are allowed in the forecast window at prediction time. If omitted or false, missing values are not allowed.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BatchPredictionJobTimeSeriesSettingsForecastWithPolicy-validate"></a>}}
\if{latex}{\out{\hypertarget{method-BatchPredictionJobTimeSeriesSettingsForecastWithPolicy-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BatchPredictionJobTimeSeriesSettingsForecastWithPolicy$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BatchPredictionJobTimeSeriesSettingsForecastWithPolicy-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-BatchPredictionJobTimeSeriesSettingsForecastWithPolicy-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BatchPredictionJobTimeSeriesSettingsForecastWithPolicy$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BatchPredictionJobTimeSeriesSettingsForecastWithPolicy-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-BatchPredictionJobTimeSeriesSettingsForecastWithPolicy-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BatchPredictionJobTimeSeriesSettingsForecastWithPolicy$fromJSON(
  BatchPredictionJobTimeSeriesSettingsForecastWithPolicyJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{BatchPredictionJobTimeSeriesSettingsForecastWithPolicyJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BatchPredictionJobTimeSeriesSettingsForecastWithPolicy-clone"></a>}}
\if{latex}{\out{\hypertarget{method-BatchPredictionJobTimeSeriesSettingsForecastWithPolicy-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BatchPredictionJobTimeSeriesSettingsForecastWithPolicy$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
