% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/feature_metrics_response.R
\docType{class}
\name{FeatureMetricsResponse}
\alias{FeatureMetricsResponse}
\title{FeatureMetricsResponse}
\format{
An \code{R6Class} generator object
}
\description{
FeatureMetricsResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{availableMetrics}}{list( character ) an array of strings representing the appropriate metrics.  If the feature cannot be selected as the target, then this array will be empty.}

\item{\code{featureName}}{character the name of the feature that was looked up}

\item{\code{metricDetails}}{list( \link{FeatureMetricDetailsResponse} ) the list of metricDetails objects.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-FeatureMetricsResponse-new}{\code{FeatureMetricsResponse$new()}}
\item \href{#method-FeatureMetricsResponse-validate}{\code{FeatureMetricsResponse$validate()}}
\item \href{#method-FeatureMetricsResponse-toJSON}{\code{FeatureMetricsResponse$toJSON()}}
\item \href{#method-FeatureMetricsResponse-fromJSON}{\code{FeatureMetricsResponse$fromJSON()}}
\item \href{#method-FeatureMetricsResponse-clone}{\code{FeatureMetricsResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FeatureMetricsResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-FeatureMetricsResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FeatureMetricsResponse$new(
  availableMetrics = NULL,
  featureName = NULL,
  metricDetails = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{availableMetrics}}{an array of strings representing the appropriate metrics.  If the feature cannot be selected as the target, then this array will be empty.}

\item{\code{featureName}}{the name of the feature that was looked up}

\item{\code{metricDetails}}{the list of metricDetails objects.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FeatureMetricsResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-FeatureMetricsResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FeatureMetricsResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FeatureMetricsResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-FeatureMetricsResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FeatureMetricsResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FeatureMetricsResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-FeatureMetricsResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FeatureMetricsResponse$fromJSON(
  FeatureMetricsResponseJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{FeatureMetricsResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FeatureMetricsResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-FeatureMetricsResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FeatureMetricsResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
