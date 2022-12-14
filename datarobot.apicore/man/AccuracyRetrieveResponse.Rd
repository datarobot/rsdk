% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accuracy_retrieve_response.R
\docType{class}
\name{AccuracyRetrieveResponse}
\alias{AccuracyRetrieveResponse}
\title{AccuracyRetrieveResponse}
\format{
An \code{R6Class} generator object
}
\description{
AccuracyRetrieveResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{metrics}}{object Accuracy metrics of the deployment, keyed by metric names,                                  each value is a `metric` object with detail on a single metric.                                  For classification deployments:                                  - LogLoss                                  - AUC                                  - Kolmogorov - Smirnov                                  - Gini Norm                                  - Rate @ Top10 \%                                  For regression deployments:                                  - RMSE                                  - MAE                                  - Gamma Deviance                                  - Tweedie Deviance                                  - R Squared}

\item{\code{modelId}}{character \link{optional} The id of the model for which metrics are being retrieved.}

\item{\code{period}}{\link{TimeRange}}

\item{\code{segmentAttribute}}{character \link{optional} The name of the segment on which segment analysis is being performed.}

\item{\code{segmentValue}}{character \link{optional} The value of the `segmentAttribute` to segment on.}

\item{\code{targetClasses}}{character \link{optional} Target classes provided in the request.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-AccuracyRetrieveResponse-new}{\code{AccuracyRetrieveResponse$new()}}
\item \href{#method-AccuracyRetrieveResponse-validate}{\code{AccuracyRetrieveResponse$validate()}}
\item \href{#method-AccuracyRetrieveResponse-toJSON}{\code{AccuracyRetrieveResponse$toJSON()}}
\item \href{#method-AccuracyRetrieveResponse-fromJSON}{\code{AccuracyRetrieveResponse$fromJSON()}}
\item \href{#method-AccuracyRetrieveResponse-clone}{\code{AccuracyRetrieveResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AccuracyRetrieveResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-AccuracyRetrieveResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AccuracyRetrieveResponse$new(
  metrics = NULL,
  period = NULL,
  modelId = NULL,
  segmentAttribute = NULL,
  segmentValue = "",
  targetClasses = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{metrics}}{Accuracy metrics of the deployment, keyed by metric names,                                  each value is a `metric` object with detail on a single metric.                                  For classification deployments:                                  - LogLoss                                  - AUC                                  - Kolmogorov - Smirnov                                  - Gini Norm                                  - Rate @ Top10 \%                                  For regression deployments:                                  - RMSE                                  - MAE                                  - Gamma Deviance                                  - Tweedie Deviance                                  - R Squared}

\item{\code{modelId}}{The id of the model for which metrics are being retrieved.}

\item{\code{segmentAttribute}}{The name of the segment on which segment analysis is being performed.}

\item{\code{segmentValue}}{The value of the `segmentAttribute` to segment on.}

\item{\code{targetClasses}}{Target classes provided in the request.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AccuracyRetrieveResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-AccuracyRetrieveResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AccuracyRetrieveResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AccuracyRetrieveResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-AccuracyRetrieveResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AccuracyRetrieveResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AccuracyRetrieveResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-AccuracyRetrieveResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AccuracyRetrieveResponse$fromJSON(
  AccuracyRetrieveResponseJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{AccuracyRetrieveResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AccuracyRetrieveResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-AccuracyRetrieveResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AccuracyRetrieveResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
