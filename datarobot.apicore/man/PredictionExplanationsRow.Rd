% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prediction_explanations_row.R
\docType{class}
\name{PredictionExplanationsRow}
\alias{PredictionExplanationsRow}
\title{PredictionExplanationsRow}
\format{
An \code{R6Class} generator object
}
\description{
PredictionExplanationsRow Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{adjustedPrediction}}{numeric The exposure-adjusted output of the model for this row.}

\item{\code{adjustedPredictionValues}}{list( \link{PredictionExplanationsPredictionValues} ) The exposure-adjusted output of the model for this row.}

\item{\code{forecastDistance}}{integer Forecast distance for the row. For time series projects only.}

\item{\code{forecastPoint}}{character Forecast point for the row. For time series projects only.}

\item{\code{prediction}}{numeric The output of the model for this row.}

\item{\code{predictionExplanations}}{list( \link{PredictionExplanation} ) A list of prediction explanations.}

\item{\code{predictionThreshold}}{numeric The threshold value used for classification prediction.}

\item{\code{predictionValues}}{list( \link{PredictionExplanationsPredictionValues} ) A list of prediction values.}

\item{\code{rowId}}{integer Which row this PredictionExplanationsRow describes.}

\item{\code{seriesId}}{character The ID of the series value for the row in a multiseries project. For a single series project this will be null. For time series projects only.}

\item{\code{timestamp}}{character Timestamp for the row. For time series projects only.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PredictionExplanationsRow-new}{\code{PredictionExplanationsRow$new()}}
\item \href{#method-PredictionExplanationsRow-validate}{\code{PredictionExplanationsRow$validate()}}
\item \href{#method-PredictionExplanationsRow-toJSON}{\code{PredictionExplanationsRow$toJSON()}}
\item \href{#method-PredictionExplanationsRow-fromJSON}{\code{PredictionExplanationsRow$fromJSON()}}
\item \href{#method-PredictionExplanationsRow-clone}{\code{PredictionExplanationsRow$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictionExplanationsRow-new"></a>}}
\if{latex}{\out{\hypertarget{method-PredictionExplanationsRow-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictionExplanationsRow$new(
  adjustedPrediction = NULL,
  adjustedPredictionValues = NULL,
  forecastDistance = NULL,
  forecastPoint = NULL,
  prediction = NULL,
  predictionExplanations = NULL,
  predictionThreshold = NULL,
  predictionValues = NULL,
  rowId = NULL,
  seriesId = NULL,
  timestamp = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{adjustedPrediction}}{The exposure-adjusted output of the model for this row.}

\item{\code{adjustedPredictionValues}}{The exposure-adjusted output of the model for this row.}

\item{\code{forecastDistance}}{Forecast distance for the row. For time series projects only.}

\item{\code{forecastPoint}}{Forecast point for the row. For time series projects only.}

\item{\code{prediction}}{The output of the model for this row.}

\item{\code{predictionExplanations}}{A list of prediction explanations.}

\item{\code{predictionThreshold}}{The threshold value used for classification prediction.}

\item{\code{predictionValues}}{A list of prediction values.}

\item{\code{rowId}}{Which row this PredictionExplanationsRow describes.}

\item{\code{seriesId}}{The ID of the series value for the row in a multiseries project. For a single series project this will be null. For time series projects only.}

\item{\code{timestamp}}{Timestamp for the row. For time series projects only.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictionExplanationsRow-validate"></a>}}
\if{latex}{\out{\hypertarget{method-PredictionExplanationsRow-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictionExplanationsRow$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictionExplanationsRow-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-PredictionExplanationsRow-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictionExplanationsRow$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictionExplanationsRow-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-PredictionExplanationsRow-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictionExplanationsRow$fromJSON(
  PredictionExplanationsRowJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{PredictionExplanationsRowJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictionExplanationsRow-clone"></a>}}
\if{latex}{\out{\hypertarget{method-PredictionExplanationsRow-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictionExplanationsRow$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
