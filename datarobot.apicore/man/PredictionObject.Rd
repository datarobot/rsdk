% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prediction_object.R
\docType{class}
\name{PredictionObject}
\alias{PredictionObject}
\title{PredictionObject}
\format{
An \code{R6Class} generator object
}
\description{
PredictionObject Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{actualValue}}{character \link{optional} In the case of an unsupervised time series project with a dataset using ``predictionsStartDate`` and ``predictionsEndDate`` for bulk predictions and a specified actual value column, the predictions will be a json array in the same format as with a forecast point with one additional element - `actualValues`. It is the actual value in the row.}

\item{\code{forecastDistance}}{integer \link{optional} (if time series project) The number of time units this prediction is away from the forecastPoint. The unit of time is determined by the timeUnit of the datetime partition column.}

\item{\code{forecastPoint}}{character \link{optional} (if time series project) The forecastPoint of the predictions. Either provided or inferred.}

\item{\code{originalFormatTimestamp}}{character \link{optional} The timestamp of this row in the prediction dataset. Unlike the ``timestamp`` field, this field will keep the same DateTime formatting as the uploaded prediction dataset. (This column is shown if enabled by your administrator.)}

\item{\code{positiveProbability}}{numeric \link{optional} For binary classification, the probability the row belongs to the positive class.}

\item{\code{prediction}}{\link{OneOfnumberstringarray} The prediction of the model.}

\item{\code{predictionExplanationMetadata}}{list( \link{PredictionExplanationsMetadataValues} ) \link{optional} Array containing algorithm-specific values. Varies depending on the value of `explanationAlgorithm`.}

\item{\code{predictionExplanations}}{list( \link{PredictionExplanationsObject} ) \link{optional} Array contains `predictionExplanation` objects. The total elements in the array are bounded by maxExplanations and feature count. It will be present only if `explanationAlgorithm` is not null (prediction explanations were requested).}

\item{\code{predictionIntervalLowerBound}}{numeric \link{optional} Present if ``includePredictionIntervals`` is True. Indicates a lower bound of the estimate of error based on test data.}

\item{\code{predictionIntervalUpperBound}}{numeric \link{optional} Present if ``includePredictionIntervals`` is True. Indicates an upper bound of the estimate of error based on test data.}

\item{\code{predictionThreshold}}{numeric \link{optional} Threshold used for binary classification in predictions.}

\item{\code{predictionValues}}{list( \link{PredictionArrayObjectValues} ) \link{optional} A list of predicted values for this row.}

\item{\code{rowId}}{integer The row in the prediction dataset this prediction corresponds to.}

\item{\code{segmentId}}{character \link{optional} The ID of the segment value for a segmented project.}

\item{\code{seriesId}}{character \link{optional} The ID of the series value for a multiseries project. For time series projects that are not a multiseries this will be a NaN.}

\item{\code{target}}{character \link{optional} In the case of a time series project with a dataset using predictionsStartDate and predictionsEndDate for bulk predictions, the predictions will be a json array in the same format as with a forecast point with one additional element - `target`. It is the target value in the row.}

\item{\code{timestamp}}{character \link{optional} (if time series project) The timestamp of this row in the prediction dataset.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PredictionObject-new}{\code{PredictionObject$new()}}
\item \href{#method-PredictionObject-validate}{\code{PredictionObject$validate()}}
\item \href{#method-PredictionObject-toJSON}{\code{PredictionObject$toJSON()}}
\item \href{#method-PredictionObject-fromJSON}{\code{PredictionObject$fromJSON()}}
\item \href{#method-PredictionObject-clone}{\code{PredictionObject$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictionObject-new"></a>}}
\if{latex}{\out{\hypertarget{method-PredictionObject-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictionObject$new(
  prediction = NULL,
  rowId = NULL,
  actualValue = NULL,
  forecastDistance = NULL,
  forecastPoint = NULL,
  originalFormatTimestamp = NULL,
  positiveProbability = NULL,
  predictionExplanationMetadata = NULL,
  predictionExplanations = NULL,
  predictionIntervalLowerBound = NULL,
  predictionIntervalUpperBound = NULL,
  predictionThreshold = NULL,
  predictionValues = NULL,
  segmentId = NULL,
  seriesId = NULL,
  target = NULL,
  timestamp = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{prediction}}{The prediction of the model.}

\item{\code{rowId}}{The row in the prediction dataset this prediction corresponds to.}

\item{\code{actualValue}}{In the case of an unsupervised time series project with a dataset using ``predictionsStartDate`` and ``predictionsEndDate`` for bulk predictions and a specified actual value column, the predictions will be a json array in the same format as with a forecast point with one additional element - `actualValues`. It is the actual value in the row.}

\item{\code{forecastDistance}}{(if time series project) The number of time units this prediction is away from the forecastPoint. The unit of time is determined by the timeUnit of the datetime partition column.}

\item{\code{forecastPoint}}{(if time series project) The forecastPoint of the predictions. Either provided or inferred.}

\item{\code{originalFormatTimestamp}}{The timestamp of this row in the prediction dataset. Unlike the ``timestamp`` field, this field will keep the same DateTime formatting as the uploaded prediction dataset. (This column is shown if enabled by your administrator.)}

\item{\code{positiveProbability}}{For binary classification, the probability the row belongs to the positive class.}

\item{\code{predictionExplanationMetadata}}{Array containing algorithm-specific values. Varies depending on the value of `explanationAlgorithm`.}

\item{\code{predictionExplanations}}{Array contains `predictionExplanation` objects. The total elements in the array are bounded by maxExplanations and feature count. It will be present only if `explanationAlgorithm` is not null (prediction explanations were requested).}

\item{\code{predictionIntervalLowerBound}}{Present if ``includePredictionIntervals`` is True. Indicates a lower bound of the estimate of error based on test data.}

\item{\code{predictionIntervalUpperBound}}{Present if ``includePredictionIntervals`` is True. Indicates an upper bound of the estimate of error based on test data.}

\item{\code{predictionThreshold}}{Threshold used for binary classification in predictions.}

\item{\code{predictionValues}}{A list of predicted values for this row.}

\item{\code{segmentId}}{The ID of the segment value for a segmented project.}

\item{\code{seriesId}}{The ID of the series value for a multiseries project. For time series projects that are not a multiseries this will be a NaN.}

\item{\code{target}}{In the case of a time series project with a dataset using predictionsStartDate and predictionsEndDate for bulk predictions, the predictions will be a json array in the same format as with a forecast point with one additional element - `target`. It is the target value in the row.}

\item{\code{timestamp}}{(if time series project) The timestamp of this row in the prediction dataset.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictionObject-validate"></a>}}
\if{latex}{\out{\hypertarget{method-PredictionObject-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictionObject$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictionObject-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-PredictionObject-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictionObject$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictionObject-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-PredictionObject-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictionObject$fromJSON(PredictionObjectJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{PredictionObjectJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictionObject-clone"></a>}}
\if{latex}{\out{\hypertarget{method-PredictionObject-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictionObject$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
