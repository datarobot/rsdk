% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/roc_curve_for_datasets.R
\docType{class}
\name{RocCurveForDatasets}
\alias{RocCurveForDatasets}
\title{RocCurveForDatasets}
\format{
An \code{R6Class} generator object
}
\description{
RocCurveForDatasets Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{datasetId}}{character The ID of dataset which was used to compute ROC curve.}

\item{\code{negativeClassPredictions}}{list( numeric ) List of example predictions for the negative class.}

\item{\code{positiveClassPredictions}}{list( numeric ) List of example predictions for the negative class.}

\item{\code{rocPoints}}{\link{RocPointsResponse}}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-RocCurveForDatasets-new}{\code{RocCurveForDatasets$new()}}
\item \href{#method-RocCurveForDatasets-validate}{\code{RocCurveForDatasets$validate()}}
\item \href{#method-RocCurveForDatasets-toJSON}{\code{RocCurveForDatasets$toJSON()}}
\item \href{#method-RocCurveForDatasets-fromJSON}{\code{RocCurveForDatasets$fromJSON()}}
\item \href{#method-RocCurveForDatasets-clone}{\code{RocCurveForDatasets$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RocCurveForDatasets-new"></a>}}
\if{latex}{\out{\hypertarget{method-RocCurveForDatasets-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RocCurveForDatasets$new(
  datasetId = NULL,
  negativeClassPredictions = NULL,
  positiveClassPredictions = NULL,
  rocPoints = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{datasetId}}{The ID of dataset which was used to compute ROC curve.}

\item{\code{negativeClassPredictions}}{List of example predictions for the negative class.}

\item{\code{positiveClassPredictions}}{List of example predictions for the negative class.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RocCurveForDatasets-validate"></a>}}
\if{latex}{\out{\hypertarget{method-RocCurveForDatasets-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RocCurveForDatasets$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RocCurveForDatasets-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-RocCurveForDatasets-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RocCurveForDatasets$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RocCurveForDatasets-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-RocCurveForDatasets-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RocCurveForDatasets$fromJSON(RocCurveForDatasetsJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{RocCurveForDatasetsJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RocCurveForDatasets-clone"></a>}}
\if{latex}{\out{\hypertarget{method-RocCurveForDatasets-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RocCurveForDatasets$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
