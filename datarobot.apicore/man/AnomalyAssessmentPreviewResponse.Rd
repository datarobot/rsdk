% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anomaly_assessment_preview_response.R
\docType{class}
\name{AnomalyAssessmentPreviewResponse}
\alias{AnomalyAssessmentPreviewResponse}
\title{AnomalyAssessmentPreviewResponse}
\format{
An \code{R6Class} generator object
}
\description{
AnomalyAssessmentPreviewResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{backtest}}{\link{OneOfintegerstring} The backtest of the record.}

\item{\code{endDate}}{character ISO-formatted last timestamp in the subset. For example: ``2019-08-30T00:00:00.000000Z``.}

\item{\code{modelId}}{character The model ID of the record.}

\item{\code{previewBins}}{list( \link{BinResponse} ) Aggregated predictions for the subset. Bins boundaries may differ from actual start/end dates because this is an aggregation.}

\item{\code{projectId}}{character The project ID of the record.}

\item{\code{recordId}}{character The ID of the anomaly assessment record.}

\item{\code{seriesId}}{character The series id of the record. Applicable in multiseries projects}

\item{\code{source}}{character The source of the record}

\item{\code{startDate}}{character ISO-formatted first timestamp in the subset. For example: ``2019-08-01T00:00:00.000000Z``.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-AnomalyAssessmentPreviewResponse-new}{\code{AnomalyAssessmentPreviewResponse$new()}}
\item \href{#method-AnomalyAssessmentPreviewResponse-validate}{\code{AnomalyAssessmentPreviewResponse$validate()}}
\item \href{#method-AnomalyAssessmentPreviewResponse-toJSON}{\code{AnomalyAssessmentPreviewResponse$toJSON()}}
\item \href{#method-AnomalyAssessmentPreviewResponse-fromJSON}{\code{AnomalyAssessmentPreviewResponse$fromJSON()}}
\item \href{#method-AnomalyAssessmentPreviewResponse-clone}{\code{AnomalyAssessmentPreviewResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AnomalyAssessmentPreviewResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-AnomalyAssessmentPreviewResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AnomalyAssessmentPreviewResponse$new(
  backtest = NULL,
  endDate = NULL,
  modelId = NULL,
  previewBins = NULL,
  projectId = NULL,
  recordId = NULL,
  seriesId = NULL,
  source = NULL,
  startDate = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{backtest}}{The backtest of the record.}

\item{\code{endDate}}{ISO-formatted last timestamp in the subset. For example: ``2019-08-30T00:00:00.000000Z``.}

\item{\code{modelId}}{The model ID of the record.}

\item{\code{previewBins}}{Aggregated predictions for the subset. Bins boundaries may differ from actual start/end dates because this is an aggregation.}

\item{\code{projectId}}{The project ID of the record.}

\item{\code{recordId}}{The ID of the anomaly assessment record.}

\item{\code{seriesId}}{The series id of the record. Applicable in multiseries projects}

\item{\code{source}}{The source of the record}

\item{\code{startDate}}{ISO-formatted first timestamp in the subset. For example: ``2019-08-01T00:00:00.000000Z``.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AnomalyAssessmentPreviewResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-AnomalyAssessmentPreviewResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AnomalyAssessmentPreviewResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AnomalyAssessmentPreviewResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-AnomalyAssessmentPreviewResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AnomalyAssessmentPreviewResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AnomalyAssessmentPreviewResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-AnomalyAssessmentPreviewResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AnomalyAssessmentPreviewResponse$fromJSON(
  AnomalyAssessmentPreviewResponseJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{AnomalyAssessmentPreviewResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AnomalyAssessmentPreviewResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-AnomalyAssessmentPreviewResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AnomalyAssessmentPreviewResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
