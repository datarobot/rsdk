% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anomaly_assessment_records_response.R
\docType{class}
\name{AnomalyAssessmentRecordsResponse}
\alias{AnomalyAssessmentRecordsResponse}
\title{AnomalyAssessmentRecordsResponse}
\format{
An \code{R6Class} generator object
}
\description{
AnomalyAssessmentRecordsResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{count}}{integer Number of items in current page.}

\item{\code{data}}{list( \link{AnomalyAssessmentRecordResponse} ) Anomaly assessment record.}

\item{\code{next_}}{character URL pointing to the next page (if null, there is no next page)}

\item{\code{previous}}{character URL pointing to the previous page (if null, there is no previous page)}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-AnomalyAssessmentRecordsResponse-new}{\code{AnomalyAssessmentRecordsResponse$new()}}
\item \href{#method-AnomalyAssessmentRecordsResponse-validate}{\code{AnomalyAssessmentRecordsResponse$validate()}}
\item \href{#method-AnomalyAssessmentRecordsResponse-toJSON}{\code{AnomalyAssessmentRecordsResponse$toJSON()}}
\item \href{#method-AnomalyAssessmentRecordsResponse-fromJSON}{\code{AnomalyAssessmentRecordsResponse$fromJSON()}}
\item \href{#method-AnomalyAssessmentRecordsResponse-clone}{\code{AnomalyAssessmentRecordsResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AnomalyAssessmentRecordsResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-AnomalyAssessmentRecordsResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AnomalyAssessmentRecordsResponse$new(
  count = NULL,
  data = NULL,
  next_ = NULL,
  previous = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{count}}{Number of items in current page.}

\item{\code{data}}{Anomaly assessment record.}

\item{\code{next_}}{URL pointing to the next page (if null, there is no next page)}

\item{\code{previous}}{URL pointing to the previous page (if null, there is no previous page)}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AnomalyAssessmentRecordsResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-AnomalyAssessmentRecordsResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AnomalyAssessmentRecordsResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AnomalyAssessmentRecordsResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-AnomalyAssessmentRecordsResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AnomalyAssessmentRecordsResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AnomalyAssessmentRecordsResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-AnomalyAssessmentRecordsResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AnomalyAssessmentRecordsResponse$fromJSON(
  AnomalyAssessmentRecordsResponseJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{AnomalyAssessmentRecordsResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AnomalyAssessmentRecordsResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-AnomalyAssessmentRecordsResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AnomalyAssessmentRecordsResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
