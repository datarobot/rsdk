% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monitoring_data_delete_payload.R
\docType{class}
\name{MonitoringDataDeletePayload}
\alias{MonitoringDataDeletePayload}
\title{MonitoringDataDeletePayload}
\format{
An \code{R6Class} generator object
}
\description{
MonitoringDataDeletePayload Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{end}}{character \link{optional} RFC3339 datetime. End of time period to delete monitoring data.Note: this field only accepts top of the hour RFC3339 datetime strings For example: ``2019-08-01T00:00:00Z``.}

\item{\code{modelId}}{character The id of the model for which monitoring data are being deleted.}

\item{\code{start}}{character \link{optional} RFC3339 datetime. Start of time period to delete monitoring data.Note: this field only accepts top of the hour RFC3339 datetime strings For example: ``2019-08-01T00:00:00Z``.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-MonitoringDataDeletePayload-new}{\code{MonitoringDataDeletePayload$new()}}
\item \href{#method-MonitoringDataDeletePayload-validate}{\code{MonitoringDataDeletePayload$validate()}}
\item \href{#method-MonitoringDataDeletePayload-toJSON}{\code{MonitoringDataDeletePayload$toJSON()}}
\item \href{#method-MonitoringDataDeletePayload-fromJSON}{\code{MonitoringDataDeletePayload$fromJSON()}}
\item \href{#method-MonitoringDataDeletePayload-clone}{\code{MonitoringDataDeletePayload$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MonitoringDataDeletePayload-new"></a>}}
\if{latex}{\out{\hypertarget{method-MonitoringDataDeletePayload-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MonitoringDataDeletePayload$new(
  modelId = NULL,
  end = NULL,
  start = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{modelId}}{The id of the model for which monitoring data are being deleted.}

\item{\code{end}}{RFC3339 datetime. End of time period to delete monitoring data.Note: this field only accepts top of the hour RFC3339 datetime strings For example: ``2019-08-01T00:00:00Z``.}

\item{\code{start}}{RFC3339 datetime. Start of time period to delete monitoring data.Note: this field only accepts top of the hour RFC3339 datetime strings For example: ``2019-08-01T00:00:00Z``.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MonitoringDataDeletePayload-validate"></a>}}
\if{latex}{\out{\hypertarget{method-MonitoringDataDeletePayload-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MonitoringDataDeletePayload$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MonitoringDataDeletePayload-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-MonitoringDataDeletePayload-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MonitoringDataDeletePayload$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MonitoringDataDeletePayload-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-MonitoringDataDeletePayload-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MonitoringDataDeletePayload$fromJSON(
  MonitoringDataDeletePayloadJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{MonitoringDataDeletePayloadJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MonitoringDataDeletePayload-clone"></a>}}
\if{latex}{\out{\hypertarget{method-MonitoringDataDeletePayload-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MonitoringDataDeletePayload$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
