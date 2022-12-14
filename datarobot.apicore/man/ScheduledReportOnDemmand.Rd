% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scheduled_report_on_demmand.R
\docType{class}
\name{ScheduledReportOnDemmand}
\alias{ScheduledReportOnDemmand}
\title{ScheduledReportOnDemmand}
\format{
An \code{R6Class} generator object
}
\description{
ScheduledReportOnDemmand Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{id}}{character ID of Scheduled report record.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ScheduledReportOnDemmand-new}{\code{ScheduledReportOnDemmand$new()}}
\item \href{#method-ScheduledReportOnDemmand-validate}{\code{ScheduledReportOnDemmand$validate()}}
\item \href{#method-ScheduledReportOnDemmand-toJSON}{\code{ScheduledReportOnDemmand$toJSON()}}
\item \href{#method-ScheduledReportOnDemmand-fromJSON}{\code{ScheduledReportOnDemmand$fromJSON()}}
\item \href{#method-ScheduledReportOnDemmand-clone}{\code{ScheduledReportOnDemmand$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ScheduledReportOnDemmand-new"></a>}}
\if{latex}{\out{\hypertarget{method-ScheduledReportOnDemmand-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ScheduledReportOnDemmand$new(id = NULL, validateParams = FALSE, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{id}}{ID of Scheduled report record.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ScheduledReportOnDemmand-validate"></a>}}
\if{latex}{\out{\hypertarget{method-ScheduledReportOnDemmand-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ScheduledReportOnDemmand$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ScheduledReportOnDemmand-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ScheduledReportOnDemmand-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ScheduledReportOnDemmand$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ScheduledReportOnDemmand-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ScheduledReportOnDemmand-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ScheduledReportOnDemmand$fromJSON(
  ScheduledReportOnDemmandJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ScheduledReportOnDemmandJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ScheduledReportOnDemmand-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ScheduledReportOnDemmand-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ScheduledReportOnDemmand$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
