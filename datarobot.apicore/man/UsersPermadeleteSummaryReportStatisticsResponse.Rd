% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/users_permadelete_summary_report_statistics_response.R
\docType{class}
\name{UsersPermadeleteSummaryReportStatisticsResponse}
\alias{UsersPermadeleteSummaryReportStatisticsResponse}
\title{UsersPermadeleteSummaryReportStatisticsResponse}
\format{
An \code{R6Class} generator object
}
\description{
UsersPermadeleteSummaryReportStatisticsResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{data}}{\link{UsersPermadeleteSummaryReportStatistics}}

\item{\code{message}}{character May contain further details.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-UsersPermadeleteSummaryReportStatisticsResponse-new}{\code{UsersPermadeleteSummaryReportStatisticsResponse$new()}}
\item \href{#method-UsersPermadeleteSummaryReportStatisticsResponse-validate}{\code{UsersPermadeleteSummaryReportStatisticsResponse$validate()}}
\item \href{#method-UsersPermadeleteSummaryReportStatisticsResponse-toJSON}{\code{UsersPermadeleteSummaryReportStatisticsResponse$toJSON()}}
\item \href{#method-UsersPermadeleteSummaryReportStatisticsResponse-fromJSON}{\code{UsersPermadeleteSummaryReportStatisticsResponse$fromJSON()}}
\item \href{#method-UsersPermadeleteSummaryReportStatisticsResponse-clone}{\code{UsersPermadeleteSummaryReportStatisticsResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UsersPermadeleteSummaryReportStatisticsResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-UsersPermadeleteSummaryReportStatisticsResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UsersPermadeleteSummaryReportStatisticsResponse$new(
  data = NULL,
  message = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{message}}{May contain further details.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UsersPermadeleteSummaryReportStatisticsResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-UsersPermadeleteSummaryReportStatisticsResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UsersPermadeleteSummaryReportStatisticsResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UsersPermadeleteSummaryReportStatisticsResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-UsersPermadeleteSummaryReportStatisticsResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UsersPermadeleteSummaryReportStatisticsResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UsersPermadeleteSummaryReportStatisticsResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-UsersPermadeleteSummaryReportStatisticsResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UsersPermadeleteSummaryReportStatisticsResponse$fromJSON(
  UsersPermadeleteSummaryReportStatisticsResponseJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{UsersPermadeleteSummaryReportStatisticsResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UsersPermadeleteSummaryReportStatisticsResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-UsersPermadeleteSummaryReportStatisticsResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UsersPermadeleteSummaryReportStatisticsResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
