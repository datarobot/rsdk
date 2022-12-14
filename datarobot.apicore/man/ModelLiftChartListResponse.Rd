% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_lift_chart_list_response.R
\docType{class}
\name{ModelLiftChartListResponse}
\alias{ModelLiftChartListResponse}
\title{ModelLiftChartListResponse}
\format{
An \code{R6Class} generator object
}
\description{
ModelLiftChartListResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{charts}}{list( \link{ModelLiftChartResponse} ) List of lift chart data from all available sources.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ModelLiftChartListResponse-new}{\code{ModelLiftChartListResponse$new()}}
\item \href{#method-ModelLiftChartListResponse-validate}{\code{ModelLiftChartListResponse$validate()}}
\item \href{#method-ModelLiftChartListResponse-toJSON}{\code{ModelLiftChartListResponse$toJSON()}}
\item \href{#method-ModelLiftChartListResponse-fromJSON}{\code{ModelLiftChartListResponse$fromJSON()}}
\item \href{#method-ModelLiftChartListResponse-clone}{\code{ModelLiftChartListResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelLiftChartListResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-ModelLiftChartListResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelLiftChartListResponse$new(charts = NULL, validateParams = FALSE, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{charts}}{List of lift chart data from all available sources.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelLiftChartListResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-ModelLiftChartListResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelLiftChartListResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelLiftChartListResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ModelLiftChartListResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelLiftChartListResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelLiftChartListResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ModelLiftChartListResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelLiftChartListResponse$fromJSON(
  ModelLiftChartListResponseJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ModelLiftChartListResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelLiftChartListResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ModelLiftChartListResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelLiftChartListResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
