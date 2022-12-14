% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_lift_chart_response.R
\docType{class}
\name{ModelLiftChartResponse}
\alias{ModelLiftChartResponse}
\title{ModelLiftChartResponse}
\format{
An \code{R6Class} generator object
}
\description{
ModelLiftChartResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{bins}}{list( \link{LiftBinResponse} ) The lift chart data for that source, as specified below.}

\item{\code{source}}{character Source of the data.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ModelLiftChartResponse-new}{\code{ModelLiftChartResponse$new()}}
\item \href{#method-ModelLiftChartResponse-validate}{\code{ModelLiftChartResponse$validate()}}
\item \href{#method-ModelLiftChartResponse-toJSON}{\code{ModelLiftChartResponse$toJSON()}}
\item \href{#method-ModelLiftChartResponse-fromJSON}{\code{ModelLiftChartResponse$fromJSON()}}
\item \href{#method-ModelLiftChartResponse-clone}{\code{ModelLiftChartResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelLiftChartResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-ModelLiftChartResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelLiftChartResponse$new(
  bins = NULL,
  source = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{bins}}{The lift chart data for that source, as specified below.}

\item{\code{source}}{Source of the data.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelLiftChartResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-ModelLiftChartResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelLiftChartResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelLiftChartResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ModelLiftChartResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelLiftChartResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelLiftChartResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ModelLiftChartResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelLiftChartResponse$fromJSON(
  ModelLiftChartResponseJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ModelLiftChartResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelLiftChartResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ModelLiftChartResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelLiftChartResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
