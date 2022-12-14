% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/series_accuracy_compute.R
\docType{class}
\name{SeriesAccuracyCompute}
\alias{SeriesAccuracyCompute}
\title{SeriesAccuracyCompute}
\format{
An \code{R6Class} generator object
}
\description{
SeriesAccuracyCompute Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{computeAllSeries}}{character \link{optional} Indicates whether to calculate accuracy for all series or only first 1000 (sorted by name).}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-SeriesAccuracyCompute-new}{\code{SeriesAccuracyCompute$new()}}
\item \href{#method-SeriesAccuracyCompute-validate}{\code{SeriesAccuracyCompute$validate()}}
\item \href{#method-SeriesAccuracyCompute-toJSON}{\code{SeriesAccuracyCompute$toJSON()}}
\item \href{#method-SeriesAccuracyCompute-fromJSON}{\code{SeriesAccuracyCompute$fromJSON()}}
\item \href{#method-SeriesAccuracyCompute-clone}{\code{SeriesAccuracyCompute$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SeriesAccuracyCompute-new"></a>}}
\if{latex}{\out{\hypertarget{method-SeriesAccuracyCompute-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SeriesAccuracyCompute$new(
  computeAllSeries = FALSE,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{computeAllSeries}}{Indicates whether to calculate accuracy for all series or only first 1000 (sorted by name).}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SeriesAccuracyCompute-validate"></a>}}
\if{latex}{\out{\hypertarget{method-SeriesAccuracyCompute-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SeriesAccuracyCompute$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SeriesAccuracyCompute-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-SeriesAccuracyCompute-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SeriesAccuracyCompute$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SeriesAccuracyCompute-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-SeriesAccuracyCompute-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SeriesAccuracyCompute$fromJSON(
  SeriesAccuracyComputeJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{SeriesAccuracyComputeJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SeriesAccuracyCompute-clone"></a>}}
\if{latex}{\out{\hypertarget{method-SeriesAccuracyCompute-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SeriesAccuracyCompute$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
