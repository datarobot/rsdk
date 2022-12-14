% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/histogram.R
\docType{class}
\name{Histogram}
\alias{Histogram}
\title{Histogram}
\format{
An \code{R6Class} generator object
}
\description{
Histogram Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{intervalEnd}}{numeric The end of the interval.}

\item{\code{intervalStart}}{numeric The start of the interval.}

\item{\code{occurrences}}{integer The number of times the predicted value fits within that interval.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Histogram-new}{\code{Histogram$new()}}
\item \href{#method-Histogram-validate}{\code{Histogram$validate()}}
\item \href{#method-Histogram-toJSON}{\code{Histogram$toJSON()}}
\item \href{#method-Histogram-fromJSON}{\code{Histogram$fromJSON()}}
\item \href{#method-Histogram-clone}{\code{Histogram$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Histogram-new"></a>}}
\if{latex}{\out{\hypertarget{method-Histogram-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Histogram$new(
  intervalEnd = NULL,
  intervalStart = NULL,
  occurrences = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{intervalEnd}}{The end of the interval.}

\item{\code{intervalStart}}{The start of the interval.}

\item{\code{occurrences}}{The number of times the predicted value fits within that interval.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Histogram-validate"></a>}}
\if{latex}{\out{\hypertarget{method-Histogram-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Histogram$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Histogram-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-Histogram-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Histogram$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Histogram-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-Histogram-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Histogram$fromJSON(HistogramJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{HistogramJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Histogram-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Histogram-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Histogram$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
