% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datetime_trend_plots_backtest_metadata.R
\docType{class}
\name{DatetimeTrendPlotsBacktestMetadata}
\alias{DatetimeTrendPlotsBacktestMetadata}
\title{DatetimeTrendPlotsBacktestMetadata}
\format{
An \code{R6Class} generator object
}
\description{
DatetimeTrendPlotsBacktestMetadata Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{training}}{\link{DatetimeTrendPlotsMetadataStartEndDates}}

\item{\code{validation}}{\link{DatetimeTrendPlotsMetadataStartEndDates}}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-DatetimeTrendPlotsBacktestMetadata-new}{\code{DatetimeTrendPlotsBacktestMetadata$new()}}
\item \href{#method-DatetimeTrendPlotsBacktestMetadata-validate}{\code{DatetimeTrendPlotsBacktestMetadata$validate()}}
\item \href{#method-DatetimeTrendPlotsBacktestMetadata-toJSON}{\code{DatetimeTrendPlotsBacktestMetadata$toJSON()}}
\item \href{#method-DatetimeTrendPlotsBacktestMetadata-fromJSON}{\code{DatetimeTrendPlotsBacktestMetadata$fromJSON()}}
\item \href{#method-DatetimeTrendPlotsBacktestMetadata-clone}{\code{DatetimeTrendPlotsBacktestMetadata$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DatetimeTrendPlotsBacktestMetadata-new"></a>}}
\if{latex}{\out{\hypertarget{method-DatetimeTrendPlotsBacktestMetadata-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DatetimeTrendPlotsBacktestMetadata$new(
  training = NULL,
  validation = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DatetimeTrendPlotsBacktestMetadata-validate"></a>}}
\if{latex}{\out{\hypertarget{method-DatetimeTrendPlotsBacktestMetadata-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DatetimeTrendPlotsBacktestMetadata$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DatetimeTrendPlotsBacktestMetadata-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-DatetimeTrendPlotsBacktestMetadata-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DatetimeTrendPlotsBacktestMetadata$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DatetimeTrendPlotsBacktestMetadata-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-DatetimeTrendPlotsBacktestMetadata-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DatetimeTrendPlotsBacktestMetadata$fromJSON(
  DatetimeTrendPlotsBacktestMetadataJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{DatetimeTrendPlotsBacktestMetadataJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DatetimeTrendPlotsBacktestMetadata-clone"></a>}}
\if{latex}{\out{\hypertarget{method-DatetimeTrendPlotsBacktestMetadata-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DatetimeTrendPlotsBacktestMetadata$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
