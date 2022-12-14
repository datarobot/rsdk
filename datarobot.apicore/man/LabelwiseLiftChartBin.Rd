% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/labelwise_lift_chart_bin.R
\docType{class}
\name{LabelwiseLiftChartBin}
\alias{LabelwiseLiftChartBin}
\title{LabelwiseLiftChartBin}
\format{
An \code{R6Class} generator object
}
\description{
LabelwiseLiftChartBin Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{actual}}{numeric Average of actual target values for the rows in the bin.}

\item{\code{binWeight}}{numeric For projects with weights, it is the sum of the weights of all rows in the bins. Otherwise, it is the number of rows in the bin.}

\item{\code{predicted}}{numeric Average of predicted target values for the rows in the bin.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-LabelwiseLiftChartBin-new}{\code{LabelwiseLiftChartBin$new()}}
\item \href{#method-LabelwiseLiftChartBin-validate}{\code{LabelwiseLiftChartBin$validate()}}
\item \href{#method-LabelwiseLiftChartBin-toJSON}{\code{LabelwiseLiftChartBin$toJSON()}}
\item \href{#method-LabelwiseLiftChartBin-fromJSON}{\code{LabelwiseLiftChartBin$fromJSON()}}
\item \href{#method-LabelwiseLiftChartBin-clone}{\code{LabelwiseLiftChartBin$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LabelwiseLiftChartBin-new"></a>}}
\if{latex}{\out{\hypertarget{method-LabelwiseLiftChartBin-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LabelwiseLiftChartBin$new(
  actual = NULL,
  binWeight = NULL,
  predicted = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{actual}}{Average of actual target values for the rows in the bin.}

\item{\code{binWeight}}{For projects with weights, it is the sum of the weights of all rows in the bins. Otherwise, it is the number of rows in the bin.}

\item{\code{predicted}}{Average of predicted target values for the rows in the bin.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LabelwiseLiftChartBin-validate"></a>}}
\if{latex}{\out{\hypertarget{method-LabelwiseLiftChartBin-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LabelwiseLiftChartBin$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LabelwiseLiftChartBin-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-LabelwiseLiftChartBin-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LabelwiseLiftChartBin$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LabelwiseLiftChartBin-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-LabelwiseLiftChartBin-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LabelwiseLiftChartBin$fromJSON(
  LabelwiseLiftChartBinJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{LabelwiseLiftChartBinJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LabelwiseLiftChartBin-clone"></a>}}
\if{latex}{\out{\hypertarget{method-LabelwiseLiftChartBin-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LabelwiseLiftChartBin$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
