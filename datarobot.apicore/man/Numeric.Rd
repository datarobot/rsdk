% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/numeric.R
\docType{class}
\name{Numeric}
\alias{Numeric}
\title{Numeric}
\format{
An \code{R6Class} generator object
}
\description{
Numeric Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{allData}}{numeric Statistic value for all data.}

\item{\code{insightName}}{character Insight name.}

\item{\code{perCluster}}{list( \link{PerClusterNumeric} ) Statistic values for for each cluster.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Numeric-new}{\code{Numeric$new()}}
\item \href{#method-Numeric-validate}{\code{Numeric$validate()}}
\item \href{#method-Numeric-toJSON}{\code{Numeric$toJSON()}}
\item \href{#method-Numeric-fromJSON}{\code{Numeric$fromJSON()}}
\item \href{#method-Numeric-clone}{\code{Numeric$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Numeric-new"></a>}}
\if{latex}{\out{\hypertarget{method-Numeric-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Numeric$new(
  allData = NULL,
  insightName = NULL,
  perCluster = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{allData}}{Statistic value for all data.}

\item{\code{insightName}}{Insight name.}

\item{\code{perCluster}}{Statistic values for for each cluster.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Numeric-validate"></a>}}
\if{latex}{\out{\hypertarget{method-Numeric-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Numeric$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Numeric-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-Numeric-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Numeric$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Numeric-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-Numeric-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Numeric$fromJSON(NumericJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{NumericJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Numeric-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Numeric-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Numeric$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
