% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/float.R
\docType{class}
\name{Float}
\alias{Float}
\title{Float}
\format{
An \code{R6Class} generator object
}
\description{
Float Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{max}}{numeric Maximum value for the parameter.}

\item{\code{min}}{numeric Minimum value for the parameter.}

\item{\code{supportsGridSearch}}{character When True, Grid Search is supported for this parameter.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Float-new}{\code{Float$new()}}
\item \href{#method-Float-validate}{\code{Float$validate()}}
\item \href{#method-Float-toJSON}{\code{Float$toJSON()}}
\item \href{#method-Float-fromJSON}{\code{Float$fromJSON()}}
\item \href{#method-Float-clone}{\code{Float$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Float-new"></a>}}
\if{latex}{\out{\hypertarget{method-Float-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Float$new(
  max = NULL,
  min = NULL,
  supportsGridSearch = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{max}}{Maximum value for the parameter.}

\item{\code{min}}{Minimum value for the parameter.}

\item{\code{supportsGridSearch}}{When True, Grid Search is supported for this parameter.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Float-validate"></a>}}
\if{latex}{\out{\hypertarget{method-Float-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Float$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Float-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-Float-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Float$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Float-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-Float-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Float$fromJSON(FloatJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{FloatJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Float-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Float-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Float$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
