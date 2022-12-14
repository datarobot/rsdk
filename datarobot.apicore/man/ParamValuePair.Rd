% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/param_value_pair.R
\docType{class}
\name{ParamValuePair}
\alias{ParamValuePair}
\title{ParamValuePair}
\format{
An \code{R6Class} generator object
}
\description{
ParamValuePair Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{param}}{character The name of a field associated with the value.}

\item{\code{value}}{\link{OneOfAnyTypearray} Any value.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ParamValuePair-new}{\code{ParamValuePair$new()}}
\item \href{#method-ParamValuePair-validate}{\code{ParamValuePair$validate()}}
\item \href{#method-ParamValuePair-toJSON}{\code{ParamValuePair$toJSON()}}
\item \href{#method-ParamValuePair-fromJSON}{\code{ParamValuePair$fromJSON()}}
\item \href{#method-ParamValuePair-clone}{\code{ParamValuePair$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ParamValuePair-new"></a>}}
\if{latex}{\out{\hypertarget{method-ParamValuePair-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ParamValuePair$new(param = NULL, value = NULL, validateParams = FALSE, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{param}}{The name of a field associated with the value.}

\item{\code{value}}{Any value.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ParamValuePair-validate"></a>}}
\if{latex}{\out{\hypertarget{method-ParamValuePair-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ParamValuePair$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ParamValuePair-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ParamValuePair-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ParamValuePair$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ParamValuePair-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ParamValuePair-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ParamValuePair$fromJSON(ParamValuePairJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ParamValuePairJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ParamValuePair-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ParamValuePair-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ParamValuePair$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
