% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shap_warning_values.R
\docType{class}
\name{ShapWarningValues}
\alias{ShapWarningValues}
\title{ShapWarningValues}
\format{
An \code{R6Class} generator object
}
\description{
ShapWarningValues Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{maxNormalizedMismatch}}{numeric The maximal relative normalized mismatch value.}

\item{\code{mismatchRowCount}}{integer The count of rows for which additivity check failed.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ShapWarningValues-new}{\code{ShapWarningValues$new()}}
\item \href{#method-ShapWarningValues-validate}{\code{ShapWarningValues$validate()}}
\item \href{#method-ShapWarningValues-toJSON}{\code{ShapWarningValues$toJSON()}}
\item \href{#method-ShapWarningValues-fromJSON}{\code{ShapWarningValues$fromJSON()}}
\item \href{#method-ShapWarningValues-clone}{\code{ShapWarningValues$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShapWarningValues-new"></a>}}
\if{latex}{\out{\hypertarget{method-ShapWarningValues-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ShapWarningValues$new(
  maxNormalizedMismatch = NULL,
  mismatchRowCount = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{maxNormalizedMismatch}}{The maximal relative normalized mismatch value.}

\item{\code{mismatchRowCount}}{The count of rows for which additivity check failed.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShapWarningValues-validate"></a>}}
\if{latex}{\out{\hypertarget{method-ShapWarningValues-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ShapWarningValues$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShapWarningValues-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ShapWarningValues-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ShapWarningValues$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShapWarningValues-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ShapWarningValues-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ShapWarningValues$fromJSON(ShapWarningValuesJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ShapWarningValuesJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShapWarningValues-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ShapWarningValues-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ShapWarningValues$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
