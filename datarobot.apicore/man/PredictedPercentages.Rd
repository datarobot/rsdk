% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predicted_percentages.R
\docType{class}
\name{PredictedPercentages}
\alias{PredictedPercentages}
\title{PredictedPercentages}
\format{
An \code{R6Class} generator object
}
\description{
PredictedPercentages Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{otherClassName}}{character the name of the class}

\item{\code{percentage}}{numeric the percentage of the times this class was actual when `classMetrics.className` is predicted}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PredictedPercentages-new}{\code{PredictedPercentages$new()}}
\item \href{#method-PredictedPercentages-validate}{\code{PredictedPercentages$validate()}}
\item \href{#method-PredictedPercentages-toJSON}{\code{PredictedPercentages$toJSON()}}
\item \href{#method-PredictedPercentages-fromJSON}{\code{PredictedPercentages$fromJSON()}}
\item \href{#method-PredictedPercentages-clone}{\code{PredictedPercentages$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictedPercentages-new"></a>}}
\if{latex}{\out{\hypertarget{method-PredictedPercentages-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictedPercentages$new(
  otherClassName = NULL,
  percentage = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{otherClassName}}{the name of the class}

\item{\code{percentage}}{the percentage of the times this class was actual when `classMetrics.className` is predicted}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictedPercentages-validate"></a>}}
\if{latex}{\out{\hypertarget{method-PredictedPercentages-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictedPercentages$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictedPercentages-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-PredictedPercentages-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictedPercentages$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictedPercentages-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-PredictedPercentages-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictedPercentages$fromJSON(PredictedPercentagesJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{PredictedPercentagesJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictedPercentages-clone"></a>}}
\if{latex}{\out{\hypertarget{method-PredictedPercentages-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictedPercentages$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
