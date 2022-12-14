% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insights_prediction_field.R
\docType{class}
\name{InsightsPredictionField}
\alias{InsightsPredictionField}
\title{InsightsPredictionField}
\format{
An \code{R6Class} generator object
}
\description{
InsightsPredictionField Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{labels}}{list( character ) List of predicted label names corresponding to values}

\item{\code{values}}{list( \link{OneOfnumberarray} ) Predicted value or probability of the class identified by the label}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-InsightsPredictionField-new}{\code{InsightsPredictionField$new()}}
\item \href{#method-InsightsPredictionField-validate}{\code{InsightsPredictionField$validate()}}
\item \href{#method-InsightsPredictionField-toJSON}{\code{InsightsPredictionField$toJSON()}}
\item \href{#method-InsightsPredictionField-fromJSON}{\code{InsightsPredictionField$fromJSON()}}
\item \href{#method-InsightsPredictionField-clone}{\code{InsightsPredictionField$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-InsightsPredictionField-new"></a>}}
\if{latex}{\out{\hypertarget{method-InsightsPredictionField-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{InsightsPredictionField$new(
  labels = NULL,
  values = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{labels}}{List of predicted label names corresponding to values}

\item{\code{values}}{Predicted value or probability of the class identified by the label}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-InsightsPredictionField-validate"></a>}}
\if{latex}{\out{\hypertarget{method-InsightsPredictionField-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{InsightsPredictionField$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-InsightsPredictionField-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-InsightsPredictionField-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{InsightsPredictionField$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-InsightsPredictionField-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-InsightsPredictionField-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{InsightsPredictionField$fromJSON(
  InsightsPredictionFieldJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{InsightsPredictionFieldJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-InsightsPredictionField-clone"></a>}}
\if{latex}{\out{\hypertarget{method-InsightsPredictionField-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{InsightsPredictionField$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
