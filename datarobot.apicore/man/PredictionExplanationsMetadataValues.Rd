% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prediction_explanations_metadata_values.R
\docType{class}
\name{PredictionExplanationsMetadataValues}
\alias{PredictionExplanationsMetadataValues}
\title{PredictionExplanationsMetadataValues}
\format{
An \code{R6Class} generator object
}
\description{
PredictionExplanationsMetadataValues Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{shapRemainingTotal}}{integer \link{optional} Will be present only if `explanationAlgorithm` = 'shap' and `maxExplanations` is nonzero. The total of SHAP values for features beyond the `maxExplanations`. This can be identically 0 in all rows, if `maxExplanations` is greater than the number of features and thus all features are returned.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PredictionExplanationsMetadataValues-new}{\code{PredictionExplanationsMetadataValues$new()}}
\item \href{#method-PredictionExplanationsMetadataValues-validate}{\code{PredictionExplanationsMetadataValues$validate()}}
\item \href{#method-PredictionExplanationsMetadataValues-toJSON}{\code{PredictionExplanationsMetadataValues$toJSON()}}
\item \href{#method-PredictionExplanationsMetadataValues-fromJSON}{\code{PredictionExplanationsMetadataValues$fromJSON()}}
\item \href{#method-PredictionExplanationsMetadataValues-clone}{\code{PredictionExplanationsMetadataValues$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictionExplanationsMetadataValues-new"></a>}}
\if{latex}{\out{\hypertarget{method-PredictionExplanationsMetadataValues-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictionExplanationsMetadataValues$new(
  shapRemainingTotal = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{shapRemainingTotal}}{Will be present only if `explanationAlgorithm` = 'shap' and `maxExplanations` is nonzero. The total of SHAP values for features beyond the `maxExplanations`. This can be identically 0 in all rows, if `maxExplanations` is greater than the number of features and thus all features are returned.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictionExplanationsMetadataValues-validate"></a>}}
\if{latex}{\out{\hypertarget{method-PredictionExplanationsMetadataValues-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictionExplanationsMetadataValues$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictionExplanationsMetadataValues-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-PredictionExplanationsMetadataValues-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictionExplanationsMetadataValues$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictionExplanationsMetadataValues-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-PredictionExplanationsMetadataValues-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictionExplanationsMetadataValues$fromJSON(
  PredictionExplanationsMetadataValuesJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{PredictionExplanationsMetadataValuesJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictionExplanationsMetadataValues-clone"></a>}}
\if{latex}{\out{\hypertarget{method-PredictionExplanationsMetadataValues-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictionExplanationsMetadataValues$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
