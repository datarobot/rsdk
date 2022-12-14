% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/traning_prediction_shap_metadata.R
\docType{class}
\name{TraningPredictionShapMetadata}
\alias{TraningPredictionShapMetadata}
\title{TraningPredictionShapMetadata}
\format{
An \code{R6Class} generator object
}
\description{
TraningPredictionShapMetadata Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{shapBaseValue}}{numeric The model's average prediction over the training data. SHAP values are deviations from the base value.}

\item{\code{shapRemainingTotal}}{integer The total of SHAP values for features beyond the maxExplanations. This can be identically 0 in all rows, if maxExplanations is greater than the number of features and thus all features are returned.}

\item{\code{warnings}}{list( \link{ShapWarningItems} ) SHAP values calculation warnings}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-TraningPredictionShapMetadata-new}{\code{TraningPredictionShapMetadata$new()}}
\item \href{#method-TraningPredictionShapMetadata-validate}{\code{TraningPredictionShapMetadata$validate()}}
\item \href{#method-TraningPredictionShapMetadata-toJSON}{\code{TraningPredictionShapMetadata$toJSON()}}
\item \href{#method-TraningPredictionShapMetadata-fromJSON}{\code{TraningPredictionShapMetadata$fromJSON()}}
\item \href{#method-TraningPredictionShapMetadata-clone}{\code{TraningPredictionShapMetadata$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TraningPredictionShapMetadata-new"></a>}}
\if{latex}{\out{\hypertarget{method-TraningPredictionShapMetadata-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TraningPredictionShapMetadata$new(
  shapBaseValue = NULL,
  shapRemainingTotal = NULL,
  warnings = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{shapBaseValue}}{The model's average prediction over the training data. SHAP values are deviations from the base value.}

\item{\code{shapRemainingTotal}}{The total of SHAP values for features beyond the maxExplanations. This can be identically 0 in all rows, if maxExplanations is greater than the number of features and thus all features are returned.}

\item{\code{warnings}}{SHAP values calculation warnings}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TraningPredictionShapMetadata-validate"></a>}}
\if{latex}{\out{\hypertarget{method-TraningPredictionShapMetadata-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TraningPredictionShapMetadata$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TraningPredictionShapMetadata-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-TraningPredictionShapMetadata-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TraningPredictionShapMetadata$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TraningPredictionShapMetadata-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-TraningPredictionShapMetadata-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TraningPredictionShapMetadata$fromJSON(
  TraningPredictionShapMetadataJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{TraningPredictionShapMetadataJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TraningPredictionShapMetadata-clone"></a>}}
\if{latex}{\out{\hypertarget{method-TraningPredictionShapMetadata-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TraningPredictionShapMetadata$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
