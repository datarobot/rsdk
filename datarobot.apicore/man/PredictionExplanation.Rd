% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prediction_explanation.R
\docType{class}
\name{PredictionExplanation}
\alias{PredictionExplanation}
\title{PredictionExplanation}
\format{
An \code{R6Class} generator object
}
\description{
PredictionExplanation Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{feature}}{character The name of the feature contributing to the prediction.}

\item{\code{featureValue}}{character The value the feature took on for this row. For image features, this value is the URL of the input image (New in v2.21).}

\item{\code{imageExplanationUrl}}{character For image features, the URL of the image containing the input image overlaid by the activation heatmap. For non-image features, this field is null.}

\item{\code{label}}{character Describes what this model output corresponds to. For regression projects, it is the name of the target feature. For classification projects, it is a level from the target feature. For Anomaly Detection models it is an Anomaly Score.}

\item{\code{perNgramTextExplanations}}{list( \link{PerNgramTextExplanations} ) \link{optional} For text features, an array of JSON object containing the per ngram based text prediction explanations.}

\item{\code{qualitativateStrength}}{character A human-readable description of how strongly the feature affected the prediction (e.g. '+++', '--', '+', '<+', '<-').}

\item{\code{strength}}{numeric The amount this feature's value affected the prediction.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PredictionExplanation-new}{\code{PredictionExplanation$new()}}
\item \href{#method-PredictionExplanation-validate}{\code{PredictionExplanation$validate()}}
\item \href{#method-PredictionExplanation-toJSON}{\code{PredictionExplanation$toJSON()}}
\item \href{#method-PredictionExplanation-fromJSON}{\code{PredictionExplanation$fromJSON()}}
\item \href{#method-PredictionExplanation-clone}{\code{PredictionExplanation$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictionExplanation-new"></a>}}
\if{latex}{\out{\hypertarget{method-PredictionExplanation-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictionExplanation$new(
  feature = NULL,
  featureValue = NULL,
  imageExplanationUrl = NULL,
  label = NULL,
  qualitativateStrength = NULL,
  strength = NULL,
  perNgramTextExplanations = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{feature}}{The name of the feature contributing to the prediction.}

\item{\code{featureValue}}{The value the feature took on for this row. For image features, this value is the URL of the input image (New in v2.21).}

\item{\code{imageExplanationUrl}}{For image features, the URL of the image containing the input image overlaid by the activation heatmap. For non-image features, this field is null.}

\item{\code{label}}{Describes what this model output corresponds to. For regression projects, it is the name of the target feature. For classification projects, it is a level from the target feature. For Anomaly Detection models it is an Anomaly Score.}

\item{\code{qualitativateStrength}}{A human-readable description of how strongly the feature affected the prediction (e.g. '+++', '--', '+', '<+', '<-').}

\item{\code{strength}}{The amount this feature's value affected the prediction.}

\item{\code{perNgramTextExplanations}}{For text features, an array of JSON object containing the per ngram based text prediction explanations.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictionExplanation-validate"></a>}}
\if{latex}{\out{\hypertarget{method-PredictionExplanation-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictionExplanation$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictionExplanation-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-PredictionExplanation-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictionExplanation$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictionExplanation-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-PredictionExplanation-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictionExplanation$fromJSON(
  PredictionExplanationJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{PredictionExplanationJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictionExplanation-clone"></a>}}
\if{latex}{\out{\hypertarget{method-PredictionExplanation-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictionExplanation$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
