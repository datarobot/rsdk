% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blueprint_response.R
\docType{class}
\name{BlueprintResponse}
\alias{BlueprintResponse}
\title{BlueprintResponse}
\format{
An \code{R6Class} generator object
}
\description{
BlueprintResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{blueprintCategory}}{character describes the category of the blueprint and indicates the kind of model this blueprint produces. Will be either \"DataRobot\" or \"Scaleout DataRobot\".}

\item{\code{id}}{character the blueprint ID of this blueprint - note that this is not an ObjectId.}

\item{\code{modelType}}{character the model this blueprint will produce.}

\item{\code{monotonicDecreasingFeaturelistId}}{character the ID of the featurelist that defines the set of features with a monotonically decreasing relationship to the target. If null, no such constraints are enforced.}

\item{\code{monotonicIncreasingFeaturelistId}}{character null or str, the ID of the featurelist that defines the set of features with a monotonically increasing relationship to the target. If null, no such constraints are enforced.}

\item{\code{processes}}{list( character ) a list of strings representing processes the blueprint uses.}

\item{\code{projectId}}{character the project the blueprint belongs to.}

\item{\code{recommendedFeaturelistId}}{character The ID of the feature list recommended for this blueprint. If this field is not present, then there is no recommended feature list.}

\item{\code{supportsComposableMl}}{character indicates whether this blueprint is supported in Composable ML.}

\item{\code{supportsMonotonicConstraints}}{character whether this model supports enforcing monotonic constraints.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-BlueprintResponse-new}{\code{BlueprintResponse$new()}}
\item \href{#method-BlueprintResponse-validate}{\code{BlueprintResponse$validate()}}
\item \href{#method-BlueprintResponse-toJSON}{\code{BlueprintResponse$toJSON()}}
\item \href{#method-BlueprintResponse-fromJSON}{\code{BlueprintResponse$fromJSON()}}
\item \href{#method-BlueprintResponse-clone}{\code{BlueprintResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BlueprintResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-BlueprintResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BlueprintResponse$new(
  blueprintCategory = NULL,
  id = NULL,
  modelType = NULL,
  monotonicDecreasingFeaturelistId = NULL,
  monotonicIncreasingFeaturelistId = NULL,
  processes = NULL,
  projectId = NULL,
  recommendedFeaturelistId = NULL,
  supportsComposableMl = NULL,
  supportsMonotonicConstraints = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{blueprintCategory}}{describes the category of the blueprint and indicates the kind of model this blueprint produces. Will be either \"DataRobot\" or \"Scaleout DataRobot\".}

\item{\code{id}}{the blueprint ID of this blueprint - note that this is not an ObjectId.}

\item{\code{modelType}}{the model this blueprint will produce.}

\item{\code{monotonicDecreasingFeaturelistId}}{the ID of the featurelist that defines the set of features with a monotonically decreasing relationship to the target. If null, no such constraints are enforced.}

\item{\code{monotonicIncreasingFeaturelistId}}{null or str, the ID of the featurelist that defines the set of features with a monotonically increasing relationship to the target. If null, no such constraints are enforced.}

\item{\code{processes}}{a list of strings representing processes the blueprint uses.}

\item{\code{projectId}}{the project the blueprint belongs to.}

\item{\code{recommendedFeaturelistId}}{The ID of the feature list recommended for this blueprint. If this field is not present, then there is no recommended feature list.}

\item{\code{supportsComposableMl}}{indicates whether this blueprint is supported in Composable ML.}

\item{\code{supportsMonotonicConstraints}}{whether this model supports enforcing monotonic constraints.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BlueprintResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-BlueprintResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BlueprintResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BlueprintResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-BlueprintResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BlueprintResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BlueprintResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-BlueprintResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BlueprintResponse$fromJSON(BlueprintResponseJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{BlueprintResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BlueprintResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-BlueprintResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BlueprintResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
