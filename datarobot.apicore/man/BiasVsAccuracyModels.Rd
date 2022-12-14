% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bias_vs_accuracy_models.R
\docType{class}
\name{BiasVsAccuracyModels}
\alias{BiasVsAccuracyModels}
\title{BiasVsAccuracyModels}
\format{
An \code{R6Class} generator object
}
\description{
BiasVsAccuracyModels Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{accuracyValue}}{numeric The model's accuracy score.}

\item{\code{bp}}{integer The blueprint number of the model from the leaderboard.}

\item{\code{dsName}}{character The name of the feature list used for model training.}

\item{\code{fairnessValue}}{numeric The model's relative fairness score for the class with the lowest fairness score. In other words, the fairness score of the least privileged class.}

\item{\code{modelId}}{character ID of the model.}

\item{\code{modelNumber}}{integer The model number from the leaderboard.}

\item{\code{modelType}}{character The type/name of the model.}

\item{\code{prime}}{character Flag to indicate whether the model is a prime model.}

\item{\code{samplepct}}{numeric The sample size percentage of the feature list data the model was trained on.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-BiasVsAccuracyModels-new}{\code{BiasVsAccuracyModels$new()}}
\item \href{#method-BiasVsAccuracyModels-validate}{\code{BiasVsAccuracyModels$validate()}}
\item \href{#method-BiasVsAccuracyModels-toJSON}{\code{BiasVsAccuracyModels$toJSON()}}
\item \href{#method-BiasVsAccuracyModels-fromJSON}{\code{BiasVsAccuracyModels$fromJSON()}}
\item \href{#method-BiasVsAccuracyModels-clone}{\code{BiasVsAccuracyModels$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BiasVsAccuracyModels-new"></a>}}
\if{latex}{\out{\hypertarget{method-BiasVsAccuracyModels-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BiasVsAccuracyModels$new(
  accuracyValue = NULL,
  bp = NULL,
  dsName = NULL,
  fairnessValue = NULL,
  modelId = NULL,
  modelNumber = NULL,
  modelType = NULL,
  prime = NULL,
  samplepct = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{accuracyValue}}{The model's accuracy score.}

\item{\code{bp}}{The blueprint number of the model from the leaderboard.}

\item{\code{dsName}}{The name of the feature list used for model training.}

\item{\code{fairnessValue}}{The model's relative fairness score for the class with the lowest fairness score. In other words, the fairness score of the least privileged class.}

\item{\code{modelId}}{ID of the model.}

\item{\code{modelNumber}}{The model number from the leaderboard.}

\item{\code{modelType}}{The type/name of the model.}

\item{\code{prime}}{Flag to indicate whether the model is a prime model.}

\item{\code{samplepct}}{The sample size percentage of the feature list data the model was trained on.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BiasVsAccuracyModels-validate"></a>}}
\if{latex}{\out{\hypertarget{method-BiasVsAccuracyModels-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BiasVsAccuracyModels$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BiasVsAccuracyModels-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-BiasVsAccuracyModels-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BiasVsAccuracyModels$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BiasVsAccuracyModels-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-BiasVsAccuracyModels-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BiasVsAccuracyModels$fromJSON(BiasVsAccuracyModelsJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{BiasVsAccuracyModelsJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BiasVsAccuracyModels-clone"></a>}}
\if{latex}{\out{\hypertarget{method-BiasVsAccuracyModels-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BiasVsAccuracyModels$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
