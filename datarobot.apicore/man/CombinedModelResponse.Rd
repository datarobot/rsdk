% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combined_model_response.R
\docType{class}
\name{CombinedModelResponse}
\alias{CombinedModelResponse}
\title{CombinedModelResponse}
\format{
An \code{R6Class} generator object
}
\description{
CombinedModelResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{combinedModelId}}{character The ID of combined model.}

\item{\code{modelCategory}}{character Indicates what kind of model this is. Will be ``combined`` for combined models.}

\item{\code{projectId}}{character The ID of the project.}

\item{\code{segmentationTaskId}}{character The ID of the segmentation task used to generate this combined model.}

\item{\code{segments}}{list( \link{SegmentProjectModelResponse} ) Information for each segment. Maps each segment to the project and model used for it.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{CombinedModelResponse$new()}}
\item \href{#method-validate}{\code{CombinedModelResponse$validate()}}
\item \href{#method-toJSON}{\code{CombinedModelResponse$toJSON()}}
\item \href{#method-fromJSON}{\code{CombinedModelResponse$fromJSON()}}
\item \href{#method-clone}{\code{CombinedModelResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CombinedModelResponse$new(
  combinedModelId = NULL,
  modelCategory = NULL,
  projectId = NULL,
  segmentationTaskId = NULL,
  segments = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{combinedModelId}}{The ID of combined model.}

\item{\code{modelCategory}}{Indicates what kind of model this is. Will be ``combined`` for combined models.}

\item{\code{projectId}}{The ID of the project.}

\item{\code{segmentationTaskId}}{The ID of the segmentation task used to generate this combined model.}

\item{\code{segments}}{Information for each segment. Maps each segment to the project and model used for it.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-validate"></a>}}
\if{latex}{\out{\hypertarget{method-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CombinedModelResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CombinedModelResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CombinedModelResponse$fromJSON(
  CombinedModelResponseJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{CombinedModelResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CombinedModelResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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