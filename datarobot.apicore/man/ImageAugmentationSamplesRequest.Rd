% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/image_augmentation_samples_request.R
\docType{class}
\name{ImageAugmentationSamplesRequest}
\alias{ImageAugmentationSamplesRequest}
\title{ImageAugmentationSamplesRequest}
\format{
An \code{R6Class} generator object
}
\description{
ImageAugmentationSamplesRequest Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{featureName}}{character \link{optional} The name of the image feature containing the data to be augmented}

\item{\code{id}}{character \link{optional} Augmentation list id}

\item{\code{inUse}}{character \link{optional} This is set to true when the Augmentation List has been used to train a model}

\item{\code{initialList}}{character \link{optional} Whether this list will be used during autopilot to perform image augmentation}

\item{\code{name}}{character The name of the image augmentation list}

\item{\code{numberOfNewImages}}{integer Number of new rows to add for each existing row}

\item{\code{numberOfRows}}{integer Number of images from the original dataset to be augmented}

\item{\code{projectId}}{character The project containing the image data to be augmented}

\item{\code{samplesId}}{character \link{optional} Image Augmentation list samples ID}

\item{\code{transformationProbability}}{numeric Probability that each enabled transformation will be applied to an image. This does not apply to Horizontal or Vertical Flip, which are set to 50\% always.}

\item{\code{transformations}}{list( \link{Transformation} ) List of Transformations to possibly apply to each image}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ImageAugmentationSamplesRequest-new}{\code{ImageAugmentationSamplesRequest$new()}}
\item \href{#method-ImageAugmentationSamplesRequest-validate}{\code{ImageAugmentationSamplesRequest$validate()}}
\item \href{#method-ImageAugmentationSamplesRequest-toJSON}{\code{ImageAugmentationSamplesRequest$toJSON()}}
\item \href{#method-ImageAugmentationSamplesRequest-fromJSON}{\code{ImageAugmentationSamplesRequest$fromJSON()}}
\item \href{#method-ImageAugmentationSamplesRequest-clone}{\code{ImageAugmentationSamplesRequest$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ImageAugmentationSamplesRequest-new"></a>}}
\if{latex}{\out{\hypertarget{method-ImageAugmentationSamplesRequest-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ImageAugmentationSamplesRequest$new(
  name = NULL,
  numberOfNewImages = NULL,
  numberOfRows = NULL,
  projectId = NULL,
  transformationProbability = NULL,
  transformations = NULL,
  featureName = NULL,
  id = NULL,
  inUse = FALSE,
  initialList = FALSE,
  samplesId = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{The name of the image augmentation list}

\item{\code{numberOfNewImages}}{Number of new rows to add for each existing row}

\item{\code{numberOfRows}}{Number of images from the original dataset to be augmented}

\item{\code{projectId}}{The project containing the image data to be augmented}

\item{\code{transformationProbability}}{Probability that each enabled transformation will be applied to an image. This does not apply to Horizontal or Vertical Flip, which are set to 50\% always.}

\item{\code{transformations}}{List of Transformations to possibly apply to each image}

\item{\code{featureName}}{The name of the image feature containing the data to be augmented}

\item{\code{id}}{Augmentation list id}

\item{\code{inUse}}{This is set to true when the Augmentation List has been used to train a model}

\item{\code{initialList}}{Whether this list will be used during autopilot to perform image augmentation}

\item{\code{samplesId}}{Image Augmentation list samples ID}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ImageAugmentationSamplesRequest-validate"></a>}}
\if{latex}{\out{\hypertarget{method-ImageAugmentationSamplesRequest-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ImageAugmentationSamplesRequest$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ImageAugmentationSamplesRequest-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ImageAugmentationSamplesRequest-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ImageAugmentationSamplesRequest$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ImageAugmentationSamplesRequest-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ImageAugmentationSamplesRequest-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ImageAugmentationSamplesRequest$fromJSON(
  ImageAugmentationSamplesRequestJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ImageAugmentationSamplesRequestJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ImageAugmentationSamplesRequest-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ImageAugmentationSamplesRequest-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ImageAugmentationSamplesRequest$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
