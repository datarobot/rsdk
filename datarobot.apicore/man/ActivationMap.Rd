% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/activation_map.R
\docType{class}
\name{ActivationMap}
\alias{ActivationMap}
\title{ActivationMap}
\format{
An \code{R6Class} generator object
}
\description{
ActivationMap Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{activationValues}}{list( \link{array[integer]} ) A 2D matrix of values (row-major) representing the activation strengths for particular image regions.}

\item{\code{actualTargetValue}}{\link{OneOfstringinteger} Actual target value of the dataset row.}

\item{\code{featureName}}{character The name of the column containing the image value the activation map is based upon.}

\item{\code{imageHeight}}{integer The height of the original image (in pixels) this activation map has been computed for.}

\item{\code{imageId}}{character ID of the original image this activation map has been computed for.}

\item{\code{imageWidth}}{integer The width of the original image (in pixels) this activation map has been computed for.}

\item{\code{links}}{\link{ActivationMapLinks}}

\item{\code{overlayImageId}}{character ID of the image containing the original image overlaid by the activation heatmap.}

\item{\code{predictedTargetValue}}{\link{OneOfstringinteger} predicted target value of the dataset row containing this image.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ActivationMap-new}{\code{ActivationMap$new()}}
\item \href{#method-ActivationMap-validate}{\code{ActivationMap$validate()}}
\item \href{#method-ActivationMap-toJSON}{\code{ActivationMap$toJSON()}}
\item \href{#method-ActivationMap-fromJSON}{\code{ActivationMap$fromJSON()}}
\item \href{#method-ActivationMap-clone}{\code{ActivationMap$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ActivationMap-new"></a>}}
\if{latex}{\out{\hypertarget{method-ActivationMap-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ActivationMap$new(
  activationValues = NULL,
  actualTargetValue = NULL,
  featureName = NULL,
  imageHeight = NULL,
  imageId = NULL,
  imageWidth = NULL,
  links = NULL,
  overlayImageId = NULL,
  predictedTargetValue = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{activationValues}}{A 2D matrix of values (row-major) representing the activation strengths for particular image regions.}

\item{\code{actualTargetValue}}{Actual target value of the dataset row.}

\item{\code{featureName}}{The name of the column containing the image value the activation map is based upon.}

\item{\code{imageHeight}}{The height of the original image (in pixels) this activation map has been computed for.}

\item{\code{imageId}}{ID of the original image this activation map has been computed for.}

\item{\code{imageWidth}}{The width of the original image (in pixels) this activation map has been computed for.}

\item{\code{overlayImageId}}{ID of the image containing the original image overlaid by the activation heatmap.}

\item{\code{predictedTargetValue}}{predicted target value of the dataset row containing this image.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ActivationMap-validate"></a>}}
\if{latex}{\out{\hypertarget{method-ActivationMap-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ActivationMap$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ActivationMap-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ActivationMap-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ActivationMap$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ActivationMap-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ActivationMap-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ActivationMap$fromJSON(ActivationMapJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ActivationMapJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ActivationMap-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ActivationMap-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ActivationMap$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
