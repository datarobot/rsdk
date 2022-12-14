% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/activation_map_links.R
\docType{class}
\name{ActivationMapLinks}
\alias{ActivationMapLinks}
\title{ActivationMapLinks}
\format{
An \code{R6Class} generator object
}
\description{
ActivationMapLinks Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{downloadOriginalImage}}{character URL of the original image}

\item{\code{downloadOverlayImage}}{character URL of the original image overlaid by the activation heatmap}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ActivationMapLinks-new}{\code{ActivationMapLinks$new()}}
\item \href{#method-ActivationMapLinks-validate}{\code{ActivationMapLinks$validate()}}
\item \href{#method-ActivationMapLinks-toJSON}{\code{ActivationMapLinks$toJSON()}}
\item \href{#method-ActivationMapLinks-fromJSON}{\code{ActivationMapLinks$fromJSON()}}
\item \href{#method-ActivationMapLinks-clone}{\code{ActivationMapLinks$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ActivationMapLinks-new"></a>}}
\if{latex}{\out{\hypertarget{method-ActivationMapLinks-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ActivationMapLinks$new(
  downloadOriginalImage = NULL,
  downloadOverlayImage = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{downloadOriginalImage}}{URL of the original image}

\item{\code{downloadOverlayImage}}{URL of the original image overlaid by the activation heatmap}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ActivationMapLinks-validate"></a>}}
\if{latex}{\out{\hypertarget{method-ActivationMapLinks-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ActivationMapLinks$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ActivationMapLinks-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ActivationMapLinks-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ActivationMapLinks$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ActivationMapLinks-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ActivationMapLinks-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ActivationMapLinks$fromJSON(ActivationMapLinksJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ActivationMapLinksJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ActivationMapLinks-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ActivationMapLinks-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ActivationMapLinks$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
