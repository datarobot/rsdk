% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dss_data_streamer.R
\docType{class}
\name{DSSDataStreamer}
\alias{DSSDataStreamer}
\title{DSSDataStreamer}
\format{
An \code{R6Class} generator object
}
\description{
DSSDataStreamer Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{datasetId}}{\link{OneOfstringstring} Either the populated value of the field or \link{redacted} due to permission settings}

\item{\code{partition}}{character \link{optional} Partition used to predict}

\item{\code{projectId}}{\link{OneOfstringstring} Either the populated value of the field or \link{redacted} due to permission settings}

\item{\code{type}}{character Type name for this intake type}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-DSSDataStreamer-new}{\code{DSSDataStreamer$new()}}
\item \href{#method-DSSDataStreamer-validate}{\code{DSSDataStreamer$validate()}}
\item \href{#method-DSSDataStreamer-toJSON}{\code{DSSDataStreamer$toJSON()}}
\item \href{#method-DSSDataStreamer-fromJSON}{\code{DSSDataStreamer$fromJSON()}}
\item \href{#method-DSSDataStreamer-clone}{\code{DSSDataStreamer$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DSSDataStreamer-new"></a>}}
\if{latex}{\out{\hypertarget{method-DSSDataStreamer-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DSSDataStreamer$new(
  datasetId = NULL,
  projectId = NULL,
  type = NULL,
  partition = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{datasetId}}{Either the populated value of the field or \link{redacted} due to permission settings}

\item{\code{projectId}}{Either the populated value of the field or \link{redacted} due to permission settings}

\item{\code{type}}{Type name for this intake type}

\item{\code{partition}}{Partition used to predict}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DSSDataStreamer-validate"></a>}}
\if{latex}{\out{\hypertarget{method-DSSDataStreamer-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DSSDataStreamer$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DSSDataStreamer-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-DSSDataStreamer-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DSSDataStreamer$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DSSDataStreamer-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-DSSDataStreamer-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DSSDataStreamer$fromJSON(DSSDataStreamerJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{DSSDataStreamerJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DSSDataStreamer-clone"></a>}}
\if{latex}{\out{\hypertarget{method-DSSDataStreamer-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DSSDataStreamer$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
