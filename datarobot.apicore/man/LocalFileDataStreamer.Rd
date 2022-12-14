% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/local_file_data_streamer.R
\docType{class}
\name{LocalFileDataStreamer}
\alias{LocalFileDataStreamer}
\title{LocalFileDataStreamer}
\format{
An \code{R6Class} generator object
}
\description{
LocalFileDataStreamer Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{async}}{character \link{optional} The default behavior (async: true) will still submit the job to the queue and start processing as soon as the upload is started.Setting it to false will postpone submitting the job to the queue until all data has been uploaded.This is helpful if the user is on a bad connection and bottlednecked by the upload speed. Instead of blocking the queue this will allow others to submit to the queue until the upload has finished.}

\item{\code{multipart}}{character \link{optional} specify if the data will be uploaded in multiple parts instead of a single file}

\item{\code{type}}{character Type name for this intake type}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-LocalFileDataStreamer-new}{\code{LocalFileDataStreamer$new()}}
\item \href{#method-LocalFileDataStreamer-validate}{\code{LocalFileDataStreamer$validate()}}
\item \href{#method-LocalFileDataStreamer-toJSON}{\code{LocalFileDataStreamer$toJSON()}}
\item \href{#method-LocalFileDataStreamer-fromJSON}{\code{LocalFileDataStreamer$fromJSON()}}
\item \href{#method-LocalFileDataStreamer-clone}{\code{LocalFileDataStreamer$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LocalFileDataStreamer-new"></a>}}
\if{latex}{\out{\hypertarget{method-LocalFileDataStreamer-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LocalFileDataStreamer$new(
  type = NULL,
  async = NULL,
  multipart = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{type}}{Type name for this intake type}

\item{\code{async}}{The default behavior (async: true) will still submit the job to the queue and start processing as soon as the upload is started.Setting it to false will postpone submitting the job to the queue until all data has been uploaded.This is helpful if the user is on a bad connection and bottlednecked by the upload speed. Instead of blocking the queue this will allow others to submit to the queue until the upload has finished.}

\item{\code{multipart}}{specify if the data will be uploaded in multiple parts instead of a single file}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LocalFileDataStreamer-validate"></a>}}
\if{latex}{\out{\hypertarget{method-LocalFileDataStreamer-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LocalFileDataStreamer$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LocalFileDataStreamer-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-LocalFileDataStreamer-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LocalFileDataStreamer$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LocalFileDataStreamer-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-LocalFileDataStreamer-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LocalFileDataStreamer$fromJSON(
  LocalFileDataStreamerJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{LocalFileDataStreamerJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LocalFileDataStreamer-clone"></a>}}
\if{latex}{\out{\hypertarget{method-LocalFileDataStreamer-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LocalFileDataStreamer$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
