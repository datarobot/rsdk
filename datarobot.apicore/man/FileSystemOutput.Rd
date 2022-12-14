% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/file_system_output.R
\docType{class}
\name{FileSystemOutput}
\alias{FileSystemOutput}
\title{FileSystemOutput}
\format{
An \code{R6Class} generator object
}
\description{
FileSystemOutput Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{path}}{character Path to results on host filesystem}

\item{\code{type}}{character Type name for this output type}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-FileSystemOutput-new}{\code{FileSystemOutput$new()}}
\item \href{#method-FileSystemOutput-validate}{\code{FileSystemOutput$validate()}}
\item \href{#method-FileSystemOutput-toJSON}{\code{FileSystemOutput$toJSON()}}
\item \href{#method-FileSystemOutput-fromJSON}{\code{FileSystemOutput$fromJSON()}}
\item \href{#method-FileSystemOutput-clone}{\code{FileSystemOutput$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FileSystemOutput-new"></a>}}
\if{latex}{\out{\hypertarget{method-FileSystemOutput-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FileSystemOutput$new(path = NULL, type = NULL, validateParams = FALSE, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{path}}{Path to results on host filesystem}

\item{\code{type}}{Type name for this output type}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FileSystemOutput-validate"></a>}}
\if{latex}{\out{\hypertarget{method-FileSystemOutput-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FileSystemOutput$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FileSystemOutput-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-FileSystemOutput-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FileSystemOutput$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FileSystemOutput-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-FileSystemOutput-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FileSystemOutput$fromJSON(FileSystemOutputJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{FileSystemOutputJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FileSystemOutput-clone"></a>}}
\if{latex}{\out{\hypertarget{method-FileSystemOutput-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FileSystemOutput$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
