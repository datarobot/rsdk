% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/local_file_output.R
\docType{class}
\name{LocalFileOutput}
\alias{LocalFileOutput}
\title{LocalFileOutput}
\format{
An \code{R6Class} generator object
}
\description{
LocalFileOutput Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{type}}{character Type name for this output type}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-LocalFileOutput-new}{\code{LocalFileOutput$new()}}
\item \href{#method-LocalFileOutput-validate}{\code{LocalFileOutput$validate()}}
\item \href{#method-LocalFileOutput-toJSON}{\code{LocalFileOutput$toJSON()}}
\item \href{#method-LocalFileOutput-fromJSON}{\code{LocalFileOutput$fromJSON()}}
\item \href{#method-LocalFileOutput-clone}{\code{LocalFileOutput$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LocalFileOutput-new"></a>}}
\if{latex}{\out{\hypertarget{method-LocalFileOutput-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LocalFileOutput$new(type = NULL, validateParams = FALSE, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{type}}{Type name for this output type}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LocalFileOutput-validate"></a>}}
\if{latex}{\out{\hypertarget{method-LocalFileOutput-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LocalFileOutput$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LocalFileOutput-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-LocalFileOutput-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LocalFileOutput$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LocalFileOutput-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-LocalFileOutput-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LocalFileOutput$fromJSON(LocalFileOutputJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{LocalFileOutputJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LocalFileOutput-clone"></a>}}
\if{latex}{\out{\hypertarget{method-LocalFileOutput-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LocalFileOutput$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
