% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/application_duplicate.R
\docType{class}
\name{ApplicationDuplicate}
\alias{ApplicationDuplicate}
\title{ApplicationDuplicate}
\format{
An \code{R6Class} generator object
}
\description{
ApplicationDuplicate Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{description}}{character \link{optional} The description of the application}

\item{\code{duplicatePredictions}}{character \link{optional} Import all predictions from the source application}

\item{\code{name}}{character \link{optional} The name of the app}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ApplicationDuplicate-new}{\code{ApplicationDuplicate$new()}}
\item \href{#method-ApplicationDuplicate-validate}{\code{ApplicationDuplicate$validate()}}
\item \href{#method-ApplicationDuplicate-toJSON}{\code{ApplicationDuplicate$toJSON()}}
\item \href{#method-ApplicationDuplicate-fromJSON}{\code{ApplicationDuplicate$fromJSON()}}
\item \href{#method-ApplicationDuplicate-clone}{\code{ApplicationDuplicate$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ApplicationDuplicate-new"></a>}}
\if{latex}{\out{\hypertarget{method-ApplicationDuplicate-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ApplicationDuplicate$new(
  description = NULL,
  duplicatePredictions = FALSE,
  name = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{description}}{The description of the application}

\item{\code{duplicatePredictions}}{Import all predictions from the source application}

\item{\code{name}}{The name of the app}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ApplicationDuplicate-validate"></a>}}
\if{latex}{\out{\hypertarget{method-ApplicationDuplicate-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ApplicationDuplicate$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ApplicationDuplicate-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ApplicationDuplicate-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ApplicationDuplicate$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ApplicationDuplicate-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ApplicationDuplicate-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ApplicationDuplicate$fromJSON(ApplicationDuplicateJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ApplicationDuplicateJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ApplicationDuplicate-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ApplicationDuplicate-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ApplicationDuplicate$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
