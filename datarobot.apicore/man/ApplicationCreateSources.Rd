% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/application_create_sources.R
\docType{class}
\name{ApplicationCreateSources}
\alias{ApplicationCreateSources}
\title{ApplicationCreateSources}
\format{
An \code{R6Class} generator object
}
\description{
ApplicationCreateSources Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{info}}{\link{ApplicationDeploymentSource}}

\item{\code{name}}{character \link{optional} The name of this source.}

\item{\code{source}}{character Information about the source for this application.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ApplicationCreateSources-new}{\code{ApplicationCreateSources$new()}}
\item \href{#method-ApplicationCreateSources-validate}{\code{ApplicationCreateSources$validate()}}
\item \href{#method-ApplicationCreateSources-toJSON}{\code{ApplicationCreateSources$toJSON()}}
\item \href{#method-ApplicationCreateSources-fromJSON}{\code{ApplicationCreateSources$fromJSON()}}
\item \href{#method-ApplicationCreateSources-clone}{\code{ApplicationCreateSources$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ApplicationCreateSources-new"></a>}}
\if{latex}{\out{\hypertarget{method-ApplicationCreateSources-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ApplicationCreateSources$new(
  info = NULL,
  source = NULL,
  name = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{source}}{Information about the source for this application.}

\item{\code{name}}{The name of this source.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ApplicationCreateSources-validate"></a>}}
\if{latex}{\out{\hypertarget{method-ApplicationCreateSources-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ApplicationCreateSources$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ApplicationCreateSources-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ApplicationCreateSources-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ApplicationCreateSources$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ApplicationCreateSources-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ApplicationCreateSources-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ApplicationCreateSources$fromJSON(
  ApplicationCreateSourcesJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ApplicationCreateSourcesJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ApplicationCreateSources-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ApplicationCreateSources-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ApplicationCreateSources$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
