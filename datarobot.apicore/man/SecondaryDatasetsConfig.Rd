% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/secondary_datasets_config.R
\docType{class}
\name{SecondaryDatasetsConfig}
\alias{SecondaryDatasetsConfig}
\title{SecondaryDatasetsConfig}
\format{
An \code{R6Class} generator object
}
\description{
SecondaryDatasetsConfig Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{configId}}{character ID of the secondary datasets configuration}

\item{\code{configName}}{character Name of the secondary datasets config}

\item{\code{updated}}{character Timestamp when configuration was updated on the given deployment}

\item{\code{username}}{character Name of the user who made the update}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-SecondaryDatasetsConfig-new}{\code{SecondaryDatasetsConfig$new()}}
\item \href{#method-SecondaryDatasetsConfig-validate}{\code{SecondaryDatasetsConfig$validate()}}
\item \href{#method-SecondaryDatasetsConfig-toJSON}{\code{SecondaryDatasetsConfig$toJSON()}}
\item \href{#method-SecondaryDatasetsConfig-fromJSON}{\code{SecondaryDatasetsConfig$fromJSON()}}
\item \href{#method-SecondaryDatasetsConfig-clone}{\code{SecondaryDatasetsConfig$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SecondaryDatasetsConfig-new"></a>}}
\if{latex}{\out{\hypertarget{method-SecondaryDatasetsConfig-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SecondaryDatasetsConfig$new(
  configId = NULL,
  configName = NULL,
  updated = NULL,
  username = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{configId}}{ID of the secondary datasets configuration}

\item{\code{configName}}{Name of the secondary datasets config}

\item{\code{updated}}{Timestamp when configuration was updated on the given deployment}

\item{\code{username}}{Name of the user who made the update}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SecondaryDatasetsConfig-validate"></a>}}
\if{latex}{\out{\hypertarget{method-SecondaryDatasetsConfig-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SecondaryDatasetsConfig$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SecondaryDatasetsConfig-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-SecondaryDatasetsConfig-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SecondaryDatasetsConfig$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SecondaryDatasetsConfig-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-SecondaryDatasetsConfig-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SecondaryDatasetsConfig$fromJSON(
  SecondaryDatasetsConfigJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{SecondaryDatasetsConfigJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SecondaryDatasetsConfig-clone"></a>}}
\if{latex}{\out{\hypertarget{method-SecondaryDatasetsConfig-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SecondaryDatasetsConfig$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
