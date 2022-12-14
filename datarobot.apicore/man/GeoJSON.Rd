% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geo_json.R
\docType{class}
\name{GeoJSON}
\alias{GeoJSON}
\title{GeoJSON}
\format{
An \code{R6Class} generator object
}
\description{
GeoJSON Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{coordinates}}{list( object ) Coordinate representative of a geometry.}

\item{\code{type}}{character Type of geometry.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-GeoJSON-new}{\code{GeoJSON$new()}}
\item \href{#method-GeoJSON-validate}{\code{GeoJSON$validate()}}
\item \href{#method-GeoJSON-toJSON}{\code{GeoJSON$toJSON()}}
\item \href{#method-GeoJSON-fromJSON}{\code{GeoJSON$fromJSON()}}
\item \href{#method-GeoJSON-clone}{\code{GeoJSON$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GeoJSON-new"></a>}}
\if{latex}{\out{\hypertarget{method-GeoJSON-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GeoJSON$new(coordinates = NULL, type = NULL, validateParams = FALSE, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{coordinates}}{Coordinate representative of a geometry.}

\item{\code{type}}{Type of geometry.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GeoJSON-validate"></a>}}
\if{latex}{\out{\hypertarget{method-GeoJSON-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GeoJSON$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GeoJSON-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-GeoJSON-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GeoJSON$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GeoJSON-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-GeoJSON-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GeoJSON$fromJSON(GeoJSONJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{GeoJSONJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GeoJSON-clone"></a>}}
\if{latex}{\out{\hypertarget{method-GeoJSON-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GeoJSON$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
