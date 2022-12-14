% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/saml_metadata_file.R
\docType{class}
\name{SamlMetadataFile}
\alias{SamlMetadataFile}
\title{SamlMetadataFile}
\format{
An \code{R6Class} generator object
}
\description{
SamlMetadataFile Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{fileName}}{character Path to IdP metadata file.}

\item{\code{value}}{character IdP metadata.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-SamlMetadataFile-new}{\code{SamlMetadataFile$new()}}
\item \href{#method-SamlMetadataFile-validate}{\code{SamlMetadataFile$validate()}}
\item \href{#method-SamlMetadataFile-toJSON}{\code{SamlMetadataFile$toJSON()}}
\item \href{#method-SamlMetadataFile-fromJSON}{\code{SamlMetadataFile$fromJSON()}}
\item \href{#method-SamlMetadataFile-clone}{\code{SamlMetadataFile$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SamlMetadataFile-new"></a>}}
\if{latex}{\out{\hypertarget{method-SamlMetadataFile-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SamlMetadataFile$new(
  fileName = NULL,
  value = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{fileName}}{Path to IdP metadata file.}

\item{\code{value}}{IdP metadata.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SamlMetadataFile-validate"></a>}}
\if{latex}{\out{\hypertarget{method-SamlMetadataFile-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SamlMetadataFile$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SamlMetadataFile-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-SamlMetadataFile-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SamlMetadataFile$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SamlMetadataFile-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-SamlMetadataFile-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SamlMetadataFile$fromJSON(SamlMetadataFileJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{SamlMetadataFileJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SamlMetadataFile-clone"></a>}}
\if{latex}{\out{\hypertarget{method-SamlMetadataFile-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SamlMetadataFile$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
