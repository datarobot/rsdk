% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets_credential.R
\docType{class}
\name{DatasetsCredential}
\alias{DatasetsCredential}
\title{DatasetsCredential}
\format{
An \code{R6Class} generator object
}
\description{
DatasetsCredential Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{catalogVersionId}}{character ID of the catalog version}

\item{\code{credentialId}}{character ID of the credential store to be used for the given catalog version}

\item{\code{url}}{character \link{optional} The URL of the datasource}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-DatasetsCredential-new}{\code{DatasetsCredential$new()}}
\item \href{#method-DatasetsCredential-validate}{\code{DatasetsCredential$validate()}}
\item \href{#method-DatasetsCredential-toJSON}{\code{DatasetsCredential$toJSON()}}
\item \href{#method-DatasetsCredential-fromJSON}{\code{DatasetsCredential$fromJSON()}}
\item \href{#method-DatasetsCredential-clone}{\code{DatasetsCredential$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DatasetsCredential-new"></a>}}
\if{latex}{\out{\hypertarget{method-DatasetsCredential-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DatasetsCredential$new(
  catalogVersionId = NULL,
  credentialId = NULL,
  url = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{catalogVersionId}}{ID of the catalog version}

\item{\code{credentialId}}{ID of the credential store to be used for the given catalog version}

\item{\code{url}}{The URL of the datasource}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DatasetsCredential-validate"></a>}}
\if{latex}{\out{\hypertarget{method-DatasetsCredential-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DatasetsCredential$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DatasetsCredential-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-DatasetsCredential-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DatasetsCredential$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DatasetsCredential-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-DatasetsCredential-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DatasetsCredential$fromJSON(DatasetsCredentialJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{DatasetsCredentialJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DatasetsCredential-clone"></a>}}
\if{latex}{\out{\hypertarget{method-DatasetsCredential-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DatasetsCredential$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
