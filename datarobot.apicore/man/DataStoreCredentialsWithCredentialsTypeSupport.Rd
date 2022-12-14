% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/data_store_credentials_with_credentials_type_support.R
\docType{class}
\name{DataStoreCredentialsWithCredentialsTypeSupport}
\alias{DataStoreCredentialsWithCredentialsTypeSupport}
\title{DataStoreCredentialsWithCredentialsTypeSupport}
\format{
An \code{R6Class} generator object
}
\description{
DataStoreCredentialsWithCredentialsTypeSupport Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{credentialData}}{\link{OneOfBasicCredentialsS3CredentialsOAuthCredentials} \link{optional} Type of credentials to use with the data store.}

\item{\code{credentialId}}{character \link{optional} ID of the set of credentials to use instead of username and password.}

\item{\code{password}}{character \link{optional} Password for data store authentication.}

\item{\code{useKerberos}}{character \link{optional} Whether to use Kerberos for data store authentication.}

\item{\code{user}}{character \link{optional} Username for data store authentication.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-DataStoreCredentialsWithCredentialsTypeSupport-new}{\code{DataStoreCredentialsWithCredentialsTypeSupport$new()}}
\item \href{#method-DataStoreCredentialsWithCredentialsTypeSupport-validate}{\code{DataStoreCredentialsWithCredentialsTypeSupport$validate()}}
\item \href{#method-DataStoreCredentialsWithCredentialsTypeSupport-toJSON}{\code{DataStoreCredentialsWithCredentialsTypeSupport$toJSON()}}
\item \href{#method-DataStoreCredentialsWithCredentialsTypeSupport-fromJSON}{\code{DataStoreCredentialsWithCredentialsTypeSupport$fromJSON()}}
\item \href{#method-DataStoreCredentialsWithCredentialsTypeSupport-clone}{\code{DataStoreCredentialsWithCredentialsTypeSupport$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataStoreCredentialsWithCredentialsTypeSupport-new"></a>}}
\if{latex}{\out{\hypertarget{method-DataStoreCredentialsWithCredentialsTypeSupport-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataStoreCredentialsWithCredentialsTypeSupport$new(
  credentialData = NULL,
  credentialId = NULL,
  password = NULL,
  useKerberos = FALSE,
  user = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{credentialData}}{Type of credentials to use with the data store.}

\item{\code{credentialId}}{ID of the set of credentials to use instead of username and password.}

\item{\code{password}}{Password for data store authentication.}

\item{\code{useKerberos}}{Whether to use Kerberos for data store authentication.}

\item{\code{user}}{Username for data store authentication.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataStoreCredentialsWithCredentialsTypeSupport-validate"></a>}}
\if{latex}{\out{\hypertarget{method-DataStoreCredentialsWithCredentialsTypeSupport-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataStoreCredentialsWithCredentialsTypeSupport$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataStoreCredentialsWithCredentialsTypeSupport-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-DataStoreCredentialsWithCredentialsTypeSupport-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataStoreCredentialsWithCredentialsTypeSupport$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataStoreCredentialsWithCredentialsTypeSupport-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-DataStoreCredentialsWithCredentialsTypeSupport-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataStoreCredentialsWithCredentialsTypeSupport$fromJSON(
  DataStoreCredentialsWithCredentialsTypeSupportJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{DataStoreCredentialsWithCredentialsTypeSupportJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataStoreCredentialsWithCredentialsTypeSupport-clone"></a>}}
\if{latex}{\out{\hypertarget{method-DataStoreCredentialsWithCredentialsTypeSupport-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataStoreCredentialsWithCredentialsTypeSupport$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
