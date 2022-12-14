% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/password_credentials.R
\docType{class}
\name{PasswordCredentials}
\alias{PasswordCredentials}
\title{PasswordCredentials}
\format{
An \code{R6Class} generator object
}
\description{
PasswordCredentials Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{catalogVersionId}}{character \link{optional} The ID of the latest version of the catalog entry.}

\item{\code{password}}{character The password (in cleartext) for database authentication. The password will be encrypted on the server side in scope of HTTP request and never saved or stored.}

\item{\code{url}}{character \link{optional} The link to retrieve more detailed information about the entity that uses this catalog dataset.}

\item{\code{user}}{character The username for database authentication.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PasswordCredentials-new}{\code{PasswordCredentials$new()}}
\item \href{#method-PasswordCredentials-validate}{\code{PasswordCredentials$validate()}}
\item \href{#method-PasswordCredentials-toJSON}{\code{PasswordCredentials$toJSON()}}
\item \href{#method-PasswordCredentials-fromJSON}{\code{PasswordCredentials$fromJSON()}}
\item \href{#method-PasswordCredentials-clone}{\code{PasswordCredentials$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PasswordCredentials-new"></a>}}
\if{latex}{\out{\hypertarget{method-PasswordCredentials-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PasswordCredentials$new(
  password = NULL,
  user = NULL,
  catalogVersionId = NULL,
  url = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{password}}{The password (in cleartext) for database authentication. The password will be encrypted on the server side in scope of HTTP request and never saved or stored.}

\item{\code{user}}{The username for database authentication.}

\item{\code{catalogVersionId}}{The ID of the latest version of the catalog entry.}

\item{\code{url}}{The link to retrieve more detailed information about the entity that uses this catalog dataset.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PasswordCredentials-validate"></a>}}
\if{latex}{\out{\hypertarget{method-PasswordCredentials-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PasswordCredentials$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PasswordCredentials-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-PasswordCredentials-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PasswordCredentials$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PasswordCredentials-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-PasswordCredentials-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PasswordCredentials$fromJSON(PasswordCredentialsJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{PasswordCredentialsJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PasswordCredentials-clone"></a>}}
\if{latex}{\out{\hypertarget{method-PasswordCredentials-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PasswordCredentials$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
