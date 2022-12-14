% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/from_specific.R
\docType{class}
\name{FromSpecific}
\alias{FromSpecific}
\title{FromSpecific}
\format{
An \code{R6Class} generator object
}
\description{
FromSpecific Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{categories}}{\link{OneOfstringarray} \link{optional} An array of strings describing the intended use of the dataset.}

\item{\code{credentialData}}{\link{OneOfBasicCredentialsS3CredentialsOAuthCredentials} \link{optional} The credentials to authenticate with the database, to be used instead of credential ID.}

\item{\code{credentialId}}{character \link{optional} The ID of the set of credentials to authenticate with the database.}

\item{\code{credentials}}{character \link{optional} A list of credentials to use if this is a Spark dataset that requires credentials.}

\item{\code{password}}{character \link{optional} The password (in cleartext) for database authentication. The password will be encrypted on the server-side HTTP request and never saved or stored. Required only if the previous data source was a data source. DEPRECATED: please use credentialId or credentialData instead.}

\item{\code{useKerberos}}{character \link{optional} If true, use Kerberos for database authentication.}

\item{\code{user}}{character \link{optional} The username for database authentication. Required only if the dataset was initially created from a data source. DEPRECATED: please use credentialId or credentialData instead.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-FromSpecific-new}{\code{FromSpecific$new()}}
\item \href{#method-FromSpecific-validate}{\code{FromSpecific$validate()}}
\item \href{#method-FromSpecific-toJSON}{\code{FromSpecific$toJSON()}}
\item \href{#method-FromSpecific-fromJSON}{\code{FromSpecific$fromJSON()}}
\item \href{#method-FromSpecific-clone}{\code{FromSpecific$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FromSpecific-new"></a>}}
\if{latex}{\out{\hypertarget{method-FromSpecific-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FromSpecific$new(
  categories = NULL,
  credentialData = NULL,
  credentialId = NULL,
  credentials = NULL,
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
\item{\code{categories}}{An array of strings describing the intended use of the dataset.}

\item{\code{credentialData}}{The credentials to authenticate with the database, to be used instead of credential ID.}

\item{\code{credentialId}}{The ID of the set of credentials to authenticate with the database.}

\item{\code{credentials}}{A list of credentials to use if this is a Spark dataset that requires credentials.}

\item{\code{password}}{The password (in cleartext) for database authentication. The password will be encrypted on the server-side HTTP request and never saved or stored. Required only if the previous data source was a data source. DEPRECATED: please use credentialId or credentialData instead.}

\item{\code{useKerberos}}{If true, use Kerberos for database authentication.}

\item{\code{user}}{The username for database authentication. Required only if the dataset was initially created from a data source. DEPRECATED: please use credentialId or credentialData instead.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FromSpecific-validate"></a>}}
\if{latex}{\out{\hypertarget{method-FromSpecific-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FromSpecific$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FromSpecific-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-FromSpecific-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FromSpecific$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FromSpecific-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-FromSpecific-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FromSpecific$fromJSON(FromSpecificJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{FromSpecificJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FromSpecific-clone"></a>}}
\if{latex}{\out{\hypertarget{method-FromSpecific-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FromSpecific$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
