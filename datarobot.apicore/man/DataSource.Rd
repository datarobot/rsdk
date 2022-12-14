% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasource.R
\docType{class}
\name{Datasource}
\alias{Datasource}
\title{Datasource}
\format{
An \code{R6Class} generator object
}
\description{
Datasource Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{categories}}{\link{OneOfstringarray} \link{optional} An array of strings describing the intended use of the dataset.}

\item{\code{credentialData}}{\link{OneOfBasicCredentialsS3CredentialsOAuthCredentials} \link{optional} The credentials to authenticate with the database, to be used instead of credential ID.}

\item{\code{credentialId}}{character \link{optional} The ID of the set of credentials to authenticate with the database.}

\item{\code{dataSourceId}}{character The identifier for the DataSource to use as the source of data.}

\item{\code{doSnapshot}}{character \link{optional} If true, create a snapshot dataset; if false, create a remote dataset. Creating snapshots from non-file sources requires an additional permission, `Enable Create Snapshot Data Source`.}

\item{\code{password}}{character \link{optional} The password (in cleartext) for database authentication. The password will be encrypted on the server side in scope of HTTP request and never saved or stored. DEPRECATED: please use credentialId or credentialData instead.}

\item{\code{persistDataAfterIngestion}}{character \link{optional} If true, will enforce saving all data (for download and sampling) and will allow a user to view extended data profile (which includes data statistics like min/max/median/mean, histogram, etc.). If false, will not enforce saving data. The data schema (feature names and types) still will be available. Specifying this parameter to false and `doSnapshot` to true will result in an error.}

\item{\code{sampleSize}}{\link{SampleSize} \link{optional}}

\item{\code{useKerberos}}{character \link{optional} If true, use kerberos authentication for database authentication.}

\item{\code{user}}{character \link{optional} The username for database authentication. DEPRECATED: please use credentialId or credentialData instead.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Datasource-new}{\code{Datasource$new()}}
\item \href{#method-Datasource-validate}{\code{Datasource$validate()}}
\item \href{#method-Datasource-toJSON}{\code{Datasource$toJSON()}}
\item \href{#method-Datasource-fromJSON}{\code{Datasource$fromJSON()}}
\item \href{#method-Datasource-clone}{\code{Datasource$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Datasource-new"></a>}}
\if{latex}{\out{\hypertarget{method-Datasource-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Datasource$new(
  dataSourceId = NULL,
  categories = NULL,
  credentialData = NULL,
  credentialId = NULL,
  doSnapshot = TRUE,
  password = NULL,
  persistDataAfterIngestion = NULL,
  sampleSize = NULL,
  useKerberos = FALSE,
  user = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{dataSourceId}}{The identifier for the DataSource to use as the source of data.}

\item{\code{categories}}{An array of strings describing the intended use of the dataset.}

\item{\code{credentialData}}{The credentials to authenticate with the database, to be used instead of credential ID.}

\item{\code{credentialId}}{The ID of the set of credentials to authenticate with the database.}

\item{\code{doSnapshot}}{If true, create a snapshot dataset; if false, create a remote dataset. Creating snapshots from non-file sources requires an additional permission, `Enable Create Snapshot Data Source`.}

\item{\code{password}}{The password (in cleartext) for database authentication. The password will be encrypted on the server side in scope of HTTP request and never saved or stored. DEPRECATED: please use credentialId or credentialData instead.}

\item{\code{persistDataAfterIngestion}}{If true, will enforce saving all data (for download and sampling) and will allow a user to view extended data profile (which includes data statistics like min/max/median/mean, histogram, etc.). If false, will not enforce saving data. The data schema (feature names and types) still will be available. Specifying this parameter to false and `doSnapshot` to true will result in an error.}

\item{\code{useKerberos}}{If true, use kerberos authentication for database authentication.}

\item{\code{user}}{The username for database authentication. DEPRECATED: please use credentialId or credentialData instead.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Datasource-validate"></a>}}
\if{latex}{\out{\hypertarget{method-Datasource-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Datasource$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Datasource-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-Datasource-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Datasource$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Datasource-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-Datasource-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Datasource$fromJSON(DatasourceJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{DatasourceJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Datasource-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Datasource-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Datasource$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
