% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/snowflake_output.R
\docType{class}
\name{SnowflakeOutput}
\alias{SnowflakeOutput}
\title{SnowflakeOutput}
\format{
An \code{R6Class} generator object
}
\description{
SnowflakeOutput Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{catalog}}{character \link{optional} The name of the specified database catalog to write output data to.}

\item{\code{cloudStorageCredentialId}}{character \link{optional} The ID of the credential holding information about a user with write access to the cloud storage.}

\item{\code{cloudStorageType}}{character \link{optional} Type name for cloud storage}

\item{\code{createTableIfNotExists}}{character \link{optional} Attempt to create the table first if no existing one is detected, before writing data with the strategy defined in the `statementType` parameter.}

\item{\code{credentialId}}{character \link{optional} The ID of the credential holding information about a user with write access to the Snowflake data source.}

\item{\code{dataStoreId}}{character ID of the data store to connect to}

\item{\code{externalStage}}{character External storage}

\item{\code{schema}}{character \link{optional} The name of the specified database schema to write results to.}

\item{\code{statementType}}{character The statement type to use when writing the results.}

\item{\code{table}}{character The name of the specified database table to write results to.}

\item{\code{type}}{character Type name for this output type}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-SnowflakeOutput-new}{\code{SnowflakeOutput$new()}}
\item \href{#method-SnowflakeOutput-validate}{\code{SnowflakeOutput$validate()}}
\item \href{#method-SnowflakeOutput-toJSON}{\code{SnowflakeOutput$toJSON()}}
\item \href{#method-SnowflakeOutput-fromJSON}{\code{SnowflakeOutput$fromJSON()}}
\item \href{#method-SnowflakeOutput-clone}{\code{SnowflakeOutput$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SnowflakeOutput-new"></a>}}
\if{latex}{\out{\hypertarget{method-SnowflakeOutput-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SnowflakeOutput$new(
  dataStoreId = NULL,
  externalStage = NULL,
  statementType = NULL,
  table = NULL,
  type = NULL,
  catalog = NULL,
  cloudStorageCredentialId = NULL,
  cloudStorageType = "s3",
  createTableIfNotExists = FALSE,
  credentialId = NULL,
  schema = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{dataStoreId}}{ID of the data store to connect to}

\item{\code{externalStage}}{External storage}

\item{\code{statementType}}{The statement type to use when writing the results.}

\item{\code{table}}{The name of the specified database table to write results to.}

\item{\code{type}}{Type name for this output type}

\item{\code{catalog}}{The name of the specified database catalog to write output data to.}

\item{\code{cloudStorageCredentialId}}{The ID of the credential holding information about a user with write access to the cloud storage.}

\item{\code{cloudStorageType}}{Type name for cloud storage}

\item{\code{createTableIfNotExists}}{Attempt to create the table first if no existing one is detected, before writing data with the strategy defined in the `statementType` parameter.}

\item{\code{credentialId}}{The ID of the credential holding information about a user with write access to the Snowflake data source.}

\item{\code{schema}}{The name of the specified database schema to write results to.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SnowflakeOutput-validate"></a>}}
\if{latex}{\out{\hypertarget{method-SnowflakeOutput-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SnowflakeOutput$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SnowflakeOutput-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-SnowflakeOutput-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SnowflakeOutput$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SnowflakeOutput-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-SnowflakeOutput-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SnowflakeOutput$fromJSON(SnowflakeOutputJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{SnowflakeOutputJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SnowflakeOutput-clone"></a>}}
\if{latex}{\out{\hypertarget{method-SnowflakeOutput-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SnowflakeOutput$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
