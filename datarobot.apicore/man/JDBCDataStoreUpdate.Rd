% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jdbc_data_store_update.R
\docType{class}
\name{JDBCDataStoreUpdate}
\alias{JDBCDataStoreUpdate}
\title{JDBCDataStoreUpdate}
\format{
An \code{R6Class} generator object
}
\description{
JDBCDataStoreUpdate Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{driverId}}{character \link{optional} Driver ID.}

\item{\code{jdbcFields}}{list( \link{JDBCFields} ) \link{optional} The fields used to create the JDBC URL, in the form of a JSON object where parameter name/value pairs are the items in the object. For example: `{\"address\": localhost:5432, \"database\": \"fooBar\", \"connectTimeout\": 10}`. In most cases, all keys in `jdbcFields` should be defined by a schema listed in `jdbcFieldSchemas` from `DriverConfiguration`. The request will be rejected if there are required parameters (as defined by `jdbcFieldSchemas`) that are not provided.}

\item{\code{jdbcUrl}}{character \link{optional} JDBC URL.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-JDBCDataStoreUpdate-new}{\code{JDBCDataStoreUpdate$new()}}
\item \href{#method-JDBCDataStoreUpdate-validate}{\code{JDBCDataStoreUpdate$validate()}}
\item \href{#method-JDBCDataStoreUpdate-toJSON}{\code{JDBCDataStoreUpdate$toJSON()}}
\item \href{#method-JDBCDataStoreUpdate-fromJSON}{\code{JDBCDataStoreUpdate$fromJSON()}}
\item \href{#method-JDBCDataStoreUpdate-clone}{\code{JDBCDataStoreUpdate$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-JDBCDataStoreUpdate-new"></a>}}
\if{latex}{\out{\hypertarget{method-JDBCDataStoreUpdate-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{JDBCDataStoreUpdate$new(
  driverId = NULL,
  jdbcFields = NULL,
  jdbcUrl = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{driverId}}{Driver ID.}

\item{\code{jdbcFields}}{The fields used to create the JDBC URL, in the form of a JSON object where parameter name/value pairs are the items in the object. For example: `{\"address\": localhost:5432, \"database\": \"fooBar\", \"connectTimeout\": 10}`. In most cases, all keys in `jdbcFields` should be defined by a schema listed in `jdbcFieldSchemas` from `DriverConfiguration`. The request will be rejected if there are required parameters (as defined by `jdbcFieldSchemas`) that are not provided.}

\item{\code{jdbcUrl}}{JDBC URL.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-JDBCDataStoreUpdate-validate"></a>}}
\if{latex}{\out{\hypertarget{method-JDBCDataStoreUpdate-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{JDBCDataStoreUpdate$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-JDBCDataStoreUpdate-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-JDBCDataStoreUpdate-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{JDBCDataStoreUpdate$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-JDBCDataStoreUpdate-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-JDBCDataStoreUpdate-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{JDBCDataStoreUpdate$fromJSON(JDBCDataStoreUpdateJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{JDBCDataStoreUpdateJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-JDBCDataStoreUpdate-clone"></a>}}
\if{latex}{\out{\hypertarget{method-JDBCDataStoreUpdate-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{JDBCDataStoreUpdate$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
