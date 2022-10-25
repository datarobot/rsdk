% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optional_jdbc_query_data_source.R
\docType{class}
\name{OptionalJDBCQueryDataSource}
\alias{OptionalJDBCQueryDataSource}
\title{OptionalJDBCQueryDataSource}
\format{
An \code{R6Class} generator object
}
\description{
OptionalJDBCQueryDataSource Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{dataStoreId}}{character \link{optional} Data store ID for this data source.}

\item{\code{fetchSize}}{integer \link{optional} User-specified fetch size.}

\item{\code{query}}{character \link{optional} The user-specified SQL query. If this is used, then catalog, schema and table will not be used.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{OptionalJDBCQueryDataSource$new()}}
\item \href{#method-validate}{\code{OptionalJDBCQueryDataSource$validate()}}
\item \href{#method-toJSON}{\code{OptionalJDBCQueryDataSource$toJSON()}}
\item \href{#method-fromJSON}{\code{OptionalJDBCQueryDataSource$fromJSON()}}
\item \href{#method-clone}{\code{OptionalJDBCQueryDataSource$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{OptionalJDBCQueryDataSource$new(
  dataStoreId = NULL,
  fetchSize = NULL,
  query = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{dataStoreId}}{Data store ID for this data source.}

\item{\code{fetchSize}}{User-specified fetch size.}

\item{\code{query}}{The user-specified SQL query. If this is used, then catalog, schema and table will not be used.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-validate"></a>}}
\if{latex}{\out{\hypertarget{method-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{OptionalJDBCQueryDataSource$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{OptionalJDBCQueryDataSource$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{OptionalJDBCQueryDataSource$fromJSON(
  OptionalJDBCQueryDataSourceJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{OptionalJDBCQueryDataSourceJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{OptionalJDBCQueryDataSource$clone(deep = FALSE)}\if{html}{\out{</div>}}
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