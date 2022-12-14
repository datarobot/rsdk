% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gcp_output.R
\docType{class}
\name{GCPOutput}
\alias{GCPOutput}
\title{GCPOutput}
\format{
An \code{R6Class} generator object
}
\description{
GCPOutput Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{credentialId}}{character \link{optional} Use the specified credential to access the url}

\item{\code{format}}{character \link{optional} Type of input file format}

\item{\code{partitionColumns}}{list( character ) \link{optional} For Parquet directory-scoring only. The column names of the intake data of which to partition the dataset. Columns are partitioned in the order they are given. At least one value is required if scoring to a directory (meaning the output url ends with a slash (\"/\").}

\item{\code{type}}{character Type name for this output type}

\item{\code{url}}{character URL for the CSV file}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-GCPOutput-new}{\code{GCPOutput$new()}}
\item \href{#method-GCPOutput-validate}{\code{GCPOutput$validate()}}
\item \href{#method-GCPOutput-toJSON}{\code{GCPOutput$toJSON()}}
\item \href{#method-GCPOutput-fromJSON}{\code{GCPOutput$fromJSON()}}
\item \href{#method-GCPOutput-clone}{\code{GCPOutput$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GCPOutput-new"></a>}}
\if{latex}{\out{\hypertarget{method-GCPOutput-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GCPOutput$new(
  type = NULL,
  url = NULL,
  credentialId = NULL,
  format = "csv",
  partitionColumns = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{type}}{Type name for this output type}

\item{\code{url}}{URL for the CSV file}

\item{\code{credentialId}}{Use the specified credential to access the url}

\item{\code{format}}{Type of input file format}

\item{\code{partitionColumns}}{For Parquet directory-scoring only. The column names of the intake data of which to partition the dataset. Columns are partitioned in the order they are given. At least one value is required if scoring to a directory (meaning the output url ends with a slash (\"/\").}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GCPOutput-validate"></a>}}
\if{latex}{\out{\hypertarget{method-GCPOutput-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GCPOutput$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GCPOutput-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-GCPOutput-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GCPOutput$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GCPOutput-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-GCPOutput-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GCPOutput$fromJSON(GCPOutputJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{GCPOutputJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GCPOutput-clone"></a>}}
\if{latex}{\out{\hypertarget{method-GCPOutput-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GCPOutput$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
