% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiseries_retrieve_response.R
\docType{class}
\name{MultiseriesRetrieveResponse}
\alias{MultiseriesRetrieveResponse}
\title{MultiseriesRetrieveResponse}
\format{
An \code{R6Class} generator object
}
\description{
MultiseriesRetrieveResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{datetimePartitionColumn}}{character The datetime partition column name.}

\item{\code{detectedMultiseriesIdColumns}}{list( \link{MultiseriesIdColumnsRecord} ) A list of detected multiseries ID columns along with timeStep and timeUnit information. Note that if no eligible columns have been detected, this list will be empty.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-MultiseriesRetrieveResponse-new}{\code{MultiseriesRetrieveResponse$new()}}
\item \href{#method-MultiseriesRetrieveResponse-validate}{\code{MultiseriesRetrieveResponse$validate()}}
\item \href{#method-MultiseriesRetrieveResponse-toJSON}{\code{MultiseriesRetrieveResponse$toJSON()}}
\item \href{#method-MultiseriesRetrieveResponse-fromJSON}{\code{MultiseriesRetrieveResponse$fromJSON()}}
\item \href{#method-MultiseriesRetrieveResponse-clone}{\code{MultiseriesRetrieveResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MultiseriesRetrieveResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-MultiseriesRetrieveResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MultiseriesRetrieveResponse$new(
  datetimePartitionColumn = NULL,
  detectedMultiseriesIdColumns = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{datetimePartitionColumn}}{The datetime partition column name.}

\item{\code{detectedMultiseriesIdColumns}}{A list of detected multiseries ID columns along with timeStep and timeUnit information. Note that if no eligible columns have been detected, this list will be empty.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MultiseriesRetrieveResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-MultiseriesRetrieveResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MultiseriesRetrieveResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MultiseriesRetrieveResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-MultiseriesRetrieveResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MultiseriesRetrieveResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MultiseriesRetrieveResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-MultiseriesRetrieveResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MultiseriesRetrieveResponse$fromJSON(
  MultiseriesRetrieveResponseJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{MultiseriesRetrieveResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MultiseriesRetrieveResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-MultiseriesRetrieveResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MultiseriesRetrieveResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
