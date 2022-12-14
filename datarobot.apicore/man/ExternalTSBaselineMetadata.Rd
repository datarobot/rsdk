% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/external_ts_baseline_metadata.R
\docType{class}
\name{ExternalTSBaselineMetadata}
\alias{ExternalTSBaselineMetadata}
\title{ExternalTSBaselineMetadata}
\format{
An \code{R6Class} generator object
}
\description{
ExternalTSBaselineMetadata Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{datasetId}}{character Catalog version id for external prediction data that can be used as a baseline to calculate new metrics.}

\item{\code{datasetName}}{character The name of the timeseries baseline dataset for the project}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ExternalTSBaselineMetadata-new}{\code{ExternalTSBaselineMetadata$new()}}
\item \href{#method-ExternalTSBaselineMetadata-validate}{\code{ExternalTSBaselineMetadata$validate()}}
\item \href{#method-ExternalTSBaselineMetadata-toJSON}{\code{ExternalTSBaselineMetadata$toJSON()}}
\item \href{#method-ExternalTSBaselineMetadata-fromJSON}{\code{ExternalTSBaselineMetadata$fromJSON()}}
\item \href{#method-ExternalTSBaselineMetadata-clone}{\code{ExternalTSBaselineMetadata$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ExternalTSBaselineMetadata-new"></a>}}
\if{latex}{\out{\hypertarget{method-ExternalTSBaselineMetadata-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ExternalTSBaselineMetadata$new(
  datasetId = NULL,
  datasetName = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{datasetId}}{Catalog version id for external prediction data that can be used as a baseline to calculate new metrics.}

\item{\code{datasetName}}{The name of the timeseries baseline dataset for the project}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ExternalTSBaselineMetadata-validate"></a>}}
\if{latex}{\out{\hypertarget{method-ExternalTSBaselineMetadata-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ExternalTSBaselineMetadata$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ExternalTSBaselineMetadata-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ExternalTSBaselineMetadata-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ExternalTSBaselineMetadata$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ExternalTSBaselineMetadata-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ExternalTSBaselineMetadata-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ExternalTSBaselineMetadata$fromJSON(
  ExternalTSBaselineMetadataJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ExternalTSBaselineMetadataJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ExternalTSBaselineMetadata-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ExternalTSBaselineMetadata-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ExternalTSBaselineMetadata$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
