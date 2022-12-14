% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anomaly_insight_table_retrieve.R
\docType{class}
\name{AnomalyInsightTableRetrieve}
\alias{AnomalyInsightTableRetrieve}
\title{AnomalyInsightTableRetrieve}
\format{
An \code{R6Class} generator object
}
\description{
AnomalyInsightTableRetrieve Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{modelId}}{character given model identifier}

\item{\code{table}}{list( \link{AnomalyInsightTableData} ) anomaly insights table}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-AnomalyInsightTableRetrieve-new}{\code{AnomalyInsightTableRetrieve$new()}}
\item \href{#method-AnomalyInsightTableRetrieve-validate}{\code{AnomalyInsightTableRetrieve$validate()}}
\item \href{#method-AnomalyInsightTableRetrieve-toJSON}{\code{AnomalyInsightTableRetrieve$toJSON()}}
\item \href{#method-AnomalyInsightTableRetrieve-fromJSON}{\code{AnomalyInsightTableRetrieve$fromJSON()}}
\item \href{#method-AnomalyInsightTableRetrieve-clone}{\code{AnomalyInsightTableRetrieve$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AnomalyInsightTableRetrieve-new"></a>}}
\if{latex}{\out{\hypertarget{method-AnomalyInsightTableRetrieve-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AnomalyInsightTableRetrieve$new(
  modelId = NULL,
  table = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{modelId}}{given model identifier}

\item{\code{table}}{anomaly insights table}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AnomalyInsightTableRetrieve-validate"></a>}}
\if{latex}{\out{\hypertarget{method-AnomalyInsightTableRetrieve-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AnomalyInsightTableRetrieve$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AnomalyInsightTableRetrieve-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-AnomalyInsightTableRetrieve-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AnomalyInsightTableRetrieve$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AnomalyInsightTableRetrieve-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-AnomalyInsightTableRetrieve-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AnomalyInsightTableRetrieve$fromJSON(
  AnomalyInsightTableRetrieveJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{AnomalyInsightTableRetrieveJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AnomalyInsightTableRetrieve-clone"></a>}}
\if{latex}{\out{\hypertarget{method-AnomalyInsightTableRetrieve-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AnomalyInsightTableRetrieve$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
