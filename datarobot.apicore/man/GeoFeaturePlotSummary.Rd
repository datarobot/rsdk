% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geo_feature_plot_summary.R
\docType{class}
\name{GeoFeaturePlotSummary}
\alias{GeoFeaturePlotSummary}
\title{GeoFeaturePlotSummary}
\format{
An \code{R6Class} generator object
}
\description{
GeoFeaturePlotSummary Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{maxCount}}{integer Max num of samples located within one geometry.}

\item{\code{minCount}}{integer Min num of samples located within one geometry.}

\item{\code{totalCount}}{integer Total num of samples across all geometry objects.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-GeoFeaturePlotSummary-new}{\code{GeoFeaturePlotSummary$new()}}
\item \href{#method-GeoFeaturePlotSummary-validate}{\code{GeoFeaturePlotSummary$validate()}}
\item \href{#method-GeoFeaturePlotSummary-toJSON}{\code{GeoFeaturePlotSummary$toJSON()}}
\item \href{#method-GeoFeaturePlotSummary-fromJSON}{\code{GeoFeaturePlotSummary$fromJSON()}}
\item \href{#method-GeoFeaturePlotSummary-clone}{\code{GeoFeaturePlotSummary$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GeoFeaturePlotSummary-new"></a>}}
\if{latex}{\out{\hypertarget{method-GeoFeaturePlotSummary-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GeoFeaturePlotSummary$new(
  maxCount = NULL,
  minCount = NULL,
  totalCount = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{maxCount}}{Max num of samples located within one geometry.}

\item{\code{minCount}}{Min num of samples located within one geometry.}

\item{\code{totalCount}}{Total num of samples across all geometry objects.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GeoFeaturePlotSummary-validate"></a>}}
\if{latex}{\out{\hypertarget{method-GeoFeaturePlotSummary-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GeoFeaturePlotSummary$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GeoFeaturePlotSummary-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-GeoFeaturePlotSummary-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GeoFeaturePlotSummary$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GeoFeaturePlotSummary-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-GeoFeaturePlotSummary-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GeoFeaturePlotSummary$fromJSON(
  GeoFeaturePlotSummaryJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{GeoFeaturePlotSummaryJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GeoFeaturePlotSummary-clone"></a>}}
\if{latex}{\out{\hypertarget{method-GeoFeaturePlotSummary-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GeoFeaturePlotSummary$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
