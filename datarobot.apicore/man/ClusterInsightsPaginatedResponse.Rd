% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cluster_insights_paginated_response.R
\docType{class}
\name{ClusterInsightsPaginatedResponse}
\alias{ClusterInsightsPaginatedResponse}
\title{ClusterInsightsPaginatedResponse}
\format{
An \code{R6Class} generator object
}
\description{
ClusterInsightsPaginatedResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{count}}{integer \link{optional} Number of items returned on this page.}

\item{\code{data}}{list( \link{AnyOfImageFeatureGeospatialFeatureTextFeatureNumericFeatureCategoricalFeatureDocumentFeature} ) A list of features with clusters insights.}

\item{\code{isCurrentClusterInsightVersion}}{character If retrieved insights are current version.}

\item{\code{next_}}{character URL pointing to the next page (if null, there is no next page).}

\item{\code{previous}}{character URL pointing to the previous page (if null, there is no previous page).}

\item{\code{totalCount}}{integer The total number of items across all pages.}

\item{\code{version}}{integer Current version of the computed insight.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ClusterInsightsPaginatedResponse-new}{\code{ClusterInsightsPaginatedResponse$new()}}
\item \href{#method-ClusterInsightsPaginatedResponse-validate}{\code{ClusterInsightsPaginatedResponse$validate()}}
\item \href{#method-ClusterInsightsPaginatedResponse-toJSON}{\code{ClusterInsightsPaginatedResponse$toJSON()}}
\item \href{#method-ClusterInsightsPaginatedResponse-fromJSON}{\code{ClusterInsightsPaginatedResponse$fromJSON()}}
\item \href{#method-ClusterInsightsPaginatedResponse-clone}{\code{ClusterInsightsPaginatedResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ClusterInsightsPaginatedResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-ClusterInsightsPaginatedResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ClusterInsightsPaginatedResponse$new(
  data = NULL,
  isCurrentClusterInsightVersion = NULL,
  next_ = NULL,
  previous = NULL,
  totalCount = NULL,
  version = NULL,
  count = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{A list of features with clusters insights.}

\item{\code{isCurrentClusterInsightVersion}}{If retrieved insights are current version.}

\item{\code{next_}}{URL pointing to the next page (if null, there is no next page).}

\item{\code{previous}}{URL pointing to the previous page (if null, there is no previous page).}

\item{\code{totalCount}}{The total number of items across all pages.}

\item{\code{version}}{Current version of the computed insight.}

\item{\code{count}}{Number of items returned on this page.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ClusterInsightsPaginatedResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-ClusterInsightsPaginatedResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ClusterInsightsPaginatedResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ClusterInsightsPaginatedResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ClusterInsightsPaginatedResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ClusterInsightsPaginatedResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ClusterInsightsPaginatedResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ClusterInsightsPaginatedResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ClusterInsightsPaginatedResponse$fromJSON(
  ClusterInsightsPaginatedResponseJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ClusterInsightsPaginatedResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ClusterInsightsPaginatedResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ClusterInsightsPaginatedResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ClusterInsightsPaginatedResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
