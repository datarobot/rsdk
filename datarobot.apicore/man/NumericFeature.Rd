% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/numeric_feature.R
\docType{class}
\name{NumericFeature}
\alias{NumericFeature}
\title{NumericFeature}
\format{
An \code{R6Class} generator object
}
\description{
NumericFeature Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{featureImpact}}{numeric \link{optional} Feature Impact score.}

\item{\code{featureName}}{character Feature name.}

\item{\code{featureType}}{character Feature Type.}

\item{\code{insights}}{list( \link{Numeric} ) A list of Cluster Insights for a feature.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-NumericFeature-new}{\code{NumericFeature$new()}}
\item \href{#method-NumericFeature-validate}{\code{NumericFeature$validate()}}
\item \href{#method-NumericFeature-toJSON}{\code{NumericFeature$toJSON()}}
\item \href{#method-NumericFeature-fromJSON}{\code{NumericFeature$fromJSON()}}
\item \href{#method-NumericFeature-clone}{\code{NumericFeature$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NumericFeature-new"></a>}}
\if{latex}{\out{\hypertarget{method-NumericFeature-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NumericFeature$new(
  featureName = NULL,
  featureType = NULL,
  insights = NULL,
  featureImpact = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{featureName}}{Feature name.}

\item{\code{featureType}}{Feature Type.}

\item{\code{insights}}{A list of Cluster Insights for a feature.}

\item{\code{featureImpact}}{Feature Impact score.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NumericFeature-validate"></a>}}
\if{latex}{\out{\hypertarget{method-NumericFeature-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NumericFeature$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NumericFeature-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-NumericFeature-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NumericFeature$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NumericFeature-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-NumericFeature-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NumericFeature$fromJSON(NumericFeatureJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{NumericFeatureJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NumericFeature-clone"></a>}}
\if{latex}{\out{\hypertarget{method-NumericFeature-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NumericFeature$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
