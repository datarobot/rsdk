% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/feature_counts.R
\docType{class}
\name{FeatureCounts}
\alias{FeatureCounts}
\title{FeatureCounts}
\format{
An \code{R6Class} generator object
}
\description{
FeatureCounts Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{count}}{integer Number of times the class was encountered.}

\item{\code{label}}{character Name of the class.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-FeatureCounts-new}{\code{FeatureCounts$new()}}
\item \href{#method-FeatureCounts-validate}{\code{FeatureCounts$validate()}}
\item \href{#method-FeatureCounts-toJSON}{\code{FeatureCounts$toJSON()}}
\item \href{#method-FeatureCounts-fromJSON}{\code{FeatureCounts$fromJSON()}}
\item \href{#method-FeatureCounts-clone}{\code{FeatureCounts$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FeatureCounts-new"></a>}}
\if{latex}{\out{\hypertarget{method-FeatureCounts-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FeatureCounts$new(count = NULL, label = NULL, validateParams = FALSE, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{count}}{Number of times the class was encountered.}

\item{\code{label}}{Name of the class.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FeatureCounts-validate"></a>}}
\if{latex}{\out{\hypertarget{method-FeatureCounts-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FeatureCounts$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FeatureCounts-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-FeatureCounts-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FeatureCounts$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FeatureCounts-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-FeatureCounts-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FeatureCounts$fromJSON(FeatureCountsJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{FeatureCountsJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FeatureCounts-clone"></a>}}
\if{latex}{\out{\hypertarget{method-FeatureCounts-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FeatureCounts$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
