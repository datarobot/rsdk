% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/feature_impact_create_response.R
\docType{class}
\name{FeatureImpactCreateResponse}
\alias{FeatureImpactCreateResponse}
\title{FeatureImpactCreateResponse}
\format{
An \code{R6Class} generator object
}
\description{
FeatureImpactCreateResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{statusId}}{character ID that can be used with :http:get:`/api/v2/status/(statusId)/` for tracking job status.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-FeatureImpactCreateResponse-new}{\code{FeatureImpactCreateResponse$new()}}
\item \href{#method-FeatureImpactCreateResponse-validate}{\code{FeatureImpactCreateResponse$validate()}}
\item \href{#method-FeatureImpactCreateResponse-toJSON}{\code{FeatureImpactCreateResponse$toJSON()}}
\item \href{#method-FeatureImpactCreateResponse-fromJSON}{\code{FeatureImpactCreateResponse$fromJSON()}}
\item \href{#method-FeatureImpactCreateResponse-clone}{\code{FeatureImpactCreateResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FeatureImpactCreateResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-FeatureImpactCreateResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FeatureImpactCreateResponse$new(statusId = NULL, validateParams = FALSE, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{statusId}}{ID that can be used with :http:get:`/api/v2/status/(statusId)/` for tracking job status.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FeatureImpactCreateResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-FeatureImpactCreateResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FeatureImpactCreateResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FeatureImpactCreateResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-FeatureImpactCreateResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FeatureImpactCreateResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FeatureImpactCreateResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-FeatureImpactCreateResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FeatureImpactCreateResponse$fromJSON(
  FeatureImpactCreateResponseJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{FeatureImpactCreateResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FeatureImpactCreateResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-FeatureImpactCreateResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FeatureImpactCreateResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
