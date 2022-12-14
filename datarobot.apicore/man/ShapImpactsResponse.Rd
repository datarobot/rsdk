% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shap_impacts_response.R
\docType{class}
\name{ShapImpactsResponse}
\alias{ShapImpactsResponse}
\title{ShapImpactsResponse}
\format{
An \code{R6Class} generator object
}
\description{
ShapImpactsResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{featureName}}{character The feature name in dataset}

\item{\code{impactNormalized}}{numeric The normalized impact score value (largest value is 1)}

\item{\code{impactUnnormalized}}{numeric The raw impact score value}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ShapImpactsResponse-new}{\code{ShapImpactsResponse$new()}}
\item \href{#method-ShapImpactsResponse-validate}{\code{ShapImpactsResponse$validate()}}
\item \href{#method-ShapImpactsResponse-toJSON}{\code{ShapImpactsResponse$toJSON()}}
\item \href{#method-ShapImpactsResponse-fromJSON}{\code{ShapImpactsResponse$fromJSON()}}
\item \href{#method-ShapImpactsResponse-clone}{\code{ShapImpactsResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShapImpactsResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-ShapImpactsResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ShapImpactsResponse$new(
  featureName = NULL,
  impactNormalized = NULL,
  impactUnnormalized = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{featureName}}{The feature name in dataset}

\item{\code{impactNormalized}}{The normalized impact score value (largest value is 1)}

\item{\code{impactUnnormalized}}{The raw impact score value}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShapImpactsResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-ShapImpactsResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ShapImpactsResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShapImpactsResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ShapImpactsResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ShapImpactsResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShapImpactsResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ShapImpactsResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ShapImpactsResponse$fromJSON(ShapImpactsResponseJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ShapImpactsResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ShapImpactsResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ShapImpactsResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ShapImpactsResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
