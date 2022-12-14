% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/feature_drift_update.R
\docType{class}
\name{FeatureDriftUpdate}
\alias{FeatureDriftUpdate}
\title{FeatureDriftUpdate}
\format{
An \code{R6Class} generator object
}
\description{
FeatureDriftUpdate Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{enabled}}{character True if feature drift tracking is enabled for this deployment, false if otherwise.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-FeatureDriftUpdate-new}{\code{FeatureDriftUpdate$new()}}
\item \href{#method-FeatureDriftUpdate-validate}{\code{FeatureDriftUpdate$validate()}}
\item \href{#method-FeatureDriftUpdate-toJSON}{\code{FeatureDriftUpdate$toJSON()}}
\item \href{#method-FeatureDriftUpdate-fromJSON}{\code{FeatureDriftUpdate$fromJSON()}}
\item \href{#method-FeatureDriftUpdate-clone}{\code{FeatureDriftUpdate$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FeatureDriftUpdate-new"></a>}}
\if{latex}{\out{\hypertarget{method-FeatureDriftUpdate-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FeatureDriftUpdate$new(enabled = NULL, validateParams = FALSE, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{enabled}}{True if feature drift tracking is enabled for this deployment, false if otherwise.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FeatureDriftUpdate-validate"></a>}}
\if{latex}{\out{\hypertarget{method-FeatureDriftUpdate-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FeatureDriftUpdate$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FeatureDriftUpdate-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-FeatureDriftUpdate-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FeatureDriftUpdate$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FeatureDriftUpdate-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-FeatureDriftUpdate-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FeatureDriftUpdate$fromJSON(FeatureDriftUpdateJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{FeatureDriftUpdateJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FeatureDriftUpdate-clone"></a>}}
\if{latex}{\out{\hypertarget{method-FeatureDriftUpdate-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FeatureDriftUpdate$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
