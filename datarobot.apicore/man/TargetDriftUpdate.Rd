% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/target_drift_update.R
\docType{class}
\name{TargetDriftUpdate}
\alias{TargetDriftUpdate}
\title{TargetDriftUpdate}
\format{
An \code{R6Class} generator object
}
\description{
TargetDriftUpdate Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{enabled}}{character True if target drift tracking is enabled for this deployment, false if otherwise.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-TargetDriftUpdate-new}{\code{TargetDriftUpdate$new()}}
\item \href{#method-TargetDriftUpdate-validate}{\code{TargetDriftUpdate$validate()}}
\item \href{#method-TargetDriftUpdate-toJSON}{\code{TargetDriftUpdate$toJSON()}}
\item \href{#method-TargetDriftUpdate-fromJSON}{\code{TargetDriftUpdate$fromJSON()}}
\item \href{#method-TargetDriftUpdate-clone}{\code{TargetDriftUpdate$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TargetDriftUpdate-new"></a>}}
\if{latex}{\out{\hypertarget{method-TargetDriftUpdate-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TargetDriftUpdate$new(enabled = NULL, validateParams = FALSE, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{enabled}}{True if target drift tracking is enabled for this deployment, false if otherwise.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TargetDriftUpdate-validate"></a>}}
\if{latex}{\out{\hypertarget{method-TargetDriftUpdate-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TargetDriftUpdate$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TargetDriftUpdate-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-TargetDriftUpdate-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TargetDriftUpdate$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TargetDriftUpdate-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-TargetDriftUpdate-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TargetDriftUpdate$fromJSON(TargetDriftUpdateJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{TargetDriftUpdateJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TargetDriftUpdate-clone"></a>}}
\if{latex}{\out{\hypertarget{method-TargetDriftUpdate-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TargetDriftUpdate$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
