% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_package_source_meta.R
\docType{class}
\name{ModelPackageSourceMeta}
\alias{ModelPackageSourceMeta}
\title{ModelPackageSourceMeta}
\format{
An \code{R6Class} generator object
}
\description{
ModelPackageSourceMeta Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{decisionFlowId}}{character The ID of the flow associated with this model package}

\item{\code{decisionFlowVersionId}}{character The version ID of the flow associated with this model package}

\item{\code{environmentUrl}}{character If available, URL of the source model}

\item{\code{projectId}}{character If available, the project id used for this model}

\item{\code{projectName}}{character If available, the project name for this model}

\item{\code{scoringCode}}{\link{ModelPacakgeScoringCodeMeta}}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ModelPackageSourceMeta-new}{\code{ModelPackageSourceMeta$new()}}
\item \href{#method-ModelPackageSourceMeta-validate}{\code{ModelPackageSourceMeta$validate()}}
\item \href{#method-ModelPackageSourceMeta-toJSON}{\code{ModelPackageSourceMeta$toJSON()}}
\item \href{#method-ModelPackageSourceMeta-fromJSON}{\code{ModelPackageSourceMeta$fromJSON()}}
\item \href{#method-ModelPackageSourceMeta-clone}{\code{ModelPackageSourceMeta$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelPackageSourceMeta-new"></a>}}
\if{latex}{\out{\hypertarget{method-ModelPackageSourceMeta-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelPackageSourceMeta$new(
  decisionFlowId = NULL,
  decisionFlowVersionId = NULL,
  environmentUrl = NULL,
  projectId = NULL,
  projectName = NULL,
  scoringCode = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{decisionFlowId}}{The ID of the flow associated with this model package}

\item{\code{decisionFlowVersionId}}{The version ID of the flow associated with this model package}

\item{\code{environmentUrl}}{If available, URL of the source model}

\item{\code{projectId}}{If available, the project id used for this model}

\item{\code{projectName}}{If available, the project name for this model}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelPackageSourceMeta-validate"></a>}}
\if{latex}{\out{\hypertarget{method-ModelPackageSourceMeta-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelPackageSourceMeta$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelPackageSourceMeta-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ModelPackageSourceMeta-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelPackageSourceMeta$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelPackageSourceMeta-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ModelPackageSourceMeta-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelPackageSourceMeta$fromJSON(
  ModelPackageSourceMetaJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ModelPackageSourceMetaJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelPackageSourceMeta-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ModelPackageSourceMeta-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelPackageSourceMeta$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
