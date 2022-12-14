% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/project_nuke_job_status_summary.R
\docType{class}
\name{ProjectNukeJobStatusSummary}
\alias{ProjectNukeJobStatusSummary}
\title{ProjectNukeJobStatusSummary}
\format{
An \code{R6Class} generator object
}
\description{
ProjectNukeJobStatusSummary Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{jobId}}{character The ID of the permadeletion multi-job.}

\item{\code{summary}}{\link{ProjectNukeJobStatusSummaryObject}}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ProjectNukeJobStatusSummary-new}{\code{ProjectNukeJobStatusSummary$new()}}
\item \href{#method-ProjectNukeJobStatusSummary-validate}{\code{ProjectNukeJobStatusSummary$validate()}}
\item \href{#method-ProjectNukeJobStatusSummary-toJSON}{\code{ProjectNukeJobStatusSummary$toJSON()}}
\item \href{#method-ProjectNukeJobStatusSummary-fromJSON}{\code{ProjectNukeJobStatusSummary$fromJSON()}}
\item \href{#method-ProjectNukeJobStatusSummary-clone}{\code{ProjectNukeJobStatusSummary$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ProjectNukeJobStatusSummary-new"></a>}}
\if{latex}{\out{\hypertarget{method-ProjectNukeJobStatusSummary-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ProjectNukeJobStatusSummary$new(
  jobId = NULL,
  summary = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{jobId}}{The ID of the permadeletion multi-job.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ProjectNukeJobStatusSummary-validate"></a>}}
\if{latex}{\out{\hypertarget{method-ProjectNukeJobStatusSummary-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ProjectNukeJobStatusSummary$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ProjectNukeJobStatusSummary-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ProjectNukeJobStatusSummary-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ProjectNukeJobStatusSummary$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ProjectNukeJobStatusSummary-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ProjectNukeJobStatusSummary-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ProjectNukeJobStatusSummary$fromJSON(
  ProjectNukeJobStatusSummaryJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ProjectNukeJobStatusSummaryJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ProjectNukeJobStatusSummary-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ProjectNukeJobStatusSummary-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ProjectNukeJobStatusSummary$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
