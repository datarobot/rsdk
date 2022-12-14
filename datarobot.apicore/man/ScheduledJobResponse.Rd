% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scheduled_job_response.R
\docType{class}
\name{ScheduledJobResponse}
\alias{ScheduledJobResponse}
\title{ScheduledJobResponse}
\format{
An \code{R6Class} generator object
}
\description{
ScheduledJobResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{createdBy}}{character \link{optional} User name of the creator}

\item{\code{deploymentId}}{character \link{optional} ID of the deployment this scheduled job is created from.}

\item{\code{enabled}}{character True if the job is enabled and false if the job is disabled.}

\item{\code{id}}{character ID of scheduled prediction job}

\item{\code{integrationTypeId}}{character \link{optional} The specific type of of prediction integration.}

\item{\code{integrationTypeName}}{character \link{optional} filter by integration type name.}

\item{\code{name}}{character \link{optional} Name of the scheduled job.}

\item{\code{schedule}}{\link{Schedule}}

\item{\code{scheduledJobId}}{character ID of this scheduled job.}

\item{\code{status}}{\link{ScheduledJobStatus}}

\item{\code{typeId}}{character Job type of the scheduled job}

\item{\code{updatedAt}}{character \link{optional} Time of last modification}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ScheduledJobResponse-new}{\code{ScheduledJobResponse$new()}}
\item \href{#method-ScheduledJobResponse-validate}{\code{ScheduledJobResponse$validate()}}
\item \href{#method-ScheduledJobResponse-toJSON}{\code{ScheduledJobResponse$toJSON()}}
\item \href{#method-ScheduledJobResponse-fromJSON}{\code{ScheduledJobResponse$fromJSON()}}
\item \href{#method-ScheduledJobResponse-clone}{\code{ScheduledJobResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ScheduledJobResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-ScheduledJobResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ScheduledJobResponse$new(
  enabled = NULL,
  id = NULL,
  schedule = NULL,
  scheduledJobId = NULL,
  status = NULL,
  typeId = NULL,
  createdBy = NULL,
  deploymentId = NULL,
  integrationTypeId = NULL,
  integrationTypeName = NULL,
  name = NULL,
  updatedAt = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{enabled}}{True if the job is enabled and false if the job is disabled.}

\item{\code{id}}{ID of scheduled prediction job}

\item{\code{scheduledJobId}}{ID of this scheduled job.}

\item{\code{typeId}}{Job type of the scheduled job}

\item{\code{createdBy}}{User name of the creator}

\item{\code{deploymentId}}{ID of the deployment this scheduled job is created from.}

\item{\code{integrationTypeId}}{The specific type of of prediction integration.}

\item{\code{integrationTypeName}}{filter by integration type name.}

\item{\code{name}}{Name of the scheduled job.}

\item{\code{updatedAt}}{Time of last modification}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ScheduledJobResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-ScheduledJobResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ScheduledJobResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ScheduledJobResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ScheduledJobResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ScheduledJobResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ScheduledJobResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ScheduledJobResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ScheduledJobResponse$fromJSON(ScheduledJobResponseJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ScheduledJobResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ScheduledJobResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ScheduledJobResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ScheduledJobResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
