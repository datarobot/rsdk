% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/users_permadelete_preview_job_status_response.R
\docType{class}
\name{UsersPermadeletePreviewJobStatusResponse}
\alias{UsersPermadeletePreviewJobStatusResponse}
\title{UsersPermadeletePreviewJobStatusResponse}
\format{
An \code{R6Class} generator object
}
\description{
UsersPermadeletePreviewJobStatusResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{created}}{character The time the status record was created.}

\item{\code{data}}{\link{PreviewPermadeleteStatus}}

\item{\code{message}}{character May contain further information about the status.}

\item{\code{status}}{character The processing state of the job.}

\item{\code{statusId}}{character The ID of the status object.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-UsersPermadeletePreviewJobStatusResponse-new}{\code{UsersPermadeletePreviewJobStatusResponse$new()}}
\item \href{#method-UsersPermadeletePreviewJobStatusResponse-validate}{\code{UsersPermadeletePreviewJobStatusResponse$validate()}}
\item \href{#method-UsersPermadeletePreviewJobStatusResponse-toJSON}{\code{UsersPermadeletePreviewJobStatusResponse$toJSON()}}
\item \href{#method-UsersPermadeletePreviewJobStatusResponse-fromJSON}{\code{UsersPermadeletePreviewJobStatusResponse$fromJSON()}}
\item \href{#method-UsersPermadeletePreviewJobStatusResponse-clone}{\code{UsersPermadeletePreviewJobStatusResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UsersPermadeletePreviewJobStatusResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-UsersPermadeletePreviewJobStatusResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UsersPermadeletePreviewJobStatusResponse$new(
  created = NULL,
  data = NULL,
  message = NULL,
  status = NULL,
  statusId = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{created}}{The time the status record was created.}

\item{\code{message}}{May contain further information about the status.}

\item{\code{status}}{The processing state of the job.}

\item{\code{statusId}}{The ID of the status object.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UsersPermadeletePreviewJobStatusResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-UsersPermadeletePreviewJobStatusResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UsersPermadeletePreviewJobStatusResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UsersPermadeletePreviewJobStatusResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-UsersPermadeletePreviewJobStatusResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UsersPermadeletePreviewJobStatusResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UsersPermadeletePreviewJobStatusResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-UsersPermadeletePreviewJobStatusResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UsersPermadeletePreviewJobStatusResponse$fromJSON(
  UsersPermadeletePreviewJobStatusResponseJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{UsersPermadeletePreviewJobStatusResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UsersPermadeletePreviewJobStatusResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-UsersPermadeletePreviewJobStatusResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UsersPermadeletePreviewJobStatusResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
