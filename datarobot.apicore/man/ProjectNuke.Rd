% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/project_nuke.R
\docType{class}
\name{ProjectNuke}
\alias{ProjectNuke}
\title{ProjectNuke}
\format{
An \code{R6Class} generator object
}
\description{
ProjectNuke Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{creator}}{character \link{optional} Creator ID to filter projects by}

\item{\code{deletedAfter}}{character \link{optional} ISO-8601 formatted date projects were deleted after}

\item{\code{deletedBefore}}{character \link{optional} ISO-8601 formatted date projects were deleted before}

\item{\code{limit}}{integer \link{optional} At most this many projects are deleted.}

\item{\code{offset}}{integer \link{optional} This many projects will be skipped.}

\item{\code{organization}}{character \link{optional} ID of organization that projects should belong to}

\item{\code{projectIds}}{list( character ) \link{optional} List of project IDs to delete permanently.}

\item{\code{searchFor}}{character \link{optional} Project or dataset name to filter by}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ProjectNuke-new}{\code{ProjectNuke$new()}}
\item \href{#method-ProjectNuke-validate}{\code{ProjectNuke$validate()}}
\item \href{#method-ProjectNuke-toJSON}{\code{ProjectNuke$toJSON()}}
\item \href{#method-ProjectNuke-fromJSON}{\code{ProjectNuke$fromJSON()}}
\item \href{#method-ProjectNuke-clone}{\code{ProjectNuke$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ProjectNuke-new"></a>}}
\if{latex}{\out{\hypertarget{method-ProjectNuke-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ProjectNuke$new(
  creator = NULL,
  deletedAfter = NULL,
  deletedBefore = NULL,
  limit = 1000,
  offset = 0,
  organization = NULL,
  projectIds = NULL,
  searchFor = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{creator}}{Creator ID to filter projects by}

\item{\code{deletedAfter}}{ISO-8601 formatted date projects were deleted after}

\item{\code{deletedBefore}}{ISO-8601 formatted date projects were deleted before}

\item{\code{limit}}{At most this many projects are deleted.}

\item{\code{offset}}{This many projects will be skipped.}

\item{\code{organization}}{ID of organization that projects should belong to}

\item{\code{projectIds}}{List of project IDs to delete permanently.}

\item{\code{searchFor}}{Project or dataset name to filter by}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ProjectNuke-validate"></a>}}
\if{latex}{\out{\hypertarget{method-ProjectNuke-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ProjectNuke$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ProjectNuke-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ProjectNuke-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ProjectNuke$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ProjectNuke-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ProjectNuke-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ProjectNuke$fromJSON(ProjectNukeJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ProjectNukeJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ProjectNuke-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ProjectNuke-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ProjectNuke$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
