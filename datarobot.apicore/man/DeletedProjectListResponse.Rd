% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deleted_project_list_response.R
\docType{class}
\name{DeletedProjectListResponse}
\alias{DeletedProjectListResponse}
\title{DeletedProjectListResponse}
\format{
An \code{R6Class} generator object
}
\description{
DeletedProjectListResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{count}}{integer \link{optional} Number of items returned on this page.}

\item{\code{data}}{list( \link{DeletedProjectResponse} ) List of deleted projects}

\item{\code{next_}}{character URL pointing to the next page (if null, there is no next page).}

\item{\code{previous}}{character URL pointing to the previous page (if null, there is no previous page).}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-DeletedProjectListResponse-new}{\code{DeletedProjectListResponse$new()}}
\item \href{#method-DeletedProjectListResponse-validate}{\code{DeletedProjectListResponse$validate()}}
\item \href{#method-DeletedProjectListResponse-toJSON}{\code{DeletedProjectListResponse$toJSON()}}
\item \href{#method-DeletedProjectListResponse-fromJSON}{\code{DeletedProjectListResponse$fromJSON()}}
\item \href{#method-DeletedProjectListResponse-clone}{\code{DeletedProjectListResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DeletedProjectListResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-DeletedProjectListResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DeletedProjectListResponse$new(
  data = NULL,
  next_ = NULL,
  previous = NULL,
  count = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{List of deleted projects}

\item{\code{next_}}{URL pointing to the next page (if null, there is no next page).}

\item{\code{previous}}{URL pointing to the previous page (if null, there is no previous page).}

\item{\code{count}}{Number of items returned on this page.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DeletedProjectListResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-DeletedProjectListResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DeletedProjectListResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DeletedProjectListResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-DeletedProjectListResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DeletedProjectListResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DeletedProjectListResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-DeletedProjectListResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DeletedProjectListResponse$fromJSON(
  DeletedProjectListResponseJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{DeletedProjectListResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DeletedProjectListResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-DeletedProjectListResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DeletedProjectListResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
