% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/review_response.R
\docType{class}
\name{ReviewResponse}
\alias{ReviewResponse}
\title{ReviewResponse}
\format{
An \code{R6Class} generator object
}
\description{
ReviewResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{changeRequestId}}{character ID of the Change Request.}

\item{\code{changeVersionId}}{character ID of the change version.}

\item{\code{comment}}{character \link{optional} Free form text to comment on the review.}

\item{\code{createdAt}}{character Timestamp when the review was created.}

\item{\code{id}}{character ID of the review.}

\item{\code{status}}{character Status of the review.}

\item{\code{userId}}{character ID of the user, who created the review.}

\item{\code{userName}}{character Email of the user, who created the review}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ReviewResponse-new}{\code{ReviewResponse$new()}}
\item \href{#method-ReviewResponse-validate}{\code{ReviewResponse$validate()}}
\item \href{#method-ReviewResponse-toJSON}{\code{ReviewResponse$toJSON()}}
\item \href{#method-ReviewResponse-fromJSON}{\code{ReviewResponse$fromJSON()}}
\item \href{#method-ReviewResponse-clone}{\code{ReviewResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ReviewResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-ReviewResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ReviewResponse$new(
  changeRequestId = NULL,
  changeVersionId = NULL,
  createdAt = NULL,
  id = NULL,
  status = NULL,
  userId = NULL,
  userName = NULL,
  comment = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{changeRequestId}}{ID of the Change Request.}

\item{\code{changeVersionId}}{ID of the change version.}

\item{\code{createdAt}}{Timestamp when the review was created.}

\item{\code{id}}{ID of the review.}

\item{\code{status}}{Status of the review.}

\item{\code{userId}}{ID of the user, who created the review.}

\item{\code{userName}}{Email of the user, who created the review}

\item{\code{comment}}{Free form text to comment on the review.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ReviewResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-ReviewResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ReviewResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ReviewResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ReviewResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ReviewResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ReviewResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ReviewResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ReviewResponse$fromJSON(ReviewResponseJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ReviewResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ReviewResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ReviewResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ReviewResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
