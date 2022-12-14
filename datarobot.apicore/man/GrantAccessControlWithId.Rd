% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grant_access_control_with_id.R
\docType{class}
\name{GrantAccessControlWithId}
\alias{GrantAccessControlWithId}
\title{GrantAccessControlWithId}
\format{
An \code{R6Class} generator object
}
\description{
GrantAccessControlWithId Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{id}}{character The ID of the recipient.}

\item{\code{role}}{character The role of the recipient on this entity. One of OWNER, USER, OBSERVER.}

\item{\code{shareRecipientType}}{character Describes the recipient type, either user, group, or organization.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-GrantAccessControlWithId-new}{\code{GrantAccessControlWithId$new()}}
\item \href{#method-GrantAccessControlWithId-validate}{\code{GrantAccessControlWithId$validate()}}
\item \href{#method-GrantAccessControlWithId-toJSON}{\code{GrantAccessControlWithId$toJSON()}}
\item \href{#method-GrantAccessControlWithId-fromJSON}{\code{GrantAccessControlWithId$fromJSON()}}
\item \href{#method-GrantAccessControlWithId-clone}{\code{GrantAccessControlWithId$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GrantAccessControlWithId-new"></a>}}
\if{latex}{\out{\hypertarget{method-GrantAccessControlWithId-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GrantAccessControlWithId$new(
  id = NULL,
  role = NULL,
  shareRecipientType = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{id}}{The ID of the recipient.}

\item{\code{role}}{The role of the recipient on this entity. One of OWNER, USER, OBSERVER.}

\item{\code{shareRecipientType}}{Describes the recipient type, either user, group, or organization.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GrantAccessControlWithId-validate"></a>}}
\if{latex}{\out{\hypertarget{method-GrantAccessControlWithId-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GrantAccessControlWithId$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GrantAccessControlWithId-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-GrantAccessControlWithId-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GrantAccessControlWithId$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GrantAccessControlWithId-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-GrantAccessControlWithId-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GrantAccessControlWithId$fromJSON(
  GrantAccessControlWithIdJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{GrantAccessControlWithIdJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GrantAccessControlWithId-clone"></a>}}
\if{latex}{\out{\hypertarget{method-GrantAccessControlWithId-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GrantAccessControlWithId$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
