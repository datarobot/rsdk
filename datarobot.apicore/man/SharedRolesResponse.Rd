% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shared_roles_response.R
\docType{class}
\name{SharedRolesResponse}
\alias{SharedRolesResponse}
\title{SharedRolesResponse}
\format{
An \code{R6Class} generator object
}
\description{
SharedRolesResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{canShare}}{character True if this user can share with other users}

\item{\code{canUseData}}{character True if the user can view, download and process data (use to create projects, predictions, etc)}

\item{\code{id}}{character The ID of the recipient organization, group or user.}

\item{\code{name}}{character The name of the recipient organization, group or user.}

\item{\code{role}}{character The role of the org/group/user on this dataset or \"NO_ROLE\" for removing access when used with route to modify access.}

\item{\code{shareRecipientType}}{character It describes the recipient type.}

\item{\code{userFullName}}{character \link{optional} If the recipient type is a user, the full name of the user if available.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-SharedRolesResponse-new}{\code{SharedRolesResponse$new()}}
\item \href{#method-SharedRolesResponse-validate}{\code{SharedRolesResponse$validate()}}
\item \href{#method-SharedRolesResponse-toJSON}{\code{SharedRolesResponse$toJSON()}}
\item \href{#method-SharedRolesResponse-fromJSON}{\code{SharedRolesResponse$fromJSON()}}
\item \href{#method-SharedRolesResponse-clone}{\code{SharedRolesResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SharedRolesResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-SharedRolesResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SharedRolesResponse$new(
  canShare = NULL,
  canUseData = NULL,
  id = NULL,
  name = NULL,
  role = NULL,
  shareRecipientType = NULL,
  userFullName = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{canShare}}{True if this user can share with other users}

\item{\code{canUseData}}{True if the user can view, download and process data (use to create projects, predictions, etc)}

\item{\code{id}}{The ID of the recipient organization, group or user.}

\item{\code{name}}{The name of the recipient organization, group or user.}

\item{\code{role}}{The role of the org/group/user on this dataset or \"NO_ROLE\" for removing access when used with route to modify access.}

\item{\code{shareRecipientType}}{It describes the recipient type.}

\item{\code{userFullName}}{If the recipient type is a user, the full name of the user if available.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SharedRolesResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-SharedRolesResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SharedRolesResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SharedRolesResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-SharedRolesResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SharedRolesResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SharedRolesResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-SharedRolesResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SharedRolesResponse$fromJSON(SharedRolesResponseJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{SharedRolesResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SharedRolesResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-SharedRolesResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SharedRolesResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
