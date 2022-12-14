% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/user_notification_related_users_delete_response.R
\docType{class}
\name{UserNotificationRelatedUsersDeleteResponse}
\alias{UserNotificationRelatedUsersDeleteResponse}
\title{UserNotificationRelatedUsersDeleteResponse}
\format{
An \code{R6Class} generator object
}
\description{
UserNotificationRelatedUsersDeleteResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{reportId}}{character The ID of the users permanent delete report}

\item{\code{statusId}}{character The ID of the users delete status.}

\item{\code{usersToDeleteCount}}{character Number of users that will be deleted.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-UserNotificationRelatedUsersDeleteResponse-new}{\code{UserNotificationRelatedUsersDeleteResponse$new()}}
\item \href{#method-UserNotificationRelatedUsersDeleteResponse-validate}{\code{UserNotificationRelatedUsersDeleteResponse$validate()}}
\item \href{#method-UserNotificationRelatedUsersDeleteResponse-toJSON}{\code{UserNotificationRelatedUsersDeleteResponse$toJSON()}}
\item \href{#method-UserNotificationRelatedUsersDeleteResponse-fromJSON}{\code{UserNotificationRelatedUsersDeleteResponse$fromJSON()}}
\item \href{#method-UserNotificationRelatedUsersDeleteResponse-clone}{\code{UserNotificationRelatedUsersDeleteResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UserNotificationRelatedUsersDeleteResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-UserNotificationRelatedUsersDeleteResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UserNotificationRelatedUsersDeleteResponse$new(
  reportId = NULL,
  statusId = NULL,
  usersToDeleteCount = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{reportId}}{The ID of the users permanent delete report}

\item{\code{statusId}}{The ID of the users delete status.}

\item{\code{usersToDeleteCount}}{Number of users that will be deleted.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UserNotificationRelatedUsersDeleteResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-UserNotificationRelatedUsersDeleteResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UserNotificationRelatedUsersDeleteResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UserNotificationRelatedUsersDeleteResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-UserNotificationRelatedUsersDeleteResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UserNotificationRelatedUsersDeleteResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UserNotificationRelatedUsersDeleteResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-UserNotificationRelatedUsersDeleteResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UserNotificationRelatedUsersDeleteResponse$fromJSON(
  UserNotificationRelatedUsersDeleteResponseJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{UserNotificationRelatedUsersDeleteResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UserNotificationRelatedUsersDeleteResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-UserNotificationRelatedUsersDeleteResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UserNotificationRelatedUsersDeleteResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
