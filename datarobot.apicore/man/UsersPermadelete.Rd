% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/users_permadelete.R
\docType{class}
\name{UsersPermadelete}
\alias{UsersPermadelete}
\title{UsersPermadelete}
\format{
An \code{R6Class} generator object
}
\description{
UsersPermadelete Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{orgId}}{character \link{optional} Organization which's users to be permanently deleted.}

\item{\code{orphansOwner}}{character \link{optional} User which becomes an owner of any projects or deployments that otherwise cause an error because they would be orphaned.}

\item{\code{userIds}}{list( character ) \link{optional} Users to be permanently deleted.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-UsersPermadelete-new}{\code{UsersPermadelete$new()}}
\item \href{#method-UsersPermadelete-validate}{\code{UsersPermadelete$validate()}}
\item \href{#method-UsersPermadelete-toJSON}{\code{UsersPermadelete$toJSON()}}
\item \href{#method-UsersPermadelete-fromJSON}{\code{UsersPermadelete$fromJSON()}}
\item \href{#method-UsersPermadelete-clone}{\code{UsersPermadelete$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UsersPermadelete-new"></a>}}
\if{latex}{\out{\hypertarget{method-UsersPermadelete-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UsersPermadelete$new(
  orgId = NULL,
  orphansOwner = NULL,
  userIds = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{orgId}}{Organization which's users to be permanently deleted.}

\item{\code{orphansOwner}}{User which becomes an owner of any projects or deployments that otherwise cause an error because they would be orphaned.}

\item{\code{userIds}}{Users to be permanently deleted.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UsersPermadelete-validate"></a>}}
\if{latex}{\out{\hypertarget{method-UsersPermadelete-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UsersPermadelete$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UsersPermadelete-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-UsersPermadelete-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UsersPermadelete$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UsersPermadelete-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-UsersPermadelete-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UsersPermadelete$fromJSON(UsersPermadeleteJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{UsersPermadeleteJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UsersPermadelete-clone"></a>}}
\if{latex}{\out{\hypertarget{method-UsersPermadelete-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UsersPermadelete$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
