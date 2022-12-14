% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/workspace_item_response.R
\docType{class}
\name{WorkspaceItemResponse}
\alias{WorkspaceItemResponse}
\title{WorkspaceItemResponse}
\format{
An \code{R6Class} generator object
}
\description{
WorkspaceItemResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{commitSha}}{character \link{optional} SHA1 hash pointing to the original file revision (set only for files pulled from Git-like repositories).}

\item{\code{created}}{character ISO-8601 timestamp of when the file item was created.}

\item{\code{fileName}}{character Name of the file item.}

\item{\code{filePath}}{character Path of the file item.}

\item{\code{fileSource}}{character Source of the file item.}

\item{\code{id}}{character ID of the file item.}

\item{\code{ref}}{character \link{optional} Remote reference (branch, commit, tag). Branch \"master\", if not specified.}

\item{\code{repositoryFilePath}}{character \link{optional} Full path to the file in the remote repository.}

\item{\code{repositoryLocation}}{character \link{optional} URL to remote repository from which the file was pulled (e.g. Git server or S3 Bucket name).}

\item{\code{repositoryName}}{character \link{optional} Name of the repository from which the file was pulled.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-WorkspaceItemResponse-new}{\code{WorkspaceItemResponse$new()}}
\item \href{#method-WorkspaceItemResponse-validate}{\code{WorkspaceItemResponse$validate()}}
\item \href{#method-WorkspaceItemResponse-toJSON}{\code{WorkspaceItemResponse$toJSON()}}
\item \href{#method-WorkspaceItemResponse-fromJSON}{\code{WorkspaceItemResponse$fromJSON()}}
\item \href{#method-WorkspaceItemResponse-clone}{\code{WorkspaceItemResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-WorkspaceItemResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-WorkspaceItemResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WorkspaceItemResponse$new(
  created = NULL,
  fileName = NULL,
  filePath = NULL,
  fileSource = NULL,
  id = NULL,
  commitSha = NULL,
  ref = NULL,
  repositoryFilePath = NULL,
  repositoryLocation = NULL,
  repositoryName = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{created}}{ISO-8601 timestamp of when the file item was created.}

\item{\code{fileName}}{Name of the file item.}

\item{\code{filePath}}{Path of the file item.}

\item{\code{fileSource}}{Source of the file item.}

\item{\code{id}}{ID of the file item.}

\item{\code{commitSha}}{SHA1 hash pointing to the original file revision (set only for files pulled from Git-like repositories).}

\item{\code{ref}}{Remote reference (branch, commit, tag). Branch \"master\", if not specified.}

\item{\code{repositoryFilePath}}{Full path to the file in the remote repository.}

\item{\code{repositoryLocation}}{URL to remote repository from which the file was pulled (e.g. Git server or S3 Bucket name).}

\item{\code{repositoryName}}{Name of the repository from which the file was pulled.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-WorkspaceItemResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-WorkspaceItemResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WorkspaceItemResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-WorkspaceItemResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-WorkspaceItemResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WorkspaceItemResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-WorkspaceItemResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-WorkspaceItemResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WorkspaceItemResponse$fromJSON(
  WorkspaceItemResponseJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{WorkspaceItemResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-WorkspaceItemResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-WorkspaceItemResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WorkspaceItemResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
