% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cluster_names_response.R
\docType{class}
\name{ClusterNamesResponse}
\alias{ClusterNamesResponse}
\title{ClusterNamesResponse}
\format{
An \code{R6Class} generator object
}
\description{
ClusterNamesResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{clusters}}{list( \link{ClusterInfoList} ) A list of the model's cluster information entries.}

\item{\code{modelId}}{character The model ID}

\item{\code{projectId}}{character The project ID}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ClusterNamesResponse-new}{\code{ClusterNamesResponse$new()}}
\item \href{#method-ClusterNamesResponse-validate}{\code{ClusterNamesResponse$validate()}}
\item \href{#method-ClusterNamesResponse-toJSON}{\code{ClusterNamesResponse$toJSON()}}
\item \href{#method-ClusterNamesResponse-fromJSON}{\code{ClusterNamesResponse$fromJSON()}}
\item \href{#method-ClusterNamesResponse-clone}{\code{ClusterNamesResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ClusterNamesResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-ClusterNamesResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ClusterNamesResponse$new(
  clusters = NULL,
  modelId = NULL,
  projectId = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{clusters}}{A list of the model's cluster information entries.}

\item{\code{modelId}}{The model ID}

\item{\code{projectId}}{The project ID}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ClusterNamesResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-ClusterNamesResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ClusterNamesResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ClusterNamesResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ClusterNamesResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ClusterNamesResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ClusterNamesResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ClusterNamesResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ClusterNamesResponse$fromJSON(ClusterNamesResponseJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ClusterNamesResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ClusterNamesResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ClusterNamesResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ClusterNamesResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
