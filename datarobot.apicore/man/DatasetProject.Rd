% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataset_project.R
\docType{class}
\name{DatasetProject}
\alias{DatasetProject}
\title{DatasetProject}
\format{
An \code{R6Class} generator object
}
\description{
DatasetProject Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{id}}{character The dataset's project ID.}

\item{\code{url}}{character The link to retrieve more information about the dataset version's project.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-DatasetProject-new}{\code{DatasetProject$new()}}
\item \href{#method-DatasetProject-validate}{\code{DatasetProject$validate()}}
\item \href{#method-DatasetProject-toJSON}{\code{DatasetProject$toJSON()}}
\item \href{#method-DatasetProject-fromJSON}{\code{DatasetProject$fromJSON()}}
\item \href{#method-DatasetProject-clone}{\code{DatasetProject$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DatasetProject-new"></a>}}
\if{latex}{\out{\hypertarget{method-DatasetProject-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DatasetProject$new(id = NULL, url = NULL, validateParams = FALSE, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{id}}{The dataset's project ID.}

\item{\code{url}}{The link to retrieve more information about the dataset version's project.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DatasetProject-validate"></a>}}
\if{latex}{\out{\hypertarget{method-DatasetProject-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DatasetProject$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DatasetProject-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-DatasetProject-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DatasetProject$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DatasetProject-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-DatasetProject-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DatasetProject$fromJSON(DatasetProjectJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{DatasetProjectJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DatasetProject-clone"></a>}}
\if{latex}{\out{\hypertarget{method-DatasetProject-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DatasetProject$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
