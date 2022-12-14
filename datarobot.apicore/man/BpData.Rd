% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bp_data.R
\docType{class}
\name{BpData}
\alias{BpData}
\title{BpData}
\format{
An \code{R6Class} generator object
}
\description{
BpData Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{children}}{list( object ) \link{optional} A nested dictionary representation of the blueprint DAG.}

\item{\code{id}}{character The type of the node (e.g. \"start\", \"input\", \"task\").}

\item{\code{inputs}}{list( character ) \link{optional} The inputs to the current node.}

\item{\code{output}}{\link{OneOfstringarray} \link{optional} Ids describing the destination of any outgoing edges.}

\item{\code{taskMap}}{object}

\item{\code{taskParameters}}{character A stringified JSON object describing the parameters and their values for a task.}

\item{\code{tasks}}{list( character ) The task defining the current node.}

\item{\code{type}}{character A unique id to represent the current node.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-BpData-new}{\code{BpData$new()}}
\item \href{#method-BpData-validate}{\code{BpData$validate()}}
\item \href{#method-BpData-toJSON}{\code{BpData$toJSON()}}
\item \href{#method-BpData-fromJSON}{\code{BpData$fromJSON()}}
\item \href{#method-BpData-clone}{\code{BpData$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BpData-new"></a>}}
\if{latex}{\out{\hypertarget{method-BpData-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BpData$new(
  id = NULL,
  taskMap = NULL,
  taskParameters = NULL,
  tasks = NULL,
  type = NULL,
  children = NULL,
  inputs = NULL,
  output = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{id}}{The type of the node (e.g. \"start\", \"input\", \"task\").}

\item{\code{taskParameters}}{A stringified JSON object describing the parameters and their values for a task.}

\item{\code{tasks}}{The task defining the current node.}

\item{\code{type}}{A unique id to represent the current node.}

\item{\code{children}}{A nested dictionary representation of the blueprint DAG.}

\item{\code{inputs}}{The inputs to the current node.}

\item{\code{output}}{Ids describing the destination of any outgoing edges.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BpData-validate"></a>}}
\if{latex}{\out{\hypertarget{method-BpData-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BpData$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BpData-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-BpData-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BpData$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BpData-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-BpData-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BpData$fromJSON(BpDataJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{BpDataJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BpData-clone"></a>}}
\if{latex}{\out{\hypertarget{method-BpData-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BpData$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
