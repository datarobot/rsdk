% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/doc_type_specific_info.R
\docType{class}
\name{DocTypeSpecificInfo}
\alias{DocTypeSpecificInfo}
\title{DocTypeSpecificInfo}
\format{
An \code{R6Class} generator object
}
\description{
DocTypeSpecificInfo Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{endDate}}{character End date of selected data in the document.}

\item{\code{modelName}}{character \link{optional} Name of the deployed model.}

\item{\code{startDate}}{character Start date of selected data in the document.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-DocTypeSpecificInfo-new}{\code{DocTypeSpecificInfo$new()}}
\item \href{#method-DocTypeSpecificInfo-validate}{\code{DocTypeSpecificInfo$validate()}}
\item \href{#method-DocTypeSpecificInfo-toJSON}{\code{DocTypeSpecificInfo$toJSON()}}
\item \href{#method-DocTypeSpecificInfo-fromJSON}{\code{DocTypeSpecificInfo$fromJSON()}}
\item \href{#method-DocTypeSpecificInfo-clone}{\code{DocTypeSpecificInfo$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DocTypeSpecificInfo-new"></a>}}
\if{latex}{\out{\hypertarget{method-DocTypeSpecificInfo-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DocTypeSpecificInfo$new(
  endDate = NULL,
  startDate = NULL,
  modelName = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{endDate}}{End date of selected data in the document.}

\item{\code{startDate}}{Start date of selected data in the document.}

\item{\code{modelName}}{Name of the deployed model.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DocTypeSpecificInfo-validate"></a>}}
\if{latex}{\out{\hypertarget{method-DocTypeSpecificInfo-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DocTypeSpecificInfo$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DocTypeSpecificInfo-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-DocTypeSpecificInfo-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DocTypeSpecificInfo$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DocTypeSpecificInfo-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-DocTypeSpecificInfo-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DocTypeSpecificInfo$fromJSON(DocTypeSpecificInfoJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{DocTypeSpecificInfoJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DocTypeSpecificInfo-clone"></a>}}
\if{latex}{\out{\hypertarget{method-DocTypeSpecificInfo-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DocTypeSpecificInfo$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
