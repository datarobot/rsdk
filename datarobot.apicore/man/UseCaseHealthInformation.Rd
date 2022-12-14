% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/use_case_health_information.R
\docType{class}
\name{UseCaseHealthInformation}
\alias{UseCaseHealthInformation}
\title{UseCaseHealthInformation}
\format{
An \code{R6Class} generator object
}
\description{
UseCaseHealthInformation Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{endDate}}{character The end date for this health status}

\item{\code{message}}{character Information about the health status}

\item{\code{startDate}}{character The start date for this health status}

\item{\code{status}}{character The status of the use case}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-UseCaseHealthInformation-new}{\code{UseCaseHealthInformation$new()}}
\item \href{#method-UseCaseHealthInformation-validate}{\code{UseCaseHealthInformation$validate()}}
\item \href{#method-UseCaseHealthInformation-toJSON}{\code{UseCaseHealthInformation$toJSON()}}
\item \href{#method-UseCaseHealthInformation-fromJSON}{\code{UseCaseHealthInformation$fromJSON()}}
\item \href{#method-UseCaseHealthInformation-clone}{\code{UseCaseHealthInformation$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UseCaseHealthInformation-new"></a>}}
\if{latex}{\out{\hypertarget{method-UseCaseHealthInformation-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UseCaseHealthInformation$new(
  endDate = NULL,
  message = NULL,
  startDate = NULL,
  status = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{endDate}}{The end date for this health status}

\item{\code{message}}{Information about the health status}

\item{\code{startDate}}{The start date for this health status}

\item{\code{status}}{The status of the use case}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UseCaseHealthInformation-validate"></a>}}
\if{latex}{\out{\hypertarget{method-UseCaseHealthInformation-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UseCaseHealthInformation$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UseCaseHealthInformation-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-UseCaseHealthInformation-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UseCaseHealthInformation$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UseCaseHealthInformation-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-UseCaseHealthInformation-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UseCaseHealthInformation$fromJSON(
  UseCaseHealthInformationJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{UseCaseHealthInformationJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UseCaseHealthInformation-clone"></a>}}
\if{latex}{\out{\hypertarget{method-UseCaseHealthInformation-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UseCaseHealthInformation$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
