% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/use_case_value_template_response.R
\docType{class}
\name{UseCaseValueTemplateResponse}
\alias{UseCaseValueTemplateResponse}
\title{UseCaseValueTemplateResponse}
\format{
An \code{R6Class} generator object
}
\description{
UseCaseValueTemplateResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{description}}{character Description of Value Template}

\item{\code{schema}}{list( \link{UseCaseValueTemplateParameterWithSchema} ) Schema definition of all template parameters}

\item{\code{templateType}}{character The name of the use case value template to be retrieved}

\item{\code{title}}{character Title of Value Template}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-UseCaseValueTemplateResponse-new}{\code{UseCaseValueTemplateResponse$new()}}
\item \href{#method-UseCaseValueTemplateResponse-validate}{\code{UseCaseValueTemplateResponse$validate()}}
\item \href{#method-UseCaseValueTemplateResponse-toJSON}{\code{UseCaseValueTemplateResponse$toJSON()}}
\item \href{#method-UseCaseValueTemplateResponse-fromJSON}{\code{UseCaseValueTemplateResponse$fromJSON()}}
\item \href{#method-UseCaseValueTemplateResponse-clone}{\code{UseCaseValueTemplateResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UseCaseValueTemplateResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-UseCaseValueTemplateResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UseCaseValueTemplateResponse$new(
  description = NULL,
  schema = NULL,
  templateType = NULL,
  title = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{description}}{Description of Value Template}

\item{\code{schema}}{Schema definition of all template parameters}

\item{\code{templateType}}{The name of the use case value template to be retrieved}

\item{\code{title}}{Title of Value Template}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UseCaseValueTemplateResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-UseCaseValueTemplateResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UseCaseValueTemplateResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UseCaseValueTemplateResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-UseCaseValueTemplateResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UseCaseValueTemplateResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UseCaseValueTemplateResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-UseCaseValueTemplateResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UseCaseValueTemplateResponse$fromJSON(
  UseCaseValueTemplateResponseJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{UseCaseValueTemplateResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UseCaseValueTemplateResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-UseCaseValueTemplateResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UseCaseValueTemplateResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
