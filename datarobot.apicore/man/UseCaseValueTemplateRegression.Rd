% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/use_case_value_template_regression.R
\docType{class}
\name{UseCaseValueTemplateRegression}
\alias{UseCaseValueTemplateRegression}
\title{UseCaseValueTemplateRegression}
\format{
An \code{R6Class} generator object
}
\description{
UseCaseValueTemplateRegression Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{data}}{\link{UseCaseValueTemplateRegressionData}}

\item{\code{templateType}}{character Use case value template type}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-UseCaseValueTemplateRegression-new}{\code{UseCaseValueTemplateRegression$new()}}
\item \href{#method-UseCaseValueTemplateRegression-validate}{\code{UseCaseValueTemplateRegression$validate()}}
\item \href{#method-UseCaseValueTemplateRegression-toJSON}{\code{UseCaseValueTemplateRegression$toJSON()}}
\item \href{#method-UseCaseValueTemplateRegression-fromJSON}{\code{UseCaseValueTemplateRegression$fromJSON()}}
\item \href{#method-UseCaseValueTemplateRegression-clone}{\code{UseCaseValueTemplateRegression$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UseCaseValueTemplateRegression-new"></a>}}
\if{latex}{\out{\hypertarget{method-UseCaseValueTemplateRegression-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UseCaseValueTemplateRegression$new(
  data = NULL,
  templateType = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{templateType}}{Use case value template type}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UseCaseValueTemplateRegression-validate"></a>}}
\if{latex}{\out{\hypertarget{method-UseCaseValueTemplateRegression-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UseCaseValueTemplateRegression$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UseCaseValueTemplateRegression-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-UseCaseValueTemplateRegression-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UseCaseValueTemplateRegression$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UseCaseValueTemplateRegression-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-UseCaseValueTemplateRegression-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UseCaseValueTemplateRegression$fromJSON(
  UseCaseValueTemplateRegressionJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{UseCaseValueTemplateRegressionJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UseCaseValueTemplateRegression-clone"></a>}}
\if{latex}{\out{\hypertarget{method-UseCaseValueTemplateRegression-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UseCaseValueTemplateRegression$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
