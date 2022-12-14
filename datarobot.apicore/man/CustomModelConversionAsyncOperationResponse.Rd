% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/custom_model_conversion_async_operation_response.R
\docType{class}
\name{CustomModelConversionAsyncOperationResponse}
\alias{CustomModelConversionAsyncOperationResponse}
\title{CustomModelConversionAsyncOperationResponse}
\format{
An \code{R6Class} generator object
}
\description{
CustomModelConversionAsyncOperationResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{conversionId}}{character ID that can be used to stop a given conversion.}

\item{\code{statusId}}{character ID that can be used with :http:get:`/api/v2/status/(statusId)/` to poll for the testing job's status}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-CustomModelConversionAsyncOperationResponse-new}{\code{CustomModelConversionAsyncOperationResponse$new()}}
\item \href{#method-CustomModelConversionAsyncOperationResponse-validate}{\code{CustomModelConversionAsyncOperationResponse$validate()}}
\item \href{#method-CustomModelConversionAsyncOperationResponse-toJSON}{\code{CustomModelConversionAsyncOperationResponse$toJSON()}}
\item \href{#method-CustomModelConversionAsyncOperationResponse-fromJSON}{\code{CustomModelConversionAsyncOperationResponse$fromJSON()}}
\item \href{#method-CustomModelConversionAsyncOperationResponse-clone}{\code{CustomModelConversionAsyncOperationResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CustomModelConversionAsyncOperationResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-CustomModelConversionAsyncOperationResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CustomModelConversionAsyncOperationResponse$new(
  conversionId = NULL,
  statusId = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{conversionId}}{ID that can be used to stop a given conversion.}

\item{\code{statusId}}{ID that can be used with :http:get:`/api/v2/status/(statusId)/` to poll for the testing job's status}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CustomModelConversionAsyncOperationResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-CustomModelConversionAsyncOperationResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CustomModelConversionAsyncOperationResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CustomModelConversionAsyncOperationResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-CustomModelConversionAsyncOperationResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CustomModelConversionAsyncOperationResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CustomModelConversionAsyncOperationResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-CustomModelConversionAsyncOperationResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CustomModelConversionAsyncOperationResponse$fromJSON(
  CustomModelConversionAsyncOperationResponseJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{CustomModelConversionAsyncOperationResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CustomModelConversionAsyncOperationResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-CustomModelConversionAsyncOperationResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CustomModelConversionAsyncOperationResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
