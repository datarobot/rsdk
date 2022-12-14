% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prediction_proxy_response.R
\docType{class}
\name{PredictionProxyResponse}
\alias{PredictionProxyResponse}
\title{PredictionProxyResponse}
\format{
An \code{R6Class} generator object
}
\description{
PredictionProxyResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{data}}{list( \link{PredictionProxyData} ) \link{optional} Array contains prediction result objects.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PredictionProxyResponse-new}{\code{PredictionProxyResponse$new()}}
\item \href{#method-PredictionProxyResponse-validate}{\code{PredictionProxyResponse$validate()}}
\item \href{#method-PredictionProxyResponse-toJSON}{\code{PredictionProxyResponse$toJSON()}}
\item \href{#method-PredictionProxyResponse-fromJSON}{\code{PredictionProxyResponse$fromJSON()}}
\item \href{#method-PredictionProxyResponse-clone}{\code{PredictionProxyResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictionProxyResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-PredictionProxyResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictionProxyResponse$new(data = NULL, validateParams = FALSE, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{Array contains prediction result objects.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictionProxyResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-PredictionProxyResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictionProxyResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictionProxyResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-PredictionProxyResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictionProxyResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictionProxyResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-PredictionProxyResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictionProxyResponse$fromJSON(
  PredictionProxyResponseJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{PredictionProxyResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictionProxyResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-PredictionProxyResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictionProxyResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
