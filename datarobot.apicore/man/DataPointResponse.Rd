% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_point_response.R
\docType{class}
\name{DataPointResponse}
\alias{DataPointResponse}
\title{DataPointResponse}
\format{
An \code{R6Class} generator object
}
\description{
DataPointResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{prediction}}{numeric The output of the model for this row.}

\item{\code{shapExplanation}}{list( \link{ShapExplanationResponse} ) Either ``null`` or an array of up to 10 `ShapleyFeatureContribution` objects. Only rows with the highest anomaly scores have Shapley explanations calculated.}

\item{\code{timestamp}}{character ISO-formatted timestamp for the row.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-DataPointResponse-new}{\code{DataPointResponse$new()}}
\item \href{#method-DataPointResponse-validate}{\code{DataPointResponse$validate()}}
\item \href{#method-DataPointResponse-toJSON}{\code{DataPointResponse$toJSON()}}
\item \href{#method-DataPointResponse-fromJSON}{\code{DataPointResponse$fromJSON()}}
\item \href{#method-DataPointResponse-clone}{\code{DataPointResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataPointResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-DataPointResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataPointResponse$new(
  prediction = NULL,
  shapExplanation = NULL,
  timestamp = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{prediction}}{The output of the model for this row.}

\item{\code{shapExplanation}}{Either ``null`` or an array of up to 10 `ShapleyFeatureContribution` objects. Only rows with the highest anomaly scores have Shapley explanations calculated.}

\item{\code{timestamp}}{ISO-formatted timestamp for the row.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataPointResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-DataPointResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataPointResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataPointResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-DataPointResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataPointResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataPointResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-DataPointResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataPointResponse$fromJSON(DataPointResponseJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{DataPointResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DataPointResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-DataPointResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DataPointResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
