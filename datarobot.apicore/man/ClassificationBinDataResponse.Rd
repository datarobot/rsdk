% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classification_bin_data_response.R
\docType{class}
\name{ClassificationBinDataResponse}
\alias{ClassificationBinDataResponse}
\title{ClassificationBinDataResponse}
\format{
An \code{R6Class} generator object
}
\description{
ClassificationBinDataResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{binEnd}}{numeric The end of the numeric range for the current bin. Note that `binEnd` - `binStart` should be a constant, modulo floating-point rounding error, for all bins in a single plot.}

\item{\code{binStart}}{numeric The start of the numeric range for the current bin. Must be equal to the `binEnd` of the previous bin.}

\item{\code{negatives}}{integer The number of records in the dataset where the model's predicted value falls into this bin and the target is negative.}

\item{\code{positives}}{integer The number of records in the dataset where the model's predicted value falls into this bin and the target is positive.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ClassificationBinDataResponse-new}{\code{ClassificationBinDataResponse$new()}}
\item \href{#method-ClassificationBinDataResponse-validate}{\code{ClassificationBinDataResponse$validate()}}
\item \href{#method-ClassificationBinDataResponse-toJSON}{\code{ClassificationBinDataResponse$toJSON()}}
\item \href{#method-ClassificationBinDataResponse-fromJSON}{\code{ClassificationBinDataResponse$fromJSON()}}
\item \href{#method-ClassificationBinDataResponse-clone}{\code{ClassificationBinDataResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ClassificationBinDataResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-ClassificationBinDataResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ClassificationBinDataResponse$new(
  binEnd = NULL,
  binStart = NULL,
  negatives = NULL,
  positives = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{binEnd}}{The end of the numeric range for the current bin. Note that `binEnd` - `binStart` should be a constant, modulo floating-point rounding error, for all bins in a single plot.}

\item{\code{binStart}}{The start of the numeric range for the current bin. Must be equal to the `binEnd` of the previous bin.}

\item{\code{negatives}}{The number of records in the dataset where the model's predicted value falls into this bin and the target is negative.}

\item{\code{positives}}{The number of records in the dataset where the model's predicted value falls into this bin and the target is positive.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ClassificationBinDataResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-ClassificationBinDataResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ClassificationBinDataResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ClassificationBinDataResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ClassificationBinDataResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ClassificationBinDataResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ClassificationBinDataResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ClassificationBinDataResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ClassificationBinDataResponse$fromJSON(
  ClassificationBinDataResponseJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ClassificationBinDataResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ClassificationBinDataResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ClassificationBinDataResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ClassificationBinDataResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
