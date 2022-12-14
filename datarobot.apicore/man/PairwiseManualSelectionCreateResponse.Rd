% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pairwise_manual_selection_create_response.R
\docType{class}
\name{PairwiseManualSelectionCreateResponse}
\alias{PairwiseManualSelectionCreateResponse}
\title{PairwiseManualSelectionCreateResponse}
\format{
An \code{R6Class} generator object
}
\description{
PairwiseManualSelectionCreateResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{id}}{character ID of the label set.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PairwiseManualSelectionCreateResponse-new}{\code{PairwiseManualSelectionCreateResponse$new()}}
\item \href{#method-PairwiseManualSelectionCreateResponse-validate}{\code{PairwiseManualSelectionCreateResponse$validate()}}
\item \href{#method-PairwiseManualSelectionCreateResponse-toJSON}{\code{PairwiseManualSelectionCreateResponse$toJSON()}}
\item \href{#method-PairwiseManualSelectionCreateResponse-fromJSON}{\code{PairwiseManualSelectionCreateResponse$fromJSON()}}
\item \href{#method-PairwiseManualSelectionCreateResponse-clone}{\code{PairwiseManualSelectionCreateResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PairwiseManualSelectionCreateResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-PairwiseManualSelectionCreateResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PairwiseManualSelectionCreateResponse$new(
  id = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{id}}{ID of the label set.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PairwiseManualSelectionCreateResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-PairwiseManualSelectionCreateResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PairwiseManualSelectionCreateResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PairwiseManualSelectionCreateResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-PairwiseManualSelectionCreateResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PairwiseManualSelectionCreateResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PairwiseManualSelectionCreateResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-PairwiseManualSelectionCreateResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PairwiseManualSelectionCreateResponse$fromJSON(
  PairwiseManualSelectionCreateResponseJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{PairwiseManualSelectionCreateResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PairwiseManualSelectionCreateResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-PairwiseManualSelectionCreateResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PairwiseManualSelectionCreateResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
