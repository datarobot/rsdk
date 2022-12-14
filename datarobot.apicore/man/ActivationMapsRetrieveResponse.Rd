% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/activation_maps_retrieve_response.R
\docType{class}
\name{ActivationMapsRetrieveResponse}
\alias{ActivationMapsRetrieveResponse}
\title{ActivationMapsRetrieveResponse}
\format{
An \code{R6Class} generator object
}
\description{
ActivationMapsRetrieveResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{activationMapHeight}}{integer The height of each activation map (the number of rows in each activationValues matrix).}

\item{\code{activationMapWidth}}{integer The width of each activation map (the number of items in each row of each activationValues matrix).}

\item{\code{activationMaps}}{list( \link{ActivationMap} ) List of activation map objects}

\item{\code{targetBins}}{list( \link{TargetBin} ) List of bin objects for regression or null}

\item{\code{targetValues}}{list( character ) List of target values for classification or null}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ActivationMapsRetrieveResponse-new}{\code{ActivationMapsRetrieveResponse$new()}}
\item \href{#method-ActivationMapsRetrieveResponse-validate}{\code{ActivationMapsRetrieveResponse$validate()}}
\item \href{#method-ActivationMapsRetrieveResponse-toJSON}{\code{ActivationMapsRetrieveResponse$toJSON()}}
\item \href{#method-ActivationMapsRetrieveResponse-fromJSON}{\code{ActivationMapsRetrieveResponse$fromJSON()}}
\item \href{#method-ActivationMapsRetrieveResponse-clone}{\code{ActivationMapsRetrieveResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ActivationMapsRetrieveResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-ActivationMapsRetrieveResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ActivationMapsRetrieveResponse$new(
  activationMapHeight = NULL,
  activationMapWidth = NULL,
  activationMaps = NULL,
  targetBins = NULL,
  targetValues = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{activationMapHeight}}{The height of each activation map (the number of rows in each activationValues matrix).}

\item{\code{activationMapWidth}}{The width of each activation map (the number of items in each row of each activationValues matrix).}

\item{\code{activationMaps}}{List of activation map objects}

\item{\code{targetBins}}{List of bin objects for regression or null}

\item{\code{targetValues}}{List of target values for classification or null}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ActivationMapsRetrieveResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-ActivationMapsRetrieveResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ActivationMapsRetrieveResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ActivationMapsRetrieveResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ActivationMapsRetrieveResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ActivationMapsRetrieveResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ActivationMapsRetrieveResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ActivationMapsRetrieveResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ActivationMapsRetrieveResponse$fromJSON(
  ActivationMapsRetrieveResponseJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ActivationMapsRetrieveResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ActivationMapsRetrieveResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ActivationMapsRetrieveResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ActivationMapsRetrieveResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
