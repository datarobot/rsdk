% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prediction_intervals_create_response.R
\docType{class}
\name{PredictionIntervalsCreateResponse}
\alias{PredictionIntervalsCreateResponse}
\title{PredictionIntervalsCreateResponse}
\format{
An \code{R6Class} generator object
}
\description{
PredictionIntervalsCreateResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{message}}{character Any extended message to include about the result. For example, if a job is submitted that is a duplicate of a job that has already been added to the queue, the message will mention that no new job was created.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PredictionIntervalsCreateResponse-new}{\code{PredictionIntervalsCreateResponse$new()}}
\item \href{#method-PredictionIntervalsCreateResponse-validate}{\code{PredictionIntervalsCreateResponse$validate()}}
\item \href{#method-PredictionIntervalsCreateResponse-toJSON}{\code{PredictionIntervalsCreateResponse$toJSON()}}
\item \href{#method-PredictionIntervalsCreateResponse-fromJSON}{\code{PredictionIntervalsCreateResponse$fromJSON()}}
\item \href{#method-PredictionIntervalsCreateResponse-clone}{\code{PredictionIntervalsCreateResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictionIntervalsCreateResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-PredictionIntervalsCreateResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictionIntervalsCreateResponse$new(
  message = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{message}}{Any extended message to include about the result. For example, if a job is submitted that is a duplicate of a job that has already been added to the queue, the message will mention that no new job was created.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictionIntervalsCreateResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-PredictionIntervalsCreateResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictionIntervalsCreateResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictionIntervalsCreateResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-PredictionIntervalsCreateResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictionIntervalsCreateResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictionIntervalsCreateResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-PredictionIntervalsCreateResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictionIntervalsCreateResponse$fromJSON(
  PredictionIntervalsCreateResponseJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{PredictionIntervalsCreateResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PredictionIntervalsCreateResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-PredictionIntervalsCreateResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PredictionIntervalsCreateResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
