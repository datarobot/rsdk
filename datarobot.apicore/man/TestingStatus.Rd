% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/testing_status.R
\docType{class}
\name{TestingStatus}
\alias{TestingStatus}
\title{TestingStatus}
\format{
An \code{R6Class} generator object
}
\description{
TestingStatus Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{message}}{character Test message.}

\item{\code{status}}{character Test status.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-TestingStatus-new}{\code{TestingStatus$new()}}
\item \href{#method-TestingStatus-validate}{\code{TestingStatus$validate()}}
\item \href{#method-TestingStatus-toJSON}{\code{TestingStatus$toJSON()}}
\item \href{#method-TestingStatus-fromJSON}{\code{TestingStatus$fromJSON()}}
\item \href{#method-TestingStatus-clone}{\code{TestingStatus$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TestingStatus-new"></a>}}
\if{latex}{\out{\hypertarget{method-TestingStatus-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TestingStatus$new(message = NULL, status = NULL, validateParams = FALSE, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{message}}{Test message.}

\item{\code{status}}{Test status.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TestingStatus-validate"></a>}}
\if{latex}{\out{\hypertarget{method-TestingStatus-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TestingStatus$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TestingStatus-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-TestingStatus-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TestingStatus$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TestingStatus-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-TestingStatus-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TestingStatus$fromJSON(TestingStatusJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{TestingStatusJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TestingStatus-clone"></a>}}
\if{latex}{\out{\hypertarget{method-TestingStatus-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TestingStatus$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
