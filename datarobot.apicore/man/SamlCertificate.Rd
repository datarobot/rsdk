% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/saml_certificate.R
\docType{class}
\name{SamlCertificate}
\alias{SamlCertificate}
\title{SamlCertificate}
\format{
An \code{R6Class} generator object
}
\description{
SamlCertificate Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{fileName}}{character \link{optional} Path to certificate file.}

\item{\code{value}}{character Certificate content.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-SamlCertificate-new}{\code{SamlCertificate$new()}}
\item \href{#method-SamlCertificate-validate}{\code{SamlCertificate$validate()}}
\item \href{#method-SamlCertificate-toJSON}{\code{SamlCertificate$toJSON()}}
\item \href{#method-SamlCertificate-fromJSON}{\code{SamlCertificate$fromJSON()}}
\item \href{#method-SamlCertificate-clone}{\code{SamlCertificate$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SamlCertificate-new"></a>}}
\if{latex}{\out{\hypertarget{method-SamlCertificate-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SamlCertificate$new(value = NULL, fileName = NULL, validateParams = FALSE, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{value}}{Certificate content.}

\item{\code{fileName}}{Path to certificate file.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SamlCertificate-validate"></a>}}
\if{latex}{\out{\hypertarget{method-SamlCertificate-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SamlCertificate$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SamlCertificate-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-SamlCertificate-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SamlCertificate$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SamlCertificate-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-SamlCertificate-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SamlCertificate$fromJSON(SamlCertificateJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{SamlCertificateJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SamlCertificate-clone"></a>}}
\if{latex}{\out{\hypertarget{method-SamlCertificate-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SamlCertificate$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
