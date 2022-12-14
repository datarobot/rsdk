% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/target_bin.R
\docType{class}
\name{TargetBin}
\alias{TargetBin}
\title{TargetBin}
\format{
An \code{R6Class} generator object
}
\description{
TargetBin Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{targetBinEnd}}{numeric End value for the target bin}

\item{\code{targetBinStart}}{numeric Start value for the target bin}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-TargetBin-new}{\code{TargetBin$new()}}
\item \href{#method-TargetBin-validate}{\code{TargetBin$validate()}}
\item \href{#method-TargetBin-toJSON}{\code{TargetBin$toJSON()}}
\item \href{#method-TargetBin-fromJSON}{\code{TargetBin$fromJSON()}}
\item \href{#method-TargetBin-clone}{\code{TargetBin$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TargetBin-new"></a>}}
\if{latex}{\out{\hypertarget{method-TargetBin-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TargetBin$new(
  targetBinEnd = NULL,
  targetBinStart = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{targetBinEnd}}{End value for the target bin}

\item{\code{targetBinStart}}{Start value for the target bin}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TargetBin-validate"></a>}}
\if{latex}{\out{\hypertarget{method-TargetBin-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TargetBin$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TargetBin-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-TargetBin-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TargetBin$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TargetBin-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-TargetBin-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TargetBin$fromJSON(TargetBinJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{TargetBinJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TargetBin-clone"></a>}}
\if{latex}{\out{\hypertarget{method-TargetBin-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TargetBin$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
