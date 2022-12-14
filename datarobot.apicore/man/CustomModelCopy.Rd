% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/custom_model_copy.R
\docType{class}
\name{CustomModelCopy}
\alias{CustomModelCopy}
\title{CustomModelCopy}
\format{
An \code{R6Class} generator object
}
\description{
CustomModelCopy Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{customModelId}}{character ID of the custom model to copy.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-CustomModelCopy-new}{\code{CustomModelCopy$new()}}
\item \href{#method-CustomModelCopy-validate}{\code{CustomModelCopy$validate()}}
\item \href{#method-CustomModelCopy-toJSON}{\code{CustomModelCopy$toJSON()}}
\item \href{#method-CustomModelCopy-fromJSON}{\code{CustomModelCopy$fromJSON()}}
\item \href{#method-CustomModelCopy-clone}{\code{CustomModelCopy$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CustomModelCopy-new"></a>}}
\if{latex}{\out{\hypertarget{method-CustomModelCopy-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CustomModelCopy$new(customModelId = NULL, validateParams = FALSE, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{customModelId}}{ID of the custom model to copy.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CustomModelCopy-validate"></a>}}
\if{latex}{\out{\hypertarget{method-CustomModelCopy-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CustomModelCopy$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CustomModelCopy-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-CustomModelCopy-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CustomModelCopy$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CustomModelCopy-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-CustomModelCopy-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CustomModelCopy$fromJSON(CustomModelCopyJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{CustomModelCopyJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CustomModelCopy-clone"></a>}}
\if{latex}{\out{\hypertarget{method-CustomModelCopy-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CustomModelCopy$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
