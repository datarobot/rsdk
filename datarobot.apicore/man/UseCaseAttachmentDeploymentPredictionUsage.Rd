% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/use_case_attachment_deployment_prediction_usage.R
\docType{class}
\name{UseCaseAttachmentDeploymentPredictionUsage}
\alias{UseCaseAttachmentDeploymentPredictionUsage}
\title{UseCaseAttachmentDeploymentPredictionUsage}
\format{
An \code{R6Class} generator object
}
\description{
UseCaseAttachmentDeploymentPredictionUsage Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{dailyRates}}{list( integer ) List of daily request counts}

\item{\code{lastTimestamp}}{character Last prediction time}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{UseCaseAttachmentDeploymentPredictionUsage$new()}}
\item \href{#method-validate}{\code{UseCaseAttachmentDeploymentPredictionUsage$validate()}}
\item \href{#method-toJSON}{\code{UseCaseAttachmentDeploymentPredictionUsage$toJSON()}}
\item \href{#method-fromJSON}{\code{UseCaseAttachmentDeploymentPredictionUsage$fromJSON()}}
\item \href{#method-clone}{\code{UseCaseAttachmentDeploymentPredictionUsage$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UseCaseAttachmentDeploymentPredictionUsage$new(
  dailyRates = NULL,
  lastTimestamp = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{dailyRates}}{List of daily request counts}

\item{\code{lastTimestamp}}{Last prediction time}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-validate"></a>}}
\if{latex}{\out{\hypertarget{method-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UseCaseAttachmentDeploymentPredictionUsage$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UseCaseAttachmentDeploymentPredictionUsage$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UseCaseAttachmentDeploymentPredictionUsage$fromJSON(
  UseCaseAttachmentDeploymentPredictionUsageJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{UseCaseAttachmentDeploymentPredictionUsageJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UseCaseAttachmentDeploymentPredictionUsage$clone(deep = FALSE)}\if{html}{\out{</div>}}
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