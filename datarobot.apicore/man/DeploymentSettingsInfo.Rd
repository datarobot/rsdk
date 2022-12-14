% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deployment_settings_info.R
\docType{class}
\name{DeploymentSettingsInfo}
\alias{DeploymentSettingsInfo}
\title{DeploymentSettingsInfo}
\format{
An \code{R6Class} generator object
}
\description{
DeploymentSettingsInfo Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{humbleAiEnabled}}{character \link{optional} if humble ai is enabled.}

\item{\code{predictionIntervalsEnabled}}{character \link{optional} If  prediction intervals are enabled.}

\item{\code{predictionWarningEnabled}}{character \link{optional} If prediction warning is enabled.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-DeploymentSettingsInfo-new}{\code{DeploymentSettingsInfo$new()}}
\item \href{#method-DeploymentSettingsInfo-validate}{\code{DeploymentSettingsInfo$validate()}}
\item \href{#method-DeploymentSettingsInfo-toJSON}{\code{DeploymentSettingsInfo$toJSON()}}
\item \href{#method-DeploymentSettingsInfo-fromJSON}{\code{DeploymentSettingsInfo$fromJSON()}}
\item \href{#method-DeploymentSettingsInfo-clone}{\code{DeploymentSettingsInfo$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DeploymentSettingsInfo-new"></a>}}
\if{latex}{\out{\hypertarget{method-DeploymentSettingsInfo-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DeploymentSettingsInfo$new(
  humbleAiEnabled = NULL,
  predictionIntervalsEnabled = NULL,
  predictionWarningEnabled = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{humbleAiEnabled}}{if humble ai is enabled.}

\item{\code{predictionIntervalsEnabled}}{If  prediction intervals are enabled.}

\item{\code{predictionWarningEnabled}}{If prediction warning is enabled.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DeploymentSettingsInfo-validate"></a>}}
\if{latex}{\out{\hypertarget{method-DeploymentSettingsInfo-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DeploymentSettingsInfo$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DeploymentSettingsInfo-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-DeploymentSettingsInfo-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DeploymentSettingsInfo$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DeploymentSettingsInfo-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-DeploymentSettingsInfo-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DeploymentSettingsInfo$fromJSON(
  DeploymentSettingsInfoJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{DeploymentSettingsInfoJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DeploymentSettingsInfo-clone"></a>}}
\if{latex}{\out{\hypertarget{method-DeploymentSettingsInfo-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DeploymentSettingsInfo$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
