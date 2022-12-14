% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/feature_discovery_setting_response.R
\docType{class}
\name{FeatureDiscoverySettingResponse}
\alias{FeatureDiscoverySettingResponse}
\title{FeatureDiscoverySettingResponse}
\format{
An \code{R6Class} generator object
}
\description{
FeatureDiscoverySettingResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{description}}{character Description of this feature discovery setting}

\item{\code{family}}{character Family of this feature discovery setting}

\item{\code{name}}{character Name of this feature discovery setting}

\item{\code{settingType}}{character Type of this feature discovery setting}

\item{\code{value}}{character Value of this feature discovery setting}

\item{\code{verboseName}}{character Human readable name of this feature discovery setting}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-FeatureDiscoverySettingResponse-new}{\code{FeatureDiscoverySettingResponse$new()}}
\item \href{#method-FeatureDiscoverySettingResponse-validate}{\code{FeatureDiscoverySettingResponse$validate()}}
\item \href{#method-FeatureDiscoverySettingResponse-toJSON}{\code{FeatureDiscoverySettingResponse$toJSON()}}
\item \href{#method-FeatureDiscoverySettingResponse-fromJSON}{\code{FeatureDiscoverySettingResponse$fromJSON()}}
\item \href{#method-FeatureDiscoverySettingResponse-clone}{\code{FeatureDiscoverySettingResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FeatureDiscoverySettingResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-FeatureDiscoverySettingResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FeatureDiscoverySettingResponse$new(
  description = NULL,
  family = NULL,
  name = NULL,
  settingType = NULL,
  value = NULL,
  verboseName = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{description}}{Description of this feature discovery setting}

\item{\code{family}}{Family of this feature discovery setting}

\item{\code{name}}{Name of this feature discovery setting}

\item{\code{settingType}}{Type of this feature discovery setting}

\item{\code{value}}{Value of this feature discovery setting}

\item{\code{verboseName}}{Human readable name of this feature discovery setting}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FeatureDiscoverySettingResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-FeatureDiscoverySettingResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FeatureDiscoverySettingResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FeatureDiscoverySettingResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-FeatureDiscoverySettingResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FeatureDiscoverySettingResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FeatureDiscoverySettingResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-FeatureDiscoverySettingResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FeatureDiscoverySettingResponse$fromJSON(
  FeatureDiscoverySettingResponseJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{FeatureDiscoverySettingResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FeatureDiscoverySettingResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-FeatureDiscoverySettingResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FeatureDiscoverySettingResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
