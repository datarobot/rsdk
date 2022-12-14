% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/secondary_dataset_config_response.R
\docType{class}
\name{SecondaryDatasetConfigResponse}
\alias{SecondaryDatasetConfigResponse}
\title{SecondaryDatasetConfigResponse}
\format{
An \code{R6Class} generator object
}
\description{
SecondaryDatasetConfigResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{created}}{character DR-formatted datetime, null for legacy (before DR 6.0) db records}

\item{\code{creatorFullName}}{character Fullname or email of the user created this config. null for legacy (before DR 6.0) db records}

\item{\code{creatorUserId}}{character ID of the user created this config, null for legacy (before DR 6.0) db records}

\item{\code{credentialIds}}{list( \link{DatasetsCredential} ) \link{optional} List of credentials used by the secondary datasets if the datasets used in the configuration are from datasource}

\item{\code{featurelistId}}{character Id of the feature list}

\item{\code{isDefault}}{character Secondary datasets config is default config or not}

\item{\code{isDeleted}}{character Secondary datasets config is soft deleted or not}

\item{\code{name}}{character Name of the secondary datasets config}

\item{\code{projectId}}{character ID of the project}

\item{\code{secondaryDatasetConfigId}}{character ID of the secondary datasets configuration}

\item{\code{secondaryDatasets}}{list( \link{DeploymentSecondaryDataset} ) \link{optional} List of secondary datasets used in the config}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-SecondaryDatasetConfigResponse-new}{\code{SecondaryDatasetConfigResponse$new()}}
\item \href{#method-SecondaryDatasetConfigResponse-validate}{\code{SecondaryDatasetConfigResponse$validate()}}
\item \href{#method-SecondaryDatasetConfigResponse-toJSON}{\code{SecondaryDatasetConfigResponse$toJSON()}}
\item \href{#method-SecondaryDatasetConfigResponse-fromJSON}{\code{SecondaryDatasetConfigResponse$fromJSON()}}
\item \href{#method-SecondaryDatasetConfigResponse-clone}{\code{SecondaryDatasetConfigResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SecondaryDatasetConfigResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-SecondaryDatasetConfigResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SecondaryDatasetConfigResponse$new(
  created = NULL,
  creatorFullName = NULL,
  creatorUserId = NULL,
  featurelistId = NULL,
  isDefault = NULL,
  isDeleted = NULL,
  name = NULL,
  projectId = NULL,
  secondaryDatasetConfigId = NULL,
  credentialIds = NULL,
  secondaryDatasets = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{created}}{DR-formatted datetime, null for legacy (before DR 6.0) db records}

\item{\code{creatorFullName}}{Fullname or email of the user created this config. null for legacy (before DR 6.0) db records}

\item{\code{creatorUserId}}{ID of the user created this config, null for legacy (before DR 6.0) db records}

\item{\code{featurelistId}}{Id of the feature list}

\item{\code{isDefault}}{Secondary datasets config is default config or not}

\item{\code{isDeleted}}{Secondary datasets config is soft deleted or not}

\item{\code{name}}{Name of the secondary datasets config}

\item{\code{projectId}}{ID of the project}

\item{\code{secondaryDatasetConfigId}}{ID of the secondary datasets configuration}

\item{\code{credentialIds}}{List of credentials used by the secondary datasets if the datasets used in the configuration are from datasource}

\item{\code{secondaryDatasets}}{List of secondary datasets used in the config}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SecondaryDatasetConfigResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-SecondaryDatasetConfigResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SecondaryDatasetConfigResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SecondaryDatasetConfigResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-SecondaryDatasetConfigResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SecondaryDatasetConfigResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SecondaryDatasetConfigResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-SecondaryDatasetConfigResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SecondaryDatasetConfigResponse$fromJSON(
  SecondaryDatasetConfigResponseJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{SecondaryDatasetConfigResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SecondaryDatasetConfigResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-SecondaryDatasetConfigResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SecondaryDatasetConfigResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
