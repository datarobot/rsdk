% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/catalog_extended_details_response.R
\docType{class}
\name{CatalogExtendedDetailsResponse}
\alias{CatalogExtendedDetailsResponse}
\title{CatalogExtendedDetailsResponse}
\format{
An \code{R6Class} generator object
}
\description{
CatalogExtendedDetailsResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{canUseDatasetData}}{character Indicates if the dataset data can be used.}

\item{\code{catalogName}}{character Catalog item name.}

\item{\code{catalogType}}{character Catalog item type.}

\item{\code{dataEngineQueryId}}{character The ID of the catalog item data engine query.}

\item{\code{dataMeshWorkspaceId}}{character (Deprecated) The ID by which workspace is referenced in Data Mesh API.}

\item{\code{dataSourceId}}{character The ID of the catalog item data source.}

\item{\code{description}}{character Catalog item description.}

\item{\code{error}}{character The latest error of the catalog item.}

\item{\code{id}}{character Catalog item ID.}

\item{\code{infoCreationDate}}{character The creation date of the catalog item.}

\item{\code{infoCreatorFullName}}{character The creator of the catalog item.}

\item{\code{infoModificationDate}}{character The date when the dataset metadata was last modified. This field is only applicable if the catalog item is a dataset.}

\item{\code{infoModifierFullName}}{character The user that last modified the dataset metadata. This field is only applicable if the catalog item is a dataset.}

\item{\code{isDataEngineEligible}}{character Indicates if the catalog item is eligible for use by the data engine.}

\item{\code{isFirstVersion}}{character \link{optional} Indicates if the catalog item is the first version.}

\item{\code{lastModificationDate}}{character The date when the catalog item was last modified.}

\item{\code{lastModifierFullName}}{character The user that last modified the catalog item.}

\item{\code{originalName}}{character Catalog item original name.}

\item{\code{pipelinesWorkspaceId}}{character The ID by which workspace is referenced in Data Mesh API.}

\item{\code{processingState}}{integer The latest processing state of the catalog item.}

\item{\code{projectsUsedInCount}}{integer The number of projects that use the catalog item.}

\item{\code{relevance}}{numeric ElasticSearch score value or null if search done in Mongo.}

\item{\code{tags}}{list( character ) List of catalog item tags in the lower case with no spaces.}

\item{\code{uri}}{character The URI to the datasource from which the catalog item was created, if it is a dataset.}

\item{\code{userBlueprintId}}{character The ID by which a user blueprint is referenced in User Blueprint API.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-CatalogExtendedDetailsResponse-new}{\code{CatalogExtendedDetailsResponse$new()}}
\item \href{#method-CatalogExtendedDetailsResponse-validate}{\code{CatalogExtendedDetailsResponse$validate()}}
\item \href{#method-CatalogExtendedDetailsResponse-toJSON}{\code{CatalogExtendedDetailsResponse$toJSON()}}
\item \href{#method-CatalogExtendedDetailsResponse-fromJSON}{\code{CatalogExtendedDetailsResponse$fromJSON()}}
\item \href{#method-CatalogExtendedDetailsResponse-clone}{\code{CatalogExtendedDetailsResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CatalogExtendedDetailsResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-CatalogExtendedDetailsResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CatalogExtendedDetailsResponse$new(
  canUseDatasetData = NULL,
  catalogName = NULL,
  catalogType = NULL,
  dataEngineQueryId = NULL,
  dataMeshWorkspaceId = NULL,
  dataSourceId = NULL,
  description = NULL,
  error = NULL,
  id = NULL,
  infoCreationDate = NULL,
  infoCreatorFullName = NULL,
  infoModificationDate = NULL,
  infoModifierFullName = NULL,
  isDataEngineEligible = NULL,
  lastModificationDate = NULL,
  lastModifierFullName = NULL,
  originalName = NULL,
  pipelinesWorkspaceId = NULL,
  processingState = NULL,
  projectsUsedInCount = NULL,
  relevance = NULL,
  tags = NULL,
  uri = NULL,
  userBlueprintId = NULL,
  isFirstVersion = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{canUseDatasetData}}{Indicates if the dataset data can be used.}

\item{\code{catalogName}}{Catalog item name.}

\item{\code{catalogType}}{Catalog item type.}

\item{\code{dataEngineQueryId}}{The ID of the catalog item data engine query.}

\item{\code{dataMeshWorkspaceId}}{(Deprecated) The ID by which workspace is referenced in Data Mesh API.}

\item{\code{dataSourceId}}{The ID of the catalog item data source.}

\item{\code{description}}{Catalog item description.}

\item{\code{error}}{The latest error of the catalog item.}

\item{\code{id}}{Catalog item ID.}

\item{\code{infoCreationDate}}{The creation date of the catalog item.}

\item{\code{infoCreatorFullName}}{The creator of the catalog item.}

\item{\code{infoModificationDate}}{The date when the dataset metadata was last modified. This field is only applicable if the catalog item is a dataset.}

\item{\code{infoModifierFullName}}{The user that last modified the dataset metadata. This field is only applicable if the catalog item is a dataset.}

\item{\code{isDataEngineEligible}}{Indicates if the catalog item is eligible for use by the data engine.}

\item{\code{lastModificationDate}}{The date when the catalog item was last modified.}

\item{\code{lastModifierFullName}}{The user that last modified the catalog item.}

\item{\code{originalName}}{Catalog item original name.}

\item{\code{pipelinesWorkspaceId}}{The ID by which workspace is referenced in Data Mesh API.}

\item{\code{processingState}}{The latest processing state of the catalog item.}

\item{\code{projectsUsedInCount}}{The number of projects that use the catalog item.}

\item{\code{relevance}}{ElasticSearch score value or null if search done in Mongo.}

\item{\code{tags}}{List of catalog item tags in the lower case with no spaces.}

\item{\code{uri}}{The URI to the datasource from which the catalog item was created, if it is a dataset.}

\item{\code{userBlueprintId}}{The ID by which a user blueprint is referenced in User Blueprint API.}

\item{\code{isFirstVersion}}{Indicates if the catalog item is the first version.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CatalogExtendedDetailsResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-CatalogExtendedDetailsResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CatalogExtendedDetailsResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CatalogExtendedDetailsResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-CatalogExtendedDetailsResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CatalogExtendedDetailsResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CatalogExtendedDetailsResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-CatalogExtendedDetailsResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CatalogExtendedDetailsResponse$fromJSON(
  CatalogExtendedDetailsResponseJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{CatalogExtendedDetailsResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CatalogExtendedDetailsResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-CatalogExtendedDetailsResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CatalogExtendedDetailsResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
