% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bulk_catalog_share_payload.R
\docType{class}
\name{BulkCatalogSharePayload}
\alias{BulkCatalogSharePayload}
\title{BulkCatalogSharePayload}
\format{
An \code{R6Class} generator object
}
\description{
BulkCatalogSharePayload Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{action}}{character The action to execute on the datasets. Has to be 'updateRoles' for this payload.}

\item{\code{applyGrantToLinkedObjects}}{character \link{optional} If true for any users being granted access to the dataset, grant the user read access to any linked objects such as DataSources and DataStores that may be used by this dataset. Ignored if no such objects are relevant for dataset. Will not result in access being lowered for a user if the user already has higher access to linked objects than read access. However, if the target user does not have sharing permissions to the linked object, they will be given sharing access without lowering existing permissions. May result in an error if user making call does not have sufficient permissions to complete grant. Default value is false.}

\item{\code{roles}}{list( \link{OneOfDatasetRolesWithNamesDatasetRolesWithId} ) An array of RoleRequest objects. May contain at most 100 such objects.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-BulkCatalogSharePayload-new}{\code{BulkCatalogSharePayload$new()}}
\item \href{#method-BulkCatalogSharePayload-validate}{\code{BulkCatalogSharePayload$validate()}}
\item \href{#method-BulkCatalogSharePayload-toJSON}{\code{BulkCatalogSharePayload$toJSON()}}
\item \href{#method-BulkCatalogSharePayload-fromJSON}{\code{BulkCatalogSharePayload$fromJSON()}}
\item \href{#method-BulkCatalogSharePayload-clone}{\code{BulkCatalogSharePayload$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BulkCatalogSharePayload-new"></a>}}
\if{latex}{\out{\hypertarget{method-BulkCatalogSharePayload-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BulkCatalogSharePayload$new(
  action = NULL,
  roles = NULL,
  applyGrantToLinkedObjects = FALSE,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{action}}{The action to execute on the datasets. Has to be 'updateRoles' for this payload.}

\item{\code{roles}}{An array of RoleRequest objects. May contain at most 100 such objects.}

\item{\code{applyGrantToLinkedObjects}}{If true for any users being granted access to the dataset, grant the user read access to any linked objects such as DataSources and DataStores that may be used by this dataset. Ignored if no such objects are relevant for dataset. Will not result in access being lowered for a user if the user already has higher access to linked objects than read access. However, if the target user does not have sharing permissions to the linked object, they will be given sharing access without lowering existing permissions. May result in an error if user making call does not have sufficient permissions to complete grant. Default value is false.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BulkCatalogSharePayload-validate"></a>}}
\if{latex}{\out{\hypertarget{method-BulkCatalogSharePayload-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BulkCatalogSharePayload$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BulkCatalogSharePayload-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-BulkCatalogSharePayload-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BulkCatalogSharePayload$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BulkCatalogSharePayload-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-BulkCatalogSharePayload-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BulkCatalogSharePayload$fromJSON(
  BulkCatalogSharePayloadJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{BulkCatalogSharePayloadJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BulkCatalogSharePayload-clone"></a>}}
\if{latex}{\out{\hypertarget{method-BulkCatalogSharePayload-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BulkCatalogSharePayload$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
