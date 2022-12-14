% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_sso_configuration.R
\docType{class}
\name{UpdateSsoConfiguration}
\alias{UpdateSsoConfiguration}
\title{UpdateSsoConfiguration}
\format{
An \code{R6Class} generator object
}
\description{
UpdateSsoConfiguration Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{advancedConfiguration}}{\link{SamlAdvancedConfiguration} \link{optional}}

\item{\code{attributeMapping}}{\link{SamlAttributeMapping} \link{optional}}

\item{\code{autoGenerateUsers}}{character \link{optional} determines if DataRobot automatically creates an account on first successful login via IdP if the user doesn't exist in the DataRobot application.}

\item{\code{certificate}}{\link{SamlCertificate} \link{optional}}

\item{\code{configurationType}}{character \link{optional} The type of the SSO configuration, defines the source of SSO metadata. It can be one of the following: `METADATA` - when IDP metadata is provided in the config, `METADATA_URL` - when an URL for metadata retrieval is provided in the config and `MANUAL` - when IDP sign-on/sign-out URLs and certificate are provided.}

\item{\code{enableSso}}{character \link{optional} Defines if SSO is enabled.}

\item{\code{enforceSso}}{character \link{optional} Defines if SSO is enforced.}

\item{\code{entityId}}{character \link{optional} The globally unique identifier of the entity. Provided by IdP service.}

\item{\code{groupMapping}}{list( \link{SamlGroupMapping} ) \link{optional} The list of DataRobot group to identity provider group maps.}

\item{\code{idpMetadata}}{\link{SamlMetadataFile} \link{optional}}

\item{\code{idpMetadataHttpsVerify}}{character \link{optional} When idp_metadata_url uses HTTPS, require the server to have a trusted certificate. To avoid security vulnerabilities, only set to False when a trusted server has a self-signed certificate.}

\item{\code{idpMetadataUrl}}{character \link{optional} URL to the IdP SSO descriptor. Provided by IdP service.}

\item{\code{idpResponseMethod}}{character \link{optional} Identity provider response method, used to move user from IdP's authentication form back to the DataRobot side.}

\item{\code{issuer}}{character \link{optional} Optional Issuer field that may be required by IdP.}

\item{\code{name}}{character \link{optional} The name of the SSO configuration.}

\item{\code{organizationId}}{character \link{optional} The organization ID to which the SSO config belongs.}

\item{\code{roleMapping}}{list( \link{SamlRoleMapping} ) \link{optional} The list of DataRobot access role to identity provider role maps.}

\item{\code{securityParameters}}{\link{SamlSecurityParameters} \link{optional}}

\item{\code{sessionLengthSeconds}}{integer \link{optional} Time window for the authentication session via IdP.}

\item{\code{signOnUrl}}{character \link{optional} URL to sign on via SSO.}

\item{\code{signOutUrl}}{character \link{optional} URL to sign out via SSO.}

\item{\code{spRequestMethod}}{character \link{optional} Service provider (DataRobot application) request method, is used to move user to the IdP's authentication form.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-UpdateSsoConfiguration-new}{\code{UpdateSsoConfiguration$new()}}
\item \href{#method-UpdateSsoConfiguration-validate}{\code{UpdateSsoConfiguration$validate()}}
\item \href{#method-UpdateSsoConfiguration-toJSON}{\code{UpdateSsoConfiguration$toJSON()}}
\item \href{#method-UpdateSsoConfiguration-fromJSON}{\code{UpdateSsoConfiguration$fromJSON()}}
\item \href{#method-UpdateSsoConfiguration-clone}{\code{UpdateSsoConfiguration$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UpdateSsoConfiguration-new"></a>}}
\if{latex}{\out{\hypertarget{method-UpdateSsoConfiguration-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UpdateSsoConfiguration$new(
  advancedConfiguration = NULL,
  attributeMapping = NULL,
  autoGenerateUsers = NULL,
  certificate = NULL,
  configurationType = NULL,
  enableSso = NULL,
  enforceSso = NULL,
  entityId = NULL,
  groupMapping = NULL,
  idpMetadata = NULL,
  idpMetadataHttpsVerify = NULL,
  idpMetadataUrl = NULL,
  idpResponseMethod = NULL,
  issuer = NULL,
  name = NULL,
  organizationId = NULL,
  roleMapping = NULL,
  securityParameters = NULL,
  sessionLengthSeconds = NULL,
  signOnUrl = NULL,
  signOutUrl = NULL,
  spRequestMethod = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{autoGenerateUsers}}{determines if DataRobot automatically creates an account on first successful login via IdP if the user doesn't exist in the DataRobot application.}

\item{\code{configurationType}}{The type of the SSO configuration, defines the source of SSO metadata. It can be one of the following: `METADATA` - when IDP metadata is provided in the config, `METADATA_URL` - when an URL for metadata retrieval is provided in the config and `MANUAL` - when IDP sign-on/sign-out URLs and certificate are provided.}

\item{\code{enableSso}}{Defines if SSO is enabled.}

\item{\code{enforceSso}}{Defines if SSO is enforced.}

\item{\code{entityId}}{The globally unique identifier of the entity. Provided by IdP service.}

\item{\code{groupMapping}}{The list of DataRobot group to identity provider group maps.}

\item{\code{idpMetadataHttpsVerify}}{When idp_metadata_url uses HTTPS, require the server to have a trusted certificate. To avoid security vulnerabilities, only set to False when a trusted server has a self-signed certificate.}

\item{\code{idpMetadataUrl}}{URL to the IdP SSO descriptor. Provided by IdP service.}

\item{\code{idpResponseMethod}}{Identity provider response method, used to move user from IdP's authentication form back to the DataRobot side.}

\item{\code{issuer}}{Optional Issuer field that may be required by IdP.}

\item{\code{name}}{The name of the SSO configuration.}

\item{\code{organizationId}}{The organization ID to which the SSO config belongs.}

\item{\code{roleMapping}}{The list of DataRobot access role to identity provider role maps.}

\item{\code{sessionLengthSeconds}}{Time window for the authentication session via IdP.}

\item{\code{signOnUrl}}{URL to sign on via SSO.}

\item{\code{signOutUrl}}{URL to sign out via SSO.}

\item{\code{spRequestMethod}}{Service provider (DataRobot application) request method, is used to move user to the IdP's authentication form.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UpdateSsoConfiguration-validate"></a>}}
\if{latex}{\out{\hypertarget{method-UpdateSsoConfiguration-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UpdateSsoConfiguration$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UpdateSsoConfiguration-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-UpdateSsoConfiguration-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UpdateSsoConfiguration$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UpdateSsoConfiguration-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-UpdateSsoConfiguration-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UpdateSsoConfiguration$fromJSON(
  UpdateSsoConfigurationJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{UpdateSsoConfigurationJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UpdateSsoConfiguration-clone"></a>}}
\if{latex}{\out{\hypertarget{method-UpdateSsoConfiguration-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UpdateSsoConfiguration$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
