% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/notification_webhook_channel_test_create.R
\docType{class}
\name{NotificationWebhookChannelTestCreate}
\alias{NotificationWebhookChannelTestCreate}
\title{NotificationWebhookChannelTestCreate}
\format{
An \code{R6Class} generator object
}
\description{
NotificationWebhookChannelTestCreate Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{channelType}}{character The type of the new notification channel.}

\item{\code{contentType}}{character \link{optional} The content type of the messages of the new notification channel.}

\item{\code{customHeaders}}{list( \link{CustomerHeader} ) \link{optional} Custom headers and their values to be sent in the new notification channel.}

\item{\code{emailAddress}}{character \link{optional} The email address to be used in the new notification channel.}

\item{\code{name}}{character The name of the new notification channel.}

\item{\code{orgId}}{character \link{optional} The identifier of the organization that notification channel belongs to.}

\item{\code{payloadUrl}}{character \link{optional} The payload URL of the new notification channel.}

\item{\code{secretToken}}{character \link{optional} Secret token to be used for new notification channel.}

\item{\code{validateSsl}}{character \link{optional} Whether SSL will be validated in the notification channel.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-NotificationWebhookChannelTestCreate-new}{\code{NotificationWebhookChannelTestCreate$new()}}
\item \href{#method-NotificationWebhookChannelTestCreate-validate}{\code{NotificationWebhookChannelTestCreate$validate()}}
\item \href{#method-NotificationWebhookChannelTestCreate-toJSON}{\code{NotificationWebhookChannelTestCreate$toJSON()}}
\item \href{#method-NotificationWebhookChannelTestCreate-fromJSON}{\code{NotificationWebhookChannelTestCreate$fromJSON()}}
\item \href{#method-NotificationWebhookChannelTestCreate-clone}{\code{NotificationWebhookChannelTestCreate$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NotificationWebhookChannelTestCreate-new"></a>}}
\if{latex}{\out{\hypertarget{method-NotificationWebhookChannelTestCreate-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NotificationWebhookChannelTestCreate$new(
  channelType = NULL,
  name = NULL,
  contentType = NULL,
  customHeaders = NULL,
  emailAddress = NULL,
  orgId = NULL,
  payloadUrl = NULL,
  secretToken = NULL,
  validateSsl = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{channelType}}{The type of the new notification channel.}

\item{\code{name}}{The name of the new notification channel.}

\item{\code{contentType}}{The content type of the messages of the new notification channel.}

\item{\code{customHeaders}}{Custom headers and their values to be sent in the new notification channel.}

\item{\code{emailAddress}}{The email address to be used in the new notification channel.}

\item{\code{orgId}}{The identifier of the organization that notification channel belongs to.}

\item{\code{payloadUrl}}{The payload URL of the new notification channel.}

\item{\code{secretToken}}{Secret token to be used for new notification channel.}

\item{\code{validateSsl}}{Whether SSL will be validated in the notification channel.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NotificationWebhookChannelTestCreate-validate"></a>}}
\if{latex}{\out{\hypertarget{method-NotificationWebhookChannelTestCreate-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NotificationWebhookChannelTestCreate$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NotificationWebhookChannelTestCreate-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-NotificationWebhookChannelTestCreate-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NotificationWebhookChannelTestCreate$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NotificationWebhookChannelTestCreate-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-NotificationWebhookChannelTestCreate-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NotificationWebhookChannelTestCreate$fromJSON(
  NotificationWebhookChannelTestCreateJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{NotificationWebhookChannelTestCreateJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NotificationWebhookChannelTestCreate-clone"></a>}}
\if{latex}{\out{\hypertarget{method-NotificationWebhookChannelTestCreate-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NotificationWebhookChannelTestCreate$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
