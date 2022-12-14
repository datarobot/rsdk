% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/notification_channel_create.R
\docType{class}
\name{NotificationChannelCreate}
\alias{NotificationChannelCreate}
\title{NotificationChannelCreate}
\format{
An \code{R6Class} generator object
}
\description{
NotificationChannelCreate Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{channelType}}{character The type of the new notification channel.}

\item{\code{contentType}}{character \link{optional} The content type of the messages of the new notification channel.}

\item{\code{customHeaders}}{list( \link{CustomerHeader} ) \link{optional} Custom headers and their values to be sent in the new notification channel.}

\item{\code{emailAddress}}{character \link{optional} The email address to be used in the new notification channel.}

\item{\code{languageCode}}{character \link{optional} The preferred language code.}

\item{\code{name}}{character The name of the new notification channel.}

\item{\code{orgId}}{character \link{optional} The id of organization that notification channel belongs to.}

\item{\code{payloadUrl}}{character \link{optional} The payload URL of the new notification channel.}

\item{\code{secretToken}}{character \link{optional} Secret token to be used for new notification channel.}

\item{\code{validateSsl}}{character \link{optional} Defines if  validate ssl or not in the notification channel.}

\item{\code{verificationCode}}{character \link{optional} Required if the channel type is Email.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-NotificationChannelCreate-new}{\code{NotificationChannelCreate$new()}}
\item \href{#method-NotificationChannelCreate-validate}{\code{NotificationChannelCreate$validate()}}
\item \href{#method-NotificationChannelCreate-toJSON}{\code{NotificationChannelCreate$toJSON()}}
\item \href{#method-NotificationChannelCreate-fromJSON}{\code{NotificationChannelCreate$fromJSON()}}
\item \href{#method-NotificationChannelCreate-clone}{\code{NotificationChannelCreate$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NotificationChannelCreate-new"></a>}}
\if{latex}{\out{\hypertarget{method-NotificationChannelCreate-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NotificationChannelCreate$new(
  channelType = NULL,
  name = NULL,
  contentType = NULL,
  customHeaders = NULL,
  emailAddress = NULL,
  languageCode = NULL,
  orgId = NULL,
  payloadUrl = NULL,
  secretToken = NULL,
  validateSsl = NULL,
  verificationCode = NULL,
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

\item{\code{languageCode}}{The preferred language code.}

\item{\code{orgId}}{The id of organization that notification channel belongs to.}

\item{\code{payloadUrl}}{The payload URL of the new notification channel.}

\item{\code{secretToken}}{Secret token to be used for new notification channel.}

\item{\code{validateSsl}}{Defines if  validate ssl or not in the notification channel.}

\item{\code{verificationCode}}{Required if the channel type is Email.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NotificationChannelCreate-validate"></a>}}
\if{latex}{\out{\hypertarget{method-NotificationChannelCreate-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NotificationChannelCreate$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NotificationChannelCreate-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-NotificationChannelCreate-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NotificationChannelCreate$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NotificationChannelCreate-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-NotificationChannelCreate-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NotificationChannelCreate$fromJSON(
  NotificationChannelCreateJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{NotificationChannelCreateJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NotificationChannelCreate-clone"></a>}}
\if{latex}{\out{\hypertarget{method-NotificationChannelCreate-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NotificationChannelCreate$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
