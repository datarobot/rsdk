% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/notifications_apiWrapper.R
\name{PutNotificationChannels}
\alias{PutNotificationChannels}
\title{PutNotificationChannels}
\usage{
PutNotificationChannels(
  emailAddress,
  secretToken,
  name,
  validateSsl,
  languageCode,
  contentType,
  customHeaders,
  channelId,
  verificationCode,
  payloadUrl,
  ...
)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Update notification channel
}
\details{
Update the notification channel.

This method invokes \verb{PUT /notificationChannels/\{channelId\}/} in the DataRobot Public API.
}
\seealso{
The method NotificationChannelsPut in \link{NotificationsApi}, which invokes the same underlying API endpoint.
}
