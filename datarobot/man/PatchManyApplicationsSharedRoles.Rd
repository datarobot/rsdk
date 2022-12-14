% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/applications_apiWrapper.R
\name{PatchManyApplicationsSharedRoles}
\alias{PatchManyApplicationsSharedRoles}
\title{PatchManyApplicationsSharedRoles}
\usage{
PatchManyApplicationsSharedRoles(
  roles,
  applicationId,
  operation,
  note = "",
  sendNotification = FALSE,
  ...
)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Share an application with a user, group, or organization
}
\details{
Share an application with a user, group, or organization

This method invokes \verb{PATCH /applications/\{applicationId\}/sharedRoles/} in the DataRobot Public API.
}
\seealso{
The method ApplicationsSharedRolesPatchMany in \link[datarobot.apicore:ApplicationsApi]{datarobot.apicore::ApplicationsApi}, which invokes the same underlying API endpoint.
}
