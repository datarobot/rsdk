% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/applications_apiWrapper.R
\name{ListApplicationsSharedRoles}
\alias{ListApplicationsSharedRoles}
\title{ListApplicationsSharedRoles}
\usage{
ListApplicationsSharedRoles(
  next_,
  shareRecipientType,
  previous,
  data,
  count,
  name,
  limit,
  id,
  totalCount,
  applicationId,
  offset = 0,
  ...
)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Get a list of users, groups and organizations that have an access to this application
}
\details{
Get a list of users, groups and organizations that have an access to this application

This method invokes \verb{GET /applications/\{applicationId\}/sharedRoles/} in the DataRobot Public API.
}
\seealso{
The method ApplicationsSharedRolesList in \link{ApplicationsApi}, which invokes the same underlying API endpoint.
}
