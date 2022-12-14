% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analytics_apiWrapper.R
\name{ListEventLogs}
\alias{ListEventLogs}
\title{ListEventLogs}
\usage{
ListEventLogs(
  maxTimestamp,
  auditReportType,
  event,
  userId,
  projectId,
  minTimestamp,
  orgId,
  offset = 0,
  includeIdentifyingFields = "true",
  order = "desc",
  ...
)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Retrieve one page of audit log records.
}
\details{
Retrieve one page of audit log records.

This method invokes \verb{GET /eventLogs/} in the DataRobot Public API.
}
\seealso{
The method EventLogsList in \link[datarobot.apicore:AnalyticsApi]{datarobot.apicore::AnalyticsApi}, which invokes the same underlying API endpoint.
}
