% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ai_catalog_apiWrapper.R
\name{PatchDatasetsRefreshJobs}
\alias{PatchDatasetsRefreshJobs}
\title{PatchDatasetsRefreshJobs}
\usage{
PatchDatasetsRefreshJobs(
  schedule,
  jobId,
  credentials,
  name,
  credentialId,
  datasetId,
  categories,
  scheduleReferenceDate,
  enabled,
  useKerberos,
  ...
)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Update a dataset refresh job
}
\details{
Update a dataset refresh job.  Optionally if the limit of enabled jobs per user is reached the following metadata will be added to the default error response payload:  - \strong{datasetsWithJob} (\emph{array}) - The list of datasets IDs that have at   least one enabled job. - \strong{errorType} (\emph{string}) - (New in version v2.21) The type of error   that happened, possible values include (but are not limited to):   \verb{Generic Limit Reached}, \verb{Max Job Limit Reached for Dataset}, and   \verb{Max Job Limit Reached for User}.

This method invokes \verb{PATCH /datasets/\{datasetId\}/refreshJobs/\{jobId\}/} in the DataRobot Public API.
}
\seealso{
The method DatasetsRefreshJobsPatch in \link{AiCatalogApi}, which invokes the same underlying API endpoint.
}
