% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/documentation_apiWrapper.R
\name{CreateAutomatedDocuments}
\alias{CreateAutomatedDocuments}
\title{CreateAutomatedDocuments}
\usage{
CreateAutomatedDocuments(
  documentTypeSpecificParameters,
  documentType,
  entityId,
  templateId,
  locale,
  outputFormat,
  ...
)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Request generation of automated document
}
\details{
Request generation of an automated document that's available for your account. Below is an example request body to generate Model Compliance documentation:  \verb{json \{     \\"documentType\\": \\"MODEL_COMPLIANCE\\",     \\"entityId\\": \\"507f191e810c19729de860ea\\",     \\"outputFormat\\": \\"docx\\" \}}  For Autopilot Summary, set a corresponding document type, \code{AUTOPILOT_SUMMARY}, and assign a needed project ID to the \code{entityId} value.  After the request is sent, the jobs needed for document generation are queued. You can see the status of the generation by polling the URL in the \code{Location} headers. After the generation is complete, the status URL will automatically redirect you to the resource location to download the document.

This method invokes \verb{POST /automatedDocuments/} in the DataRobot Public API.
}
\seealso{
The method AutomatedDocumentsCreate in \link[datarobot.apicore:DocumentationApi]{datarobot.apicore::DocumentationApi}, which invokes the same underlying API endpoint.
}
