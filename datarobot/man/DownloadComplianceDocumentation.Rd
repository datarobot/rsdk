% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ComplianceDocumentation.R
\name{DownloadComplianceDocumentation}
\alias{DownloadComplianceDocumentation}
\title{Download compliance documentation (in DOCX format).}
\usage{
DownloadComplianceDocumentation(
  model,
  filename,
  templateId = NULL,
  create = TRUE,
  maxWait = 600
)
}
\arguments{
\item{model}{An S3 object of class dataRobotModel like that returned by
the function GetModel, or each element of the list returned by
the function ListModels.}

\item{filename}{character. Filename of file to save the compliance documentation to.}

\item{templateId}{character. Optional. The ID of the template to use in generating custom
model documentation.}

\item{create}{logical. Should we create the compliance documentation prior to downloading?}

\item{maxWait}{integer. How long to wait (in seconds) for compliance documentation creation
before raising a timeout error? Default 600.}
}
\value{
Nothing returned, but downloads the file to the stated filename.
}
\description{
This function will create the compliance documentation first if it has not already been created.
To create compliance documentation without downloading it, use
\code{CreateComplianceDocumentation}. You can then skip the create step in this function by using
`create = FALSE`.
}
\examples{
\dontrun{
projectId <- "59a5af20c80891534e3c2bde"
modelId <- "5996f820af07fc605e81ead4"
model <- GetModel(projectId, modelId)
DownloadComplianceDocumentation(model)
}
}
