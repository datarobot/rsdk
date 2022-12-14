% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/documentation_apiWrapper.R
\name{GetComplianceDocTemplate}
\alias{GetComplianceDocTemplate}
\title{GetComplianceDocTemplate}
\usage{
GetComplianceDocTemplate(templateId = NULL, type = NULL, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Retrieve a documentation template
}
\details{
Retrieve a JSON representation of a custom Compliance Documentation template.

This method invokes \verb{GET /complianceDocTemplates/\{templateId\}/} in the DataRobot Public API.
}
\seealso{
The method ComplianceDocTemplatesRetrieve in \link{DocumentationApi}, which invokes the same underlying API endpoint.
}
