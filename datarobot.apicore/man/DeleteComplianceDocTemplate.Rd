% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/documentation_apiWrapper.R
\name{DeleteComplianceDocTemplate}
\alias{DeleteComplianceDocTemplate}
\title{DeleteComplianceDocTemplate}
\usage{
DeleteComplianceDocTemplate(templateId, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Delete a compliance documentation template
}
\details{
Delete a compliance documentation template. Documentation previously generated using this template will remain unchanged.

This method invokes \verb{DELETE /complianceDocTemplates/\{templateId\}/} in the DataRobot Public API.
}
\seealso{
The method ComplianceDocTemplatesDelete in \link{DocumentationApi}, which invokes the same underlying API endpoint.
}
