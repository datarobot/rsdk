% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/documentation_apiWrapper.R
\name{UpdateComplianceDocTemplate}
\alias{UpdateComplianceDocTemplate}
\title{UpdateComplianceDocTemplate}
\usage{
UpdateComplianceDocTemplate(templateId, name = NULL, sections = NULL, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Update an existing model compliance documentation template
}
\details{
Update an existing model compliance documentation template with the given \code{templateId}. The template must be accessible by the user. If the \code{templateId} is not found for the user, the update cannot be performed. For a description of the template \code{sections} object options, see the sample \code{sections} on the schema below.

This method invokes \verb{PATCH /complianceDocTemplates/\{templateId\}/} in the DataRobot Public API.
}
\seealso{
The method ComplianceDocTemplatesPatch in \link[datarobot.apicore:DocumentationApi]{datarobot.apicore::DocumentationApi}, which invokes the same underlying API endpoint.
}
