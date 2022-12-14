% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models_apiWrapper.R
\name{CreateModelPackagesArchive}
\alias{CreateModelPackagesArchive}
\title{CreateModelPackagesArchive}
\usage{
CreateModelPackagesArchive(modelPackageId, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Archive a model package.
}
\details{
Permanently archive a model package. It will no longer be able to be used in new deployments or replacement. It will not be accessible in the model package list api. It will only be accessible at the model package retrieve route for this model package.

This method invokes \verb{POST /modelPackages/\{modelPackageId\}/archive/} in the DataRobot Public API.
}
\seealso{
The method ModelPackagesArchiveCreate in \link[datarobot.apicore:ModelsApi]{datarobot.apicore::ModelsApi}, which invokes the same underlying API endpoint.
}
