% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projects_apiWrapper.R
\name{CreateModelingFeaturelist}
\alias{CreateModelingFeaturelist}
\title{CreateModelingFeaturelist}
\usage{
CreateModelingFeaturelist(project, listName, featureNames, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Create a new modeling featurelist.
}
\details{
Create new modeling featurelist from list of feature names. Only time series projects differentiate between modeling and input featurelists. On other projects, this route will behave the same as :\url{http:post}:/api/v2/projects/(projectId)/featurelists/. On time series projects, this can be used after the target has been set in order to create a new featurelist on the modeling features, although the previously mentioned route for creating featurelists will be disabled. On time series projects, only modeling features may be passed to this route to create a featurelist.

This method invokes \verb{POST /projects/\{projectId\}/modelingFeaturelists/} in the DataRobot Public API.
}
\seealso{
The method ModelingFeaturelistsCreate in \link{ProjectsApi}, which invokes the same underlying API endpoint.
}
