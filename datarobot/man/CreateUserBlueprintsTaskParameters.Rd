% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blueprints_apiWrapper.R,
%   R/ai_catalog_apiWrapper.R
\name{CreateUserBlueprintsTaskParameters}
\alias{CreateUserBlueprintsTaskParameters}
\title{CreateUserBlueprintsTaskParameters}
\usage{
CreateUserBlueprintsTaskParameters(
  taskCode,
  outputMethod,
  taskParameters,
  projectId,
  ...
)

CreateUserBlueprintsTaskParameters(
  taskCode,
  outputMethod,
  taskParameters,
  projectId,
  ...
)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Validate task parameters.

Validate task parameters.
}
\details{
Validate that each value assigned to specified task parameters are valid.

This method invokes \verb{POST /userBlueprintsTaskParameters/} in the DataRobot Public API.

Validate that each value assigned to specified task parameters are valid.

This method invokes \verb{POST /userBlueprintsTaskParameters/} in the DataRobot Public API.
}
\seealso{
The method UserBlueprintsTaskParametersCreate in \link[datarobot.apicore:BlueprintsApi]{datarobot.apicore::BlueprintsApi}, which invokes the same underlying API endpoint.

The method UserBlueprintsTaskParametersCreate in \link[datarobot.apicore:AiCatalogApi]{datarobot.apicore::AiCatalogApi}, which invokes the same underlying API endpoint.
}
