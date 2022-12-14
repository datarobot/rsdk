% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projects_apiWrapper.R, R/Features.R
\name{BatchFeaturesTypeTransform}
\alias{BatchFeaturesTypeTransform}
\title{BatchFeaturesTypeTransform}
\usage{
BatchFeaturesTypeTransform(
  project,
  parentNames,
  variableType,
  prefix = NULL,
  suffix = NULL,
  maxWait = 600,
  ...
)
}
\arguments{
\item{project}{character. Either (1) a character string giving the unique alphanumeric
identifier for the project, or (2) a list containing the element projectId with this
identifier.}

\item{parentNames}{character. Character vector of variable names to be transformed.}

\item{variableType}{character. The new type that the columns should be converted to.
See \code{VariableTransformTypes}.}

\item{prefix}{character. Optional. The string to preface all the transformed features.
Either \code{prefix} or \code{suffix} or both must be provided.}

\item{suffix}{character. Optional. The string that will be appended at the end to all
the transformed features. Either \code{prefix} or \code{suffix} or both must be provided.}

\item{maxWait}{integer. Optional. The maximum amount of time (in seconds) to wait for
DataRobot to finish processing the new column before providing a timeout error.}

\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\value{
a list of all the features, after transformation. See \code{GetFeaturelist}
 for details.
}
\description{
Create multiple new features by changing the type of existing features.
}
\details{
Create multiple new features by changing the type of existing features.

This method invokes \verb{POST /projects/\{projectId\}/batchTypeTransformFeatures/} in the DataRobot Public API.

Create new features by transforming the type of an existing ones.

Supports feature transformations, including:
\itemize{
   \item text to categorical
   \item text to numeric
   \item categorical to text
   \item categorical to numeric
   \item numeric to categorical
}
}
\examples{
\dontrun{
projectId <- "59a5af20c80891534e3c2bde"
BatchFeaturesTypeTransform(projectId,
  parentNames = c("var1", "var2"),
  variableType = VariableTransformTypes$Categorical,
  suffix = "_transformed"
)
}
}
\seealso{
The method BatchTypeTransformFeaturesCreate in \link[datarobot.apicore:ProjectsApi]{datarobot.apicore::ProjectsApi}, which invokes the same underlying API endpoint.
}
