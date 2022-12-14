% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StarredModels.R
\name{StarModel}
\alias{StarModel}
\title{Star a model.}
\usage{
StarModel(model)
}
\arguments{
\item{model}{character. The model for which you want to compute Feature Impact, e.g.
from the list of models returned by \code{ListModels(project)}.}
}
\value{
the model object, but now starred
}
\description{
Star a model.
}
\examples{
\dontrun{
projectId <- "59a5af20c80891534e3c2bde"
modelId <- "5996f820af07fc605e81ead4"
model <- GetModel(projectId, modelId)
StarModel(model)
}
}
