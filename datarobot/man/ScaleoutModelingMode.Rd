% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Enums.R
\docType{data}
\name{ScaleoutModelingMode}
\alias{ScaleoutModelingMode}
\title{Scaleout modeling modes}
\format{
An object of class \code{list} of length 3.
}
\usage{
ScaleoutModelingMode
}
\description{
This is a list that contains the valid values for the \code{scaleoutModelingMode} parameter
found in \code{SetTarget}. If you wish, you can specify \code{scaleoutModelingMode} using the
list values here, e.g. ScaleoutModelingMode$Autopilot instead of "Autopilot".
}
\details{
For \code{Disabled}, no scaleout models will run in autopilot or be available in blueprints.

For \code{RepositoryOnly}, scaleout models will be available in blueprints and can be run
manually, but will not run in autopilot.

For \code{Autopilot}, scaleout models will run during autopilot and also be available in
blueprints.
}
\keyword{datasets}
