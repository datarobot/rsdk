% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/images_apiWrapper.R
\name{CreateImageAugmentationSamples}
\alias{CreateImageAugmentationSamples}
\title{CreateImageAugmentationSamples}
\usage{
CreateImageAugmentationSamples(
  transformationProbability,
  samplesId,
  featureName,
  transformations,
  name,
  numberOfRows,
  id,
  numberOfNewImages,
  projectId,
  initialList = FALSE,
  inUse = FALSE,
  ...
)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
(Deprecated in v2.28) Requests the creation of sample augmentations based on the augmentation list parameters passed in. This route is being deprecated please use \verb{POST /imageAugmentationLists/<augmentationId>/samples} route instead.
}
\details{
(Deprecated in v2.28) This endpoint will schedule a job to augment the specified images from a project's dataset and return a link to monitor the status of the job, as well as a link to retrieve the resulting augmented images.

This method invokes \verb{POST /imageAugmentationSamples/} in the DataRobot Public API.
}
\seealso{
The method ImageAugmentationSamplesCreate in \link{ImagesApi}, which invokes the same underlying API endpoint.
}
