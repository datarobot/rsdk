% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_package_datasets.R
\docType{class}
\name{ModelPackageDatasets}
\alias{ModelPackageDatasets}
\title{ModelPackageDatasets}
\format{
An \code{R6Class} generator object
}
\description{
ModelPackageDatasets Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{baselineSegmentedBy}}{list( character ) Names of categorical features by which the training baseline was segmented. This allows for deployment prediction requests to be segmented by those same features. Segmenting the training baseline by these features allows for users to perform segmented analysis of Data Drift and Accuracy, and to compare the same subset of training and scoring data based on the selected segment attribute and segment value.}

\item{\code{datasetName}}{character Name of dataset used to train the model}

\item{\code{holdoutDataCatalogId}}{character ID for holdout data (returned from uploading a data set)}

\item{\code{holdoutDatasetName}}{character Name of dataset used for model holdout}

\item{\code{targetHistogramBaseline}}{character \link{optional} Values used to establish the training baseline}

\item{\code{trainingDataCatalogId}}{character ID for training data (returned from uploading a data set)}

\item{\code{trainingDataSize}}{integer \link{optional} Number of rows in training data (used by DR models)}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ModelPackageDatasets-new}{\code{ModelPackageDatasets$new()}}
\item \href{#method-ModelPackageDatasets-validate}{\code{ModelPackageDatasets$validate()}}
\item \href{#method-ModelPackageDatasets-toJSON}{\code{ModelPackageDatasets$toJSON()}}
\item \href{#method-ModelPackageDatasets-fromJSON}{\code{ModelPackageDatasets$fromJSON()}}
\item \href{#method-ModelPackageDatasets-clone}{\code{ModelPackageDatasets$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelPackageDatasets-new"></a>}}
\if{latex}{\out{\hypertarget{method-ModelPackageDatasets-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelPackageDatasets$new(
  baselineSegmentedBy = NULL,
  datasetName = NULL,
  holdoutDataCatalogId = NULL,
  holdoutDatasetName = NULL,
  trainingDataCatalogId = NULL,
  targetHistogramBaseline = NULL,
  trainingDataSize = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{baselineSegmentedBy}}{Names of categorical features by which the training baseline was segmented. This allows for deployment prediction requests to be segmented by those same features. Segmenting the training baseline by these features allows for users to perform segmented analysis of Data Drift and Accuracy, and to compare the same subset of training and scoring data based on the selected segment attribute and segment value.}

\item{\code{datasetName}}{Name of dataset used to train the model}

\item{\code{holdoutDataCatalogId}}{ID for holdout data (returned from uploading a data set)}

\item{\code{holdoutDatasetName}}{Name of dataset used for model holdout}

\item{\code{trainingDataCatalogId}}{ID for training data (returned from uploading a data set)}

\item{\code{targetHistogramBaseline}}{Values used to establish the training baseline}

\item{\code{trainingDataSize}}{Number of rows in training data (used by DR models)}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelPackageDatasets-validate"></a>}}
\if{latex}{\out{\hypertarget{method-ModelPackageDatasets-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelPackageDatasets$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelPackageDatasets-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ModelPackageDatasets-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelPackageDatasets$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelPackageDatasets-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ModelPackageDatasets-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelPackageDatasets$fromJSON(ModelPackageDatasetsJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ModelPackageDatasetsJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelPackageDatasets-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ModelPackageDatasets-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelPackageDatasets$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
