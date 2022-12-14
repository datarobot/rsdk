% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/project_partition_response.R
\docType{class}
\name{ProjectPartitionResponse}
\alias{ProjectPartitionResponse}
\title{ProjectPartitionResponse}
\format{
An \code{R6Class} generator object
}
\description{
ProjectPartitionResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{cvHoldoutLevel}}{\link{AnyOfstringnumberinteger} if a user partition column was used with cross validation, the value assigned to the holdout set}

\item{\code{cvMethod}}{character the partitioning method used. Note that \"date\" partitioning is an old partitioning method no longer supported for new projects, as of API version v2.0.}

\item{\code{datetimeCol}}{character if a date partition column was used, the name of the column. Note that datetimeCol applies to an old partitioning method no longer supported for new projects, as of API version v2.0.}

\item{\code{datetimePartitionColumn}}{character \link{optional} if a datetime partition column was used, the name of the column}

\item{\code{holdoutLevel}}{\link{AnyOfstringnumberinteger} if a user partition column was used with train-validation-holdout split, the value assigned to the holdout set}

\item{\code{holdoutPct}}{numeric the percentage of the dataset reserved for the holdout set}

\item{\code{partitionKeyCols}}{list( character ) An array containing a single string - the name of the group partition column}

\item{\code{reps}}{numeric if cross validation was used, the number of folds to use}

\item{\code{trainingLevel}}{\link{AnyOfstringnumberinteger} if a user partition column was used with train-validation-holdout split, the value assigned to the training set}

\item{\code{useTimeSeries}}{character A boolean value indicating whether a time series project was created as opposed to a regular project using datetime partitioning.}

\item{\code{userPartitionCol}}{character if a user partition column was used, the name of the column}

\item{\code{validationLevel}}{\link{AnyOfstringnumberinteger} if a user partition column was used with train-validation-holdout split, the value assigned to the validation set}

\item{\code{validationPct}}{numeric if train-validation-holdout split was used, the percentage of the dataset used for the validation set}

\item{\code{validationType}}{character either CV for cross-validation or TVH for train-validation-holdout split}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ProjectPartitionResponse-new}{\code{ProjectPartitionResponse$new()}}
\item \href{#method-ProjectPartitionResponse-validate}{\code{ProjectPartitionResponse$validate()}}
\item \href{#method-ProjectPartitionResponse-toJSON}{\code{ProjectPartitionResponse$toJSON()}}
\item \href{#method-ProjectPartitionResponse-fromJSON}{\code{ProjectPartitionResponse$fromJSON()}}
\item \href{#method-ProjectPartitionResponse-clone}{\code{ProjectPartitionResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ProjectPartitionResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-ProjectPartitionResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ProjectPartitionResponse$new(
  cvHoldoutLevel = NULL,
  cvMethod = NULL,
  datetimeCol = NULL,
  holdoutLevel = NULL,
  holdoutPct = NULL,
  partitionKeyCols = NULL,
  reps = NULL,
  trainingLevel = NULL,
  useTimeSeries = NULL,
  userPartitionCol = NULL,
  validationLevel = NULL,
  validationPct = NULL,
  validationType = NULL,
  datetimePartitionColumn = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{cvHoldoutLevel}}{if a user partition column was used with cross validation, the value assigned to the holdout set}

\item{\code{cvMethod}}{the partitioning method used. Note that \"date\" partitioning is an old partitioning method no longer supported for new projects, as of API version v2.0.}

\item{\code{datetimeCol}}{if a date partition column was used, the name of the column. Note that datetimeCol applies to an old partitioning method no longer supported for new projects, as of API version v2.0.}

\item{\code{holdoutLevel}}{if a user partition column was used with train-validation-holdout split, the value assigned to the holdout set}

\item{\code{holdoutPct}}{the percentage of the dataset reserved for the holdout set}

\item{\code{partitionKeyCols}}{An array containing a single string - the name of the group partition column}

\item{\code{reps}}{if cross validation was used, the number of folds to use}

\item{\code{trainingLevel}}{if a user partition column was used with train-validation-holdout split, the value assigned to the training set}

\item{\code{useTimeSeries}}{A boolean value indicating whether a time series project was created as opposed to a regular project using datetime partitioning.}

\item{\code{userPartitionCol}}{if a user partition column was used, the name of the column}

\item{\code{validationLevel}}{if a user partition column was used with train-validation-holdout split, the value assigned to the validation set}

\item{\code{validationPct}}{if train-validation-holdout split was used, the percentage of the dataset used for the validation set}

\item{\code{validationType}}{either CV for cross-validation or TVH for train-validation-holdout split}

\item{\code{datetimePartitionColumn}}{if a datetime partition column was used, the name of the column}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ProjectPartitionResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-ProjectPartitionResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ProjectPartitionResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ProjectPartitionResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ProjectPartitionResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ProjectPartitionResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ProjectPartitionResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ProjectPartitionResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ProjectPartitionResponse$fromJSON(
  ProjectPartitionResponseJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ProjectPartitionResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ProjectPartitionResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ProjectPartitionResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ProjectPartitionResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
