% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/train_datetime_model.R
\docType{class}
\name{TrainDatetimeModel}
\alias{TrainDatetimeModel}
\title{TrainDatetimeModel}
\format{
An \code{R6Class} generator object
}
\description{
TrainDatetimeModel Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{blueprintId}}{character The ID of a blueprint to use to generate the model. Allowed blueprints can be retrieved using :http:get:`/api/v2/projects/(projectId)/blueprints/` or taken from existing models.}

\item{\code{featurelistId}}{character \link{optional} If specified, the model will be trained using this featurelist. If not specified, the recommended featurelist for the specified blueprint will be used. If there is no recommended featurelist, the project's default will be used.}

\item{\code{monotonicDecreasingFeaturelistId}}{character \link{optional} The ID of the featurelist that defines the set of features with a monotonically decreasing relationship to the target. If ``null``, no constraints will be enforced. If omitted, the project default is used. May only be specified for OTV projects.}

\item{\code{monotonicIncreasingFeaturelistId}}{character \link{optional} The ID of the featurelist that defines the set of features with a monotonically increasing relationship to the target. If ``null``, no constraints will be enforced. If omitted, the project default is used. May only be specified for OTV projects.}

\item{\code{nClusters}}{integer \link{optional} The number of clusters to use in the specified unsupervised clustering model. Only valid in unsupervised clustering projects.}

\item{\code{samplingMethod}}{character \link{optional} Defines how training data is selected if subsampling is used (e.g., if `timeWindowSamplePct` is specified). Can be either ``random`` or ``latest``. If omitted, defaults to ``latest`` if `trainingRowCount` is used and ``random`` for other cases (e.g., if `trainingDuration` or `useProjectSettings` is specified). May only be specified for OTV projects.}

\item{\code{sourceProjectId}}{character \link{optional} The project the blueprint comes from. Required only if the `blueprintId` comes from a different project.}

\item{\code{timeWindowSamplePct}}{integer \link{optional} An integer between 1 and 99 indicating the percentage of sampling within the time window. The points kept are determined by the value provided for the `samplingMethod` option. If specified, `trainingRowCount` may not be specified, and the specified model must either be a duration or selectedDateRange model, or one of `trainingDuration` or `trainingStartDate` and `trainingEndDate` must be specified.}

\item{\code{trainingDuration}}{character \link{optional} A duration string representing the training duration for the submitted model.}

\item{\code{trainingRowCount}}{integer \link{optional} The number of rows of data that should be used when training this model.}

\item{\code{useProjectSettings}}{character \link{optional} If ``True``, the model will be trained using the previously-specified custom backtest training settings.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-TrainDatetimeModel-new}{\code{TrainDatetimeModel$new()}}
\item \href{#method-TrainDatetimeModel-validate}{\code{TrainDatetimeModel$validate()}}
\item \href{#method-TrainDatetimeModel-toJSON}{\code{TrainDatetimeModel$toJSON()}}
\item \href{#method-TrainDatetimeModel-fromJSON}{\code{TrainDatetimeModel$fromJSON()}}
\item \href{#method-TrainDatetimeModel-clone}{\code{TrainDatetimeModel$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TrainDatetimeModel-new"></a>}}
\if{latex}{\out{\hypertarget{method-TrainDatetimeModel-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TrainDatetimeModel$new(
  blueprintId = NULL,
  featurelistId = NULL,
  monotonicDecreasingFeaturelistId = NULL,
  monotonicIncreasingFeaturelistId = NULL,
  nClusters = NULL,
  samplingMethod = NULL,
  sourceProjectId = NULL,
  timeWindowSamplePct = NULL,
  trainingDuration = NULL,
  trainingRowCount = NULL,
  useProjectSettings = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{blueprintId}}{The ID of a blueprint to use to generate the model. Allowed blueprints can be retrieved using :http:get:`/api/v2/projects/(projectId)/blueprints/` or taken from existing models.}

\item{\code{featurelistId}}{If specified, the model will be trained using this featurelist. If not specified, the recommended featurelist for the specified blueprint will be used. If there is no recommended featurelist, the project's default will be used.}

\item{\code{monotonicDecreasingFeaturelistId}}{The ID of the featurelist that defines the set of features with a monotonically decreasing relationship to the target. If ``null``, no constraints will be enforced. If omitted, the project default is used. May only be specified for OTV projects.}

\item{\code{monotonicIncreasingFeaturelistId}}{The ID of the featurelist that defines the set of features with a monotonically increasing relationship to the target. If ``null``, no constraints will be enforced. If omitted, the project default is used. May only be specified for OTV projects.}

\item{\code{nClusters}}{The number of clusters to use in the specified unsupervised clustering model. Only valid in unsupervised clustering projects.}

\item{\code{samplingMethod}}{Defines how training data is selected if subsampling is used (e.g., if `timeWindowSamplePct` is specified). Can be either ``random`` or ``latest``. If omitted, defaults to ``latest`` if `trainingRowCount` is used and ``random`` for other cases (e.g., if `trainingDuration` or `useProjectSettings` is specified). May only be specified for OTV projects.}

\item{\code{sourceProjectId}}{The project the blueprint comes from. Required only if the `blueprintId` comes from a different project.}

\item{\code{timeWindowSamplePct}}{An integer between 1 and 99 indicating the percentage of sampling within the time window. The points kept are determined by the value provided for the `samplingMethod` option. If specified, `trainingRowCount` may not be specified, and the specified model must either be a duration or selectedDateRange model, or one of `trainingDuration` or `trainingStartDate` and `trainingEndDate` must be specified.}

\item{\code{trainingDuration}}{A duration string representing the training duration for the submitted model.}

\item{\code{trainingRowCount}}{The number of rows of data that should be used when training this model.}

\item{\code{useProjectSettings}}{If ``True``, the model will be trained using the previously-specified custom backtest training settings.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TrainDatetimeModel-validate"></a>}}
\if{latex}{\out{\hypertarget{method-TrainDatetimeModel-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TrainDatetimeModel$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TrainDatetimeModel-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-TrainDatetimeModel-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TrainDatetimeModel$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TrainDatetimeModel-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-TrainDatetimeModel-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TrainDatetimeModel$fromJSON(TrainDatetimeModelJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{TrainDatetimeModelJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TrainDatetimeModel-clone"></a>}}
\if{latex}{\out{\hypertarget{method-TrainDatetimeModel-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TrainDatetimeModel$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
