% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/project_advanced_options_response.R
\docType{class}
\name{ProjectAdvancedOptionsResponse}
\alias{ProjectAdvancedOptionsResponse}
\title{ProjectAdvancedOptionsResponse}
\format{
An \code{R6Class} generator object
}
\description{
ProjectAdvancedOptionsResponse Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{allowedPairwiseInteractionGroups}}{list( \link{array[character]} ) \link{optional} For GAM models - specify groups of columns for which pairwise interactions will be allowed. E.g. if set to [\link[=\\"A\\", \\"B\\", \\"C\\"]{\"A\", \"B\", \"C\"}, \link[=\\"C\\", \\"D\\"]{\"C\", \"D\"} ] then GAM models will allow interactions between columns AxB, BxC, AxC, CxD. All others (AxD, BxD) will not be considered. If not specified - all possible interactions will be considered by model.}

\item{\code{blendBestModels}}{character blend best models during Autopilot run}

\item{\code{blueprintThreshold}}{integer an upper bound on running time (in hours), such that models exceeding the bound will be excluded in subsequent autopilot runs}

\item{\code{considerBlendersInRecommendation}}{character \link{optional} Include blenders when selecting a model to prepare for deployment in an Autopilot Run.}

\item{\code{defaultMonotonicDecreasingFeaturelistId}}{character null or str, the ID of the featurelist specifying a set of features with a monotonically decreasing relationship to the target. All blueprints generated in the project use this as their default monotonic constraint, but it can be overriden at model submission time.}

\item{\code{defaultMonotonicIncreasingFeaturelistId}}{character null or str, the ID of the featurelist specifying a set of features with a monotonically increasing relationship to the target. All blueprints generated in the project use this as their default monotonic constraint, but it can be overriden at model submission time.}

\item{\code{downsampledMajorityRows}}{integer the total number of the majority rows available for modeling, or null for projects without smart downsampling}

\item{\code{downsampledMinorityRows}}{integer the total number of the minority rows available for modeling, or null for projects without smart downsampling}

\item{\code{eventsCount}}{character \link{optional} the name of the event count column, if specified, otherwise null.}

\item{\code{exposure}}{character \link{optional} the name of the exposure column, if specified.}

\item{\code{majorityDownsamplingRate}}{numeric the percentage between 0 and 100 of the majority rows that are kept, or null for projects without smart downsampling}

\item{\code{minSecondaryValidationModelCount}}{character \link{optional} Compute \"All backtest\" scores (datetime models) or cross validation scores for the specified number of highest ranking models on the Leaderboard, if over the Autopilot default.}

\item{\code{offset}}{list( character ) \link{optional} the list of names of the offset columns, if specified, otherwise null.}

\item{\code{onlyIncludeMonotonicBlueprints}}{character whether the project only includes blueprints support enforcing monotonic constraints}

\item{\code{prepareModelForDeployment}}{character Prepare model for deployment during Autopilot run. The preparation includes creating reduced feature list models, retraining best model on higher sample size, computing insights and assigning \"RECOMMENDED FOR DEPLOYMENT\" label.}

\item{\code{responseCap}}{\link{OneOfbooleannumber} defaults to False, if specified used to cap the maximum response of a model}

\item{\code{runLeakageRemovedFeatureList}}{character \link{optional} Run Autopilot on Leakage Removed feature list (if exists).}

\item{\code{scoringCodeOnly}}{character Keep only models that can be converted to scorable java code during Autopilot run.}

\item{\code{seed}}{character defaults to null, the random seed to be used if specified}

\item{\code{shapOnlyMode}}{character Keep only models that support SHAP values during Autopilot run. Use SHAP-based insights wherever possible. For pre SHAP-only mode projects this is always ``null``.}

\item{\code{smartDownsampled}}{character whether the project uses smart downsampling to throw away excess rows of the majority class.  Smart downsampled projects express all sample percents in terms of percent of minority rows (as opposed to percent of all rows).}

\item{\code{weights}}{character the name of the weight column, if specified, otherwise null.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ProjectAdvancedOptionsResponse-new}{\code{ProjectAdvancedOptionsResponse$new()}}
\item \href{#method-ProjectAdvancedOptionsResponse-validate}{\code{ProjectAdvancedOptionsResponse$validate()}}
\item \href{#method-ProjectAdvancedOptionsResponse-toJSON}{\code{ProjectAdvancedOptionsResponse$toJSON()}}
\item \href{#method-ProjectAdvancedOptionsResponse-fromJSON}{\code{ProjectAdvancedOptionsResponse$fromJSON()}}
\item \href{#method-ProjectAdvancedOptionsResponse-clone}{\code{ProjectAdvancedOptionsResponse$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ProjectAdvancedOptionsResponse-new"></a>}}
\if{latex}{\out{\hypertarget{method-ProjectAdvancedOptionsResponse-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ProjectAdvancedOptionsResponse$new(
  blendBestModels = NULL,
  blueprintThreshold = NULL,
  defaultMonotonicDecreasingFeaturelistId = NULL,
  defaultMonotonicIncreasingFeaturelistId = NULL,
  downsampledMajorityRows = NULL,
  downsampledMinorityRows = NULL,
  majorityDownsamplingRate = NULL,
  onlyIncludeMonotonicBlueprints = NULL,
  prepareModelForDeployment = NULL,
  responseCap = NULL,
  scoringCodeOnly = NULL,
  seed = NULL,
  shapOnlyMode = NULL,
  smartDownsampled = NULL,
  weights = NULL,
  allowedPairwiseInteractionGroups = NULL,
  considerBlendersInRecommendation = NULL,
  eventsCount = NULL,
  exposure = NULL,
  minSecondaryValidationModelCount = NULL,
  offset = NULL,
  runLeakageRemovedFeatureList = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{blendBestModels}}{blend best models during Autopilot run}

\item{\code{blueprintThreshold}}{an upper bound on running time (in hours), such that models exceeding the bound will be excluded in subsequent autopilot runs}

\item{\code{defaultMonotonicDecreasingFeaturelistId}}{null or str, the ID of the featurelist specifying a set of features with a monotonically decreasing relationship to the target. All blueprints generated in the project use this as their default monotonic constraint, but it can be overriden at model submission time.}

\item{\code{defaultMonotonicIncreasingFeaturelistId}}{null or str, the ID of the featurelist specifying a set of features with a monotonically increasing relationship to the target. All blueprints generated in the project use this as their default monotonic constraint, but it can be overriden at model submission time.}

\item{\code{downsampledMajorityRows}}{the total number of the majority rows available for modeling, or null for projects without smart downsampling}

\item{\code{downsampledMinorityRows}}{the total number of the minority rows available for modeling, or null for projects without smart downsampling}

\item{\code{majorityDownsamplingRate}}{the percentage between 0 and 100 of the majority rows that are kept, or null for projects without smart downsampling}

\item{\code{onlyIncludeMonotonicBlueprints}}{whether the project only includes blueprints support enforcing monotonic constraints}

\item{\code{prepareModelForDeployment}}{Prepare model for deployment during Autopilot run. The preparation includes creating reduced feature list models, retraining best model on higher sample size, computing insights and assigning \"RECOMMENDED FOR DEPLOYMENT\" label.}

\item{\code{responseCap}}{defaults to False, if specified used to cap the maximum response of a model}

\item{\code{scoringCodeOnly}}{Keep only models that can be converted to scorable java code during Autopilot run.}

\item{\code{seed}}{defaults to null, the random seed to be used if specified}

\item{\code{shapOnlyMode}}{Keep only models that support SHAP values during Autopilot run. Use SHAP-based insights wherever possible. For pre SHAP-only mode projects this is always ``null``.}

\item{\code{smartDownsampled}}{whether the project uses smart downsampling to throw away excess rows of the majority class.  Smart downsampled projects express all sample percents in terms of percent of minority rows (as opposed to percent of all rows).}

\item{\code{weights}}{the name of the weight column, if specified, otherwise null.}

\item{\code{allowedPairwiseInteractionGroups}}{For GAM models - specify groups of columns for which pairwise interactions will be allowed. E.g. if set to [\link[=\\"A\\", \\"B\\", \\"C\\"]{\"A\", \"B\", \"C\"}, \link[=\\"C\\", \\"D\\"]{\"C\", \"D\"} ] then GAM models will allow interactions between columns AxB, BxC, AxC, CxD. All others (AxD, BxD) will not be considered. If not specified - all possible interactions will be considered by model.}

\item{\code{considerBlendersInRecommendation}}{Include blenders when selecting a model to prepare for deployment in an Autopilot Run.}

\item{\code{eventsCount}}{the name of the event count column, if specified, otherwise null.}

\item{\code{exposure}}{the name of the exposure column, if specified.}

\item{\code{minSecondaryValidationModelCount}}{Compute \"All backtest\" scores (datetime models) or cross validation scores for the specified number of highest ranking models on the Leaderboard, if over the Autopilot default.}

\item{\code{offset}}{the list of names of the offset columns, if specified, otherwise null.}

\item{\code{runLeakageRemovedFeatureList}}{Run Autopilot on Leakage Removed feature list (if exists).}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ProjectAdvancedOptionsResponse-validate"></a>}}
\if{latex}{\out{\hypertarget{method-ProjectAdvancedOptionsResponse-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ProjectAdvancedOptionsResponse$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ProjectAdvancedOptionsResponse-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ProjectAdvancedOptionsResponse-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ProjectAdvancedOptionsResponse$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ProjectAdvancedOptionsResponse-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ProjectAdvancedOptionsResponse-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ProjectAdvancedOptionsResponse$fromJSON(
  ProjectAdvancedOptionsResponseJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ProjectAdvancedOptionsResponseJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ProjectAdvancedOptionsResponse-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ProjectAdvancedOptionsResponse-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ProjectAdvancedOptionsResponse$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
