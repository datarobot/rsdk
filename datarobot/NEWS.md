# datarobot v2.18.4

The `datarobot` package is now dependent on R >= 3.5.

New Features:

* The R client will now output a warning when you attempt to access certain resources (projects, models, deployments, etc.) that are deprecated or disabled by the DataRobot platform migration to Python 3.
* Added support for comprehensive autopilot: use `mode = AutopilotMode.Comprehensive`.

Enhancements:

* The function `RequestFeatureImpact` now accepts a `rowCount` argument, which will change the sample size used for Feature Impact calculations.
* The un-exported function `datarobot:::UploadData` now takes an optional argument `fileName`.
* The function `ListProjects` now accepts `limit` and `offset` arguments, which allows users to retrieve additional projects.

Bugfixes:

* Fixed an issue where a [new feature](https://github.com/jeroen/curl/pull/290/) in curl==5.0.1 caused invocations of `datarobot:::UploadData` (i.e. `SetupProject`) as well as `datarobot:::UploadPredictionDataset` to fail with the error `No method asJSON S3 class: form_file`.
* Loading the `datarobot` package with `suppressPackageStartupMessages()` will now suppress all messages.

API Changes:

* The functions `ListProjects` and `as.data.frame.projectSummaryList` no longer return fields related to recommender models, which were removed in v2.5.0.
* The function `SetTarget` now sets autopilot mode to `Quick` by default. Additionally, when `Quick` is passed, the underlying `/aim` endpoint will no longer be invoked with `Auto`.

Deprecated and Defunct:

* `quickrun` argument is removed from the function `SetTarget`. Users should set `mode = AutopilotMode.Quick` instead.
* Compliance Documentation got deprecated in favor of Automated Documentation API.

Dependency Changes:

* The `datarobot` package is now dependent on R >= 3.5 due to changes in the updated "Introduction to DataRobot" vignette.
* Added dependency on `AmesHousing` package for updated "Introduction to DataRobot" vignette.
* Removed dependency on `MASS` package.
* Client documentation is now explicitly generated with Roxygen2 v7.2.3.

Documentation Changes:

* Updated "Introduction to DataRobot" vignette to use Ames, Iowa housing data instead of Boston housing dataset. 
* Removed hard links to `lendingclub.com` in vignettes due to issues with the CRAN URL checker.

# datarobot v2.18.3

This release is superseded by v2.18.4.

# datarobot v2.18.2

This release fixes the test suite to conditionally use the `stubthat` package, which as of 2022-04-17 is no longer available on CRAN (but is still available in the Microsoft snapshot archive.) This package is listed as a Suggests dependency, and the codebase is updated to reflect this.

Bugfixes:

* Fixed some tests exercising the `BuildPath` helper function.

# datarobot v2.18.1

This release fixes a breaking change in the client around `ListProjects` due to the removal of Spark / H20 models from the DataRobot platform.

Enhancements:

* `GetProject` and `ListProjects` now return all available output from the API.

Bugfixes:

* The enum `ModelCapability` has been properly exported.
* `library(datarobot)` no longer throws an error when executed in an RStudio session.
* `ListProjects()` no longer throws an error about "undefined columns selected."

API Changes:

* Projects no longer reference `scaleoutModelingMode`, `scaleoutMaxTrainPct`, and `scaleoutMaxTrainRows`. These attributes had appeared in the parameters of `setTarget` and `startProject` along with the responses from `ListProjects` and `GetProject`.

Deprecated and Defunct:

* Any references to `scaleoutModelingMode`, `scaleoutMaxTrainPct`, and `scaleoutMaxTrainRows` are now removed.

Dependency Changes:
* Client documentation is now explicitly generated with Roxygen2 v7.1.2.

Documentation Changes:

* This `NEWS` file was renamed to `NEWS.md` and formatted as Markdown.
* Removed references to scaleout.
* Removed invalid reference to DataRobot API docs for `GetDeploymentSettings` and `UpdateDeploymentSettings`.
* Compressed `extdata/Friedman1.csv` and updated vignettes dependent on that dataset.
* Removed `extdata/anomFrame.csv` as it was unused.

# datarobot v2.18.0

This release brings the R Client to parity with DataRobot API v2.18 (DataRobot 5.2), but also includes a number of features from API v2.19 (DataRobot 5.3) as well as Anomaly Assessment, a DataRobot 7.1 feature.

New Features:

* Residuals Chart data for models can be retrieved using `GetLiftCharts` and `GetAllLiftCharts` functions. This is valid only for regression models that are not time-aware.
* Added "Average by Forecast Distance" blender for time series projects configured with more than one Forecast Distance. The blender blends the selected models, selecting the best 3
  models based on the backtesting score for each Forecast Distance and averaging their predictions. The new blender method `FORECAST_DISTANCE_AVG` has been added as `BlendMethod$FORECAST_DISTANCE_AVG`.
* Added functions for tracking the health and status of a deployment. `GetDeploymentServiceStats` retrieves metrics that track deployment utilization and performance, while `GetDeploymentAccuracy` retrieves metrics that track the accuracy of a deployment's predictions. `GetDeploymentServiceStatsOverTime` and `GetDeploymentAccuracyOverTime` will track changes to those metrics over a specified time interval.
* `SubmitActuals` can now be used to submit data about actual results from a deployed model, which can be used to calculate accuracy metrics.
* Projects can be cloned using `CloneProject`. The clone will be post-EDA1 and ready for setting targets and modeling options.
* `CreateCalendar` now supports series-specific events via the `multiSeriesIdColumn` argument. An example of a series-specific event:  some but not all stores being affected by a holiday.
* `GetDeploymentAssociationId` and `UpdateDeploymentAssociationId` can be used to manage a deployment's association ID for use with `SubmitActuals` and the Deployment Accuracy functions.
* `GetDeploymentSettings` can be used to retrieve any and all settings related to a deployed model. `UpdateDeploymentSettings` will allow you to make piecemeal changes as well. The convenience functions `GetDeploymentDriftTrackingSettings` and `GetDeploymentAssociationId` use these methods internally.
* Time series model exports also support prediction intervals: `RequestTransferableModel` now has a `predictionIntervalsSize` parameter.
* Added support for Anomaly Assessment insight. This insight is available for anomaly detection models in time series unsupervised projects which also support calculation of Shapley values. It is possible to:
  * `InitializeAnomalyAssessment` initializes an anomaly assessment insight for the specified subset
  * `ListAnomalyAssessmentRecords` retrieves records
  * `GetAnomalyAssessmentExplanations` retrieves shap explanations
  * `GetAnomalyAssessmentPredictionsPreview` retrieves predictions preview
  * `DeleteAnomalyAssessmentRecord` deletes records

Enhancements:

* Monotonic constraints are now supported for OTV projects. To that end, the parameters monotonicIncreasingFeaturelistId and monotonicDecreasingFeaturelistId can be specified in calls to `RequestNewDatetimeModel`.
* You can now get the model associated with a model job by getting the `modelId` field on the `GetModelJob` or `ListModelJobs` response objects.
* Added the new field `recommendedFeaturelistId` to the `Blueprint` response object. If absent, there is no recommended feature list for this blueprint.
* The `Model` S3 class now exposes the `modelNumber` field. This field is also exposed in the responses to `GetFrozenModel`, `GetDatetimeModel`, `GetBlenderModel`, and `GetRatingTableModel`.
* The method `GetModelCapabilities` has been extended to return `supportsCodeGeneration`, `supportsShap`, and other newly-added capabilities. See `ModelCapability` for more details.
* `GetFeatureInfo` will now return descriptive statistics on *summarized categorical* features in the field `keySummary`.
* `ListDeployments` now supports sorting and searching the results using the new `orderBy` and `search` parameters.
* `GetResidualsChart` and `ListResidualsCharts` are now backwards-compatible with DataRobot 5.2, which does not return rowNumber.
* `GetWordCloud` now includes a `variable` field that represents the source of each ngram, as well as a `class` field that represents values of the target class.
* Performance improvements for `GetPredictions` and `Predict` when retrieving probabilities for large prediction datasets on multiclass projects, i.e. `Predict(irisModel, largeDataset, type = "probability")`
* Unit tests can now be written against testthat edition 3. This is an opt-in feature; all tests are run against edition 2 by default.

Bugfixes:

* Calls to `ListDeployments` will now return more than 20 deployments when available.
* `ListPrimeModels` now returns an empty data frame when the API returns zero results, consistent with its documentation. Previously it would return an empty list. This response is also classed as `dataRobotPrimeModels`.
* `CreateCalendar` now terminates properly when DataRobot is unable to create the calendar. Previously, it would hang due to the R package not checking for the right error response.
* `formatRFC3339Timestamp` now works for vectors of length > 1.

API Changes:

* The first argument of `CreateCalendar` and `CreateRatingTable` is changed from `file` to `dataSource` to reflect that the functions can process data frames as well as CSV files.
* The helper method `ProjectFromAsyncUrl` is replaced with `ProjectFromJobResponse`; this change allowed us to simplify the package's dependency on httr.

Deprecated and Defunct:

* `BlendMethod$FORECAST_DISTANCE` is deprecated and will be removed in 2.19. Use `BlendMethod$FORECAST_DISTANCE_ENET` instead.

Dependency Changes:

* Client documentation is now explicitly generated with Roxygen2 v7.1.1.
* To support new features, `testthat@>3.0.0` and `devtools@>2.4.0` is now required. The test suites are being updated to meet testthat 3e requirements.
* Removed `Suggests: rex` as it is no longer needed for package development.

Documentation Changes:

* This `NEWS` file was renamed to `NEWS.md` and formatted as Markdown.
* Added unit test guidelines for developers to the README.
* Documentation for `GetBlenderModel` and `GetBlenderModelFromJobId` are now more consistent.
* Parameter documentation for `StartAutopilot` and `SetTarget` is clarified.
* Organized some functions into families for easier reference.
* Tweaked documentation related to predictions and time series projects.
* Fixed some spelling mistakes, typos, and Roxygen syntax errors.

# datarobot v2.17.1

* Removed dependency on V8 package by removing code that used the colormap package. The V8 package was flagged by the CRAN maintainers as not building so this removal was necessary to keep the datarobot package on CRAN.
* Removed `curl` from `Imports` since it was causing a NOTE when `devtools::check_win_devel()` was run.

# datarobot v2.17.0

New Features:

* You can now deploy models via the API! Use `CreateDeployment` to create a deployment against a particular prediction server. Use `ListPredictionServers` to list all the available prediction servers. Use `GetDeployment` and `ListDeployments` to see particular deployments that you have. You can delete a deployment with `DeleteDeployment`.
* The model backing a deployment can be replaced with `ReplaceDeployedModel`. Use `ValidateReplaceDeployedModel` first to test that the deployment replacement is valid, if desired.
* Deployments support drift tracking. Use `GetDeploymentDriftTrackingSettings` to get drift tracking settings for a deployment. You can update the drift tracking using `UpdateDeploymentDriftTrackingSettings`.
* Information on feature clustering and the association strength between pairs of numeric or categorical features is now available with `GetFeatureAssociationMatrix`. Relative pairwise feature association statistics can be retrieved with `GetFeatureAssociationMatrixDetails`.
* Multiple feature type transformations can now be executed in a single batch request using `BatchFeaturesTypeTransform`.

Enhancements:

* You can now use `doNotDerive` in the `featureSettings` of `CreateDatetimePartition` to  disable DataRobot's automatic time series feature engineering for a particular feature (e.g., so you can derive lags yourself manually).
* Users can now embed DataRobot-generated content in compliance doc templates (see `UploadComplianceDocTemplate`) using keyword tags.
* Prediction intervals are now supported for start-end retrained models in a time series project.
* Previously, all backtests had to be run before prediction intervals for a time series project could be requested with predictions. Now, backtests will be computed automatically if needed when prediction intervals are requested.

Bugfixes:

* Calls to `GetPredictionExplanationsRowsAsDataFrame` previously did not work with numeric labels. This has been fixed.

API Changes:

Deprecated and Defunct:

* The `defaultToAPriori` parameter in `CreateDatetimePartitionSpecification` has been renamed to `defaultToKnownInAdvance`. `defaultToAPriori` has now been fully removed.
* The `aPriori` flag in the `featureSettings` parameter in  `CreateDatetimePartitionSpecification` as been renamed to `knownInAdvance`. `aPriori` has now been fully removed.
* The deprecated `SetupProjectFromMySQL`, `SetupProjectFromOracle` and `SetupProjectFromPostgreSQL` have now been removed. Use `SetupProjectFromDataSource` instead.
* `GetTransferrableModel`, `ListTransferrableModels`, `UpdateTransferrableModel`, `DeleteTransferrableModel`, `DownloadTransferrableModel`, and `UploadTransferrableModel` have  been removed and replaced with their correctly spelled counterparts (`GetTransferableModel`, `ListTransferableModels`, `UpdateTransferableModel`, `DeleteTransferableModel`, `DownloadTransferableModel`, and `UploadTransferableModel`).

Dependency Changes:

Documentation Changes:

# datarobot v2.16.0

New Features:

* You can now retrieve series accuracy information, showing accuracy metrics for each series for a multiseries project. Use `GetSeriesAccuracy` to retrieve the accuracy. You can also download it as a CSV with `DownloadSeriesAccuracy`.

Enhancements:

* Prediction intervals can now be returned for predictions with datetime models. Use `includePredictionIntervals = TRUE` in calls to `Predict`. For each model, prediction intervals estimate the range of values DataRobot expects actual values of the target to fall within. They are similar to a confidence interval of a prediction, but are based on the residual errors measured during the backtesting for the selected model.
* `ListPredictions` now returns metadata on prediction intervals. `includesPredictionIntervals` is TRUE if there are prediction intervals in the predictions and `FALSE` otherwise. `predictionIntervals` specifies the size (in percent) of intervals or is `NULL` if there are no intervals.
* For time series projects, the effective Feature Derivation Window, specifying the full span of historical data required at predict time, is now available through the API. It may be longer than the feature derivation window of the project depending on the differencing settings used.
* More of the project partitioning settings are also available in the metadata for datetime models (see `GetDatetimeModel`). The new attributes are `effectiveFeatureDerivationWindowStart`, `effectiveFeatureDerivationWindowEnd`, `forecastWindowStart`, `forecastWindowEnd`, and `windowsBasisUnit`.
* `DownloadComplianceDocumentation` and `GetSeriesAccuracy` now support a `maxWait` parameter to customize the amount of time to wait before raising a timeout error.

Deprecated and Defunct:

* `RecommendedModelType$Recommended` type for `GetModelRecommendation` and `GetRecommendedModel` has been removed and replaced with `RecommendedModelType$RecommendedForDeployment`.

Documentation Changes:

* Fixed more spelling mistakes in the documentation.

# datarobot v2.15.0

New Features:

* Advanced tuning can now be done on any model. See `StartTuningSession` for details.
* DataRobot time series now supports calendar files, which allow specifying special events like holidays. See `CreateCalendar`, `GetCalendar`, `ListCalendars`, `UpdateCalendar`, and `DeleteCalendar`.
* Projects now can be shared with other users. See `Share` for details.
* Calendars can be shared with other users. See `Share` for details.

Enhancements:

* `UploadPredictionDataset` and `UploadPredictionDatasetFromDataSource` will now return `dataQualityWarnings` that mention any potential problems with the uploaded dataset.
* `UploadPredictionDataset` and `UploadPredictionDatasetFromDataSource` now have a parameter `relaxKIAFeaturesCheck`. If `TRUE`, uploaded datasets for time series projects will allow missing values for the Known in Advance features in the forecast window at prediction time.
* ROC Curve information retrieval has been extended to contain four new fields (`fractionPredictedAsPositive`, `fractionPredictedAsNegative`, `liftPositive`, and `liftNegative`) with cumulative gains and lift data.
* Added Forecast Distance blender for time series projects configured with more than one Forecast Distance. It blends the selected models creating separate linear models for each Forecast Distance.
* Added a `filter` option to `ListProjects` that supports filtering retrieval of project lists by name using the `projectName` filter.
* `GetCalendarFromProject` can be used to get the calendar associated with a project.
* Data source objects can now be used in `StartProject` to quickly create a project from a data source.
* Data source objects can now be used in addition to data source IDs in `SetupProjectFromDataSource`.
* `crossSeriesGroupByColumns` has been added to datetime partitioning to allow users the ability to indicate how to further split series in to related groups.
* The prediction explanations workflow is now ~3x faster for most use cases.

Bugfixes:

* Time series `windowBasisUnit` has been renamed to the correct `windowsBasisUnit`.

Deprecated and Defunct:

* Reason codes have been renamed to Prediction Explanations to provide increased clarity and accessibility. `DeleteReasonCodes`, `DeleteReasonCodesInitialization`, `DownloadReasonCodes`, `GetAllReasonCodesRowsAsDataFrame`, `GetReasonCodesInitialization`, `GetReasonCodesInitializationFromJobId`, `GetReasonCodesMetadata`, `GetReasonCodesMetadataFromJobId`, `GetReasonCodesRows`, `ListReasonCodesMetadata`, `RequestReasonCodes`, and `RequestReasonCodesInitialization` have all been removed and replaced with appropriately renamed functions and a new workflow. See `GetPredictionExplanations` for more.
* `SetupProjectFromMySQL`, `SetupProjectFromOracle`, `SetupProjectFromPostgreSQL`, `SetupProjectFromHDFS` are now deprecated. They will be removed in v2.17. Use `SetupProjectFromDataSource` instead.
* `RequestPredictionsForDataset` has been renamed to `RequestPredictions`. The original `RequestPredictionsForDataset` has been removed.
* `GetDatetimeModelObject` has been renamed to `GetDatetimeModel`. The original `GetDatetimeModelObject` has been removed.
* The `defaultToAPriori` parameter in `CreateDatetimePartitionSpecification` has been renamed to `defaultToKnownInAdvance`. `defaultToAPriori` is now removed.
* The `aPriori` flag in the `featureSettings` parameter in  `CreateDatetimePartitionSpecification` as been renamed to `knownInAdvance`. `aPriori` is now removed.
* `GetTransferrableModel`, `ListTransferrableModels`, `UpdateTransferrableModel`, `DeleteTransferrableModel`, `DownloadTransferrableModel`, and `UploadTransferrableModel` have all been deprecated and replaced with their correctly spelled counterparts (`GetTransferableModel`, `ListTransferableModels`, `UpdateTransferableModel`, `DeleteTransferableModel`, `DownloadTransferableModel`, and `UploadTransferableModel`). The misspelled versions will be removed in v2.17.
* Support for numeric modes in `StartAutopilot` has now been fully removed.

Dependency Changes:

* To support new features, `curl` at version 2.7 or higher is now required.

Documentation Changes:

* Fixed more spelling mistakes in the documentation.

# datarobot v2.14.2

Enhancements:

* Training predictions for multiseries projects will now return the `SeriesID`, `forecastPoint`, and `forecastDistance`.
* `GetDatetimePartition` now returns `isCrossSeries` to indicate whether the datetime partition uses cross-series features.
* `ScoreBacktests` now accepts a parameter `wait = TRUE` to wait for job completion.
* `Predict` and `GetPredictions` no longer return `positiveProbability` for non-binary problems.
* `Predict` and `GetPredictions` no longer return `seriesId` for non-multiseries problems.

Documentation Changes:

* Fixed a typo in how `knownInAdvance` was defined in the `featureSettings` in the "time series" vignette.

# datarobot v2.14.1

Bugfixes:

* Requesting a multiseries project now will work even if the Series ID cannot be automatically inferred by DataRobot.

# datarobot v2.14.0

New Features:

* `DownloadComplianceDocumentation` can be used to download compliance documentation. Compliance documentation also can be created with default or custom templates - use `GetComplianceDocTemplate` to get particular templates and `UploadComplianceDocTemplate` to use your own. See the vignette on "Compliance Documentation" for more information.
* Data sources and data stores can now be shared with other users. Use `Share` to share a data source or data store. Use `ListSharingAccess` to see current access rights. Use `UpdateAccess` for more complex access right modification operations.
* Multiseries projects can now include derived cross series features. Use `useCrossSeries = TRUE` in `CreateDatetimePartitionSpecification` to enable.
* You can now get a feature histogram (a histogram of feature counts and target distribution over bins of values for a particular feature) using `GetFeatureHistogram`.
* Get supported capabilities for a model using `GetModelCapabilities`.

Enhancements:

* Data sources and data stores can be passed into functions directly in addition to being passed as IDs.
* Binary classification for time series is now supported as a project type.
* Calls to `StartProject` and `UpdateProject` that up the worker count can now set the worker count to `"max"`, which uses the maximum available number of workers.
* `fallbackToParentInsights` is now available as a parameter on all insights functions (`GetRocCurve`, `ListRocCurves`, `GetLiftChart`, `ListLiftCharts`, `GetConfusionChart`, `ListConfusionCharts`). When `TRUE`, a frozen model with missing insights will attempt to retrieve the missing insight data from its parent model.
* Time series partitions can now have the forecast window and feature derivation windows defined in a number of rows by using `windowBasisUnit` and setting it to `"ROW"`.
* Time series partitions can now be defined in millisecond intervals.
* Training predictions for datetime partitioned projects now support the new data subset `DataSubset$AllBacktests` for requesting the predictions for all backtest validation folds.
* Training predictions for datetime partitioned projects now return the relevant timestamp associated with the prediction.

Bugfixes:

* In cases where a request would return hundreds of responses, sometimes not all those responses would be returned due to improper pagination. This has now been fixed.
* Variables can now be correctly used as tuning parameters for `StartTuningSession`.
* If you use `StartProject` without defining a project name, it now correctly uses the name of the variable passed (like `SetupProject`) rather than erroneously just calling it "dataSource".

Deprecated and Defunct:

* `GetAllLiftCharts` has now been removed (use `ListLiftCharts` instead).
* `GetModelJobs` has now been removed (use `ListModelJobs` instead).
* `GetProjectList` has now been removed (use `ListProjects` instead).
* `GetAllRocCurves` has now been removed (use `ListRocCurves` instead).
* `RecommendedModelType$Recommended` type for `GetModelRecommendation` and `GetRecommendedModel` has been deprecated and replaced with `RecommendedModelType$RecommendedForDeployment`. It will be removed in v2.16.
* `PeriodicityTimeUnits` has been renamed to `TimeUnits`. `PeriodicityTimeUnits` still exists for backwards compatibility.

Documentation Changes:

* Added more documentation for various enums.
* The use of `Predict` in the "Introduction to DataRobot" vignette was previously inaccurate. It has been fixed.
* `RecommendedModelType` and `GetModelRecommendation` now have more documentation about the model recommendation process.
* Vignettes have been updated to use `StartProject` throughout.
* The intro vignette has been cleaned up and now has an example of using feature impact.
* Data used in vignettes has now been added to package data. Broken references to data have been fixed.

# datarobot v2.13.0

New Features:

* An API for advanced tuning is now available, which allows you to manually set model parameters and override the DataRobot default selections. You can get information on available model hyperparameters via `GetTuningParameters` and start a tuning session with `StartTuningSession`. You can interactively iterate through all the parameters for a model using `RunInteractiveTuning`. These advanced tuning features are currently generally available for Eureqa models. To use this feature with other model types, contact your CFDS for more information.
* `Predict` can now be used to create predictions directly from a model and a test dataset, bypassing the need to `UploadPredictionDataset`, `RequestPredictions`, and `GetPredictions`.
* `ListPredictions` can be used to summarize all the different predictions available for a particular project, model, and/or prediction dataset.
* `GetPredictionExplanations` now supports a single workflow to get prediction explanations (previously called reason codes, see "Deprecated and Defunct") for a model and a dataset without needing all the various intermediary steps.
* `GetPredictionDataset` can be used to get metadata on a particular prediction dataset.
* The prediction threshold for binary classification models can now be changed via `SetPredictionThreshold`.
* `GetFeatureImpact` works like `GetFeatureImpactForModel`, but will also request the feature impact if it has not already been requested.
* `GetTrainingPredictionsForModel` retrieves training predictions for a given model object, requesting them in the process.
* Models can now be starred, which highlights them. `StarModel` will star a model. `UnstarModel` will unstar it. `ToggleStarForModel` will toggle the star status. `ListStarredModels` will list all the starred models for a particular project. Model objects will also have an `isStarred` parameter returned to tell whether they are starred or not. (All models are unstarred by default.)
* `DeleteFeaturelist` and `DeleteModelingFeaturelist` can now be used to delete featurelists and modeling featurelists respectively.
* `UpdateFeaturelist` can be used to change the name and description of a featurelist. `UpdateModelingFeaturelist` works for modeling featurelists as well.

Enhancements:

* Using `type = "raw"` in `GetPredictions` (or `Predict`) will return the raw dataframe of predictions metadata.
* `ListModels` now can take an `orderBy` parameter to sort the output list by `metric` or `samplePct`.
* `ListModels` now can take a `filter` parameter to filter output by `samplePct`, `name`, and/or `isStarred`.
* `StartProject` and `SetupProject` can now work without a `projectName`
* `StartProject` can now take a `workerCount` parameter to set the worker count for the project.
* `StartProject` can now take `wait = TRUE` to automatically wait for the autopilot to complete (thus making an explicit call to `WaitForAutopilot` unnecessary).
* `ProjectStage` can now be used to get a list of the available project stages.
* It is now no longer necessary to call `RequestMultiSeriesDetection` manually for a multiseries project.
* Feature impact now returns not only the impact score for the features but also whether they were detected to be redundant with other high-impact features.
* Jobs now report a parameter called `isBlocked` that specifies whether a job is blocked from execution because one or more dependencies have not yet been met.
* `ListModelJobs` now returns the `trainingRowCount` key.
* `GetPredictions` now can get predictions using a projectId and a predictionId (see `ListPredictions`) in addition to its prior ability to retrieve predictions using a `predictionJobId`.
* Featurelists (see `GetFeaturelist` and `GetModelingFeaturelist`) now return a `created` value with the timestamp, a `isUserCreated` value explaining whether or not the feature was created by a user (as opposed to DataRobot automation), `numModels` showing how many models use the featurelist, and `description` which gives a text description of the featurelist.
* Prediction datasets are now `dataRobotPredictionDataset` class in addition to being `list` class.
* `GetPredictions` now can get predictions using a projectId and a predictionId (see `ListPredictions`) in addition to its prior ability to retrieve predictions using a `predictionJobId`.
* Featurelists (see `GetFeaturelist` and `GetModelingFeaturelist`) now return a `created` value with the timestamp, a `isUserCreated` value explaining whether or not the feature was created by a user (as opposed to DataRobot automation), `numModels` showing how many models use the featurelist, and `description` which gives a text description of the featurelist.
* `GetDatetimePartition` now reports information on the number of "known in advance" features.
* `GetDatetimePartition` reports if the partition was drawn from a time series project and/or a multiseries project.

Bugfixes:

* In rare instances, requests would fail due to not being able to properly create authentication headers. This has now been resolved.

API Changes:

* `RequestMultiSeriesDetection` (which is now no longer necessary to invoke directly) now blocks until the multiseries request is complete and returns details about which multiseries columns were detected.

Deprecated and Defunct:

* Reason codes have been renamed to Prediction Explanations. `DeleteReasonCodes`, `DeleteReasonCodesInitialization`, `DownloadReasonCodes`, `GetAllReasonCodesRowsAsDataFrame`, `GetReasonCodesInitialization`, `GetReasonCodesInitializationFromJobId`, `GetReasonCodesMetadata`, `GetReasonCodesMetadataFromJobId`, `GetReasonCodesRows`, `ListReasonCodesMetadata`, `RequestReasonCodes`, and `RequestReasonCodesInitialization` have all been deprecated (and will be removed in v2.15). These functions have been replaced with appropriately renamed functions and a new workflow. See `GetPredictionExplanations` for more.
* RequestPredictionsForDataset is replaced by RequestPredictions and deprecated (and will be removed in v2.15).
* `GetDatetimeModelObject` has been renamed to `GetDatetimeModel`. The original `GetDatetimeModelObject` has been deprecated and will be removed in v2.15.

Documentation Changes:

* Added a vignette explaining the advanced tuning interfaces.
* Vignettes have been updated to use the new `Predict` workflow.
* The vignette on time series and multiseries has been expanded to include more useful information.
* Vignettes now use `GetRecommendedModel` instead of calculating the best mode instead of calculating the best model.
* Corrected various typos in docstrings.

# datarobot v2.12.1

Enhancements:

* `plot` on ListOfModels returns an error if the desired percent passed to `pct` is not found.

Bugfixes:

* Fix `as.data.frame` to handle missing featurelist IDs when `simple = FALSE`.
* Fix `as.data.frame` to handle a list of prediction datasets.
* Fix `as.data.frame` to handle a list of models when `samplePct` is not set.
* Fix `plot` on ListOfModels to work with missing featurelist IDs.
* Fix summary methods to work on zero-length lists.

Deprecated and Defunct:

* Passing a non-logical value to `simple` parameter in `as.data.frame` now produces an error instead of a warning.

# datarobot v2.12.0

New Features:

* A new shorthand, `StartProject`, combines both `SetupProject` and `SetTarget` into one function.
* A report on how a model handles missing values for all features in the project can now be retrieved using `GetMissingValuesReport`.
* You can now use `UploadPredictionDatasetFromDataSource` to create a prediction dataset from a DataRobot data source (introduced in v2.11).

Enhancements:

* If you have enabled monotonic constraints for your project, you can now disable these constraints for training a new model. You do this by using `RequestNewModel` and passing `""` (an empty string) as the value for each monotonic constraint featurelist you wish to override.

Bugfixes:

* In v2.10 and v2.11, the `AutopilotMode$Quick` "mode" for `SetTarget` had been broken and no longer triggered the quick mode (instead it ran the full autopilot). This has been fixed.

Deprecated and Defunct:

* GetProjectList is replaced by ListProjects and deprecated (and will be removed in v2.14).
* GetAllRocCurves is replaced by ListRocCurves and deprecated (and will be removed in v2.14).
* GetAllLiftCharts is replaced by ListLiftCharts and deprecated (and will be removed in v2.14).
* GetModelJobs is replaced by ListModelJobs and deprecated (and will be removed in v2.14).

Dependency Changes:

* To support new features, `httr` at version 1.2.0 or higher is now required.

# datarobot v2.11.0

New Features:

* DataRobot now recommends particular models. `ListModelRecommendations` has been added to get all the model recommendations, `GetModelRecommendation` can return a particular recommendation, and `GetRecommendedModel` returns the particular model object corresponding with a particular recommendation.
* DataRobot now supports "Database Connectivity", allowing databases to be used as the source of data for projects and prediction datasets. The feature works on top of the JDBC standard, so a variety of databases conforming to that standard are available; a list of databases with tested support for DataRobot is available in the user guide in the web application. See `ListDrivers` and `GetDriver` to get available drivers, `CreateDataStore` to create a data store from a driver, and `CreateDataSource` to create a data source from a data store.
* You can also create a project from a specified data source using `SetupProjectFromDataSource`.
* Time series projects support multiseries as well as single series data. See the vignette on time series for details.
* `GetTimeSeriesFeatureDerivationLog` can now be used to retrieve a lot of information on details for derived features for time series projects. `DownloadTimeSeriesFeatureDerivationLog` can download the log to a text document.

Enhancements:

* `GetFeatureInfo` and `ListFeatureInfo` now report `targetLeakage`, specifying whether a feature is considered to have target leakage or not.
* Added a helper method to easily cross validate a model. Just call `CrossValidateModel` on your model object.
* `ConnectToDataRobot` now works with environment variables. Set `DATAROBOT_API_ENDPOINT` and `DATAROBOT_API_TOKEN` to connect to DataRobot. Note that previously the R client unofficially used `DataRobot_URL` and `DataRobot_Token` as environment variables to facilitate connecting to DataRobot, but these variables are now no longer supported.

Bugfixes:

* Fix `as.data.frame` to handle missing featurelist IDs.

API Changes:

* New parameters predictionsStartDate and predictionsEndDate added to `UploadPredictionDataset` to support bulk predictions upload for time series projects.

# datarobot v2.10.0

Bugfixes:

* The Model Deployment interface which was previously visible in the client has been removed to allow the interface to mature.
* Fix `as.data.frame` to handle multiple featurelists.
* Clarified the time series workflow in the time series vignette.
* Fix `partitionKeyCols` parameter in `CreateGroupPartition` to more clearly error if more than one partition key is passed.
* Formatting within vignettes has been cleaned and standardized.

Deprecated and Defunct:

* The following were deprecated and have now been removed: the `quickrun` parameter on SetTarget, the ability to use `GetFeatureInfo` with feature IDs, the `GetRecommendedBlueprints` function, `GetModelObject`, `GetAllModels`, `GetBlueprintDocuments`, and the `RequestPredictions` function.
* The `defaultToAPriori` parameter in `CreateDatetimePartitionSpecification` is being deprecated and has been renamed to `defaultToKnownInAdvance`. `defaultToAPriori` will be fully removed in v2.15.
* The `aPriori` flag in the `featureSettings` parameter in  `CreateDatetimePartitionSpecification` is being deprecated and has been renamed to `knownInAdvance`. The `aPriori` will be fully removed in v2.15.

# datarobot v2.9.0

New features:

* Models can now be deployed to dedicated prediction servers using the new model monitoring system via the API. Create a deployment via `RequestModelDeployment`, get information on a specific deployment using `GetModelDeployment`, and list information on all deployments across all projects via `ListModelDeployments`. You can also get more information on the service health of a particular deployment using `GetModelDeploymentServiceStatistics` or get the action log for a deployed model using `GetModelDeploymentActionLog`.

Enhancements:

* DataRobot API now supports creating 3 new blender types - Random Forest, TensorFlow, LightGBM.
* Multiclass projects now support blenders creation for 3 new blender types as well as Average and ENET blenders.
* New attributes `maxTrainRows`, `scaleoutMaxTrainPct`, and `scaleoutMaxTrainRows` have been added to projects retrieved by `GetProject`. `maxTrainRows` specified the equivalent value to the existing `maxTrainPct` as a row count. The scaleout fields can be used to see how far scaleout models can be trained on projects, which for projects taking advantage of scalable ingest may exceed the limits on the data available to non-scaleout blueprints.
* Models can be trained by requesting a particular row count using the new `trainingRowCount` argument, specifying a desired amount of rows instead of a desired percentage of the dataset (via the current `samplePct` parameter).  Specifying model size by row count is recommended when the float precision of sample_pct could be problematic, e.g. when training on a small percentage of the dataset or when training up to partition boundaries. This new approach is available for `RequestNewModel`, `RequestFrozenModel`, and `RequestSampleSizeUpdate`. `RequestFrozenDatetimeModel` already had this feature.
* `GetPredictions` now returns a more informative error message when the async service times out.
* Individual features can now be marked as a priori or not a priori using the new `featureSettings` attribute when setting the target or specifying datetime partitioning settings on time series projects. Any features not specified in the `featureSettings` parameter will be assigned according to the `defaultToAPriori` value.
* Three new options have been made available in the `DatetimePartitioningSpecification` to fine-tune how time-series projects derive modeling features. `treatAsExponential` can control whether data is analyzed as an exponential trend and transformations like log-transform are applied. `differencingMethod` can control which differencing method to use for stationary data. `periodicities` can be used to specify periodicities occurring within the data.  All are optional and defaults will be chosen automatically if they are unspecified.
* An error is now raised if you do not pass a valid partition to partitioning in `SetTarget`.

Bugfixes:

* Fixed latency issues in `UploadPredictionDataset` and `GetPredictions`. These functions have now been fully tested to handle data up to 1GB, and likely can handle more than that. If you run into issues, try incrementing the `maxWait` parameter.
* You can now set `ssl_verify: FALSE` in `drconfig.yaml` to not verify SSL when connecting with DataRobot.
* Fixed a typo in the training predictions vignette. It previously read `DownloadRatingTable` when it meant to read `DownloadTrainingPredictions`.
* Fixed a typo in the reason codes docstring examples. It previously read `reasonCodeId <- GetReasonCodesMetadataFromJobId(projectId, jobId)` when it should read `reasonCodeId <- GetReasonCodesMetadataFromJobId(projectId, jobId)$id`.

API Changes:

* Now `trainingRowCount` is available on non-datetime models as well as "rowCount" based datetime models. It reports the number of rows used to train the model (equivalent to `samplePct`).
* Added support for retrieving details about the Pareto Front for a Eureqa model. Use `GetParetoFront` to get the Pareto Front details and `AddEureqaSolution` to add a new solution to the leaderboard.

# datarobot v2.8.0

New features:

* A new premium feature, time series, is now available. New projects can be created as time series projects which automatically derive features from past data and forecast the future. See the time series documentation in the web app for more information.
* The DataRobot API supports the creation, training, and predicting of multiclass classification projects. DataRobot, by default, handles a dataset with a numeric target column as regression.  If your data has a numeric cardinality of up to 10 classes, you can override this behavior to instead create a multiclass classification project from the data. To do so, use the `SetTarget` function, setting `targetType = TargetType$Multiclass`. If DataRobot recognizes your data as categorical, and it has up to 10 classes, using multiclass will create a project that classifies which label the data belongs to.
* With the introduction of Multiclass Classification projects, DataRobot needed a better way to explain the performance of a multiclass model so we created a new Confusion Chart. The API now supports retrieving and interacting with confusion charts.
* `GetFeatureInfo` and `ListFeatureInfo` now return the EDA summary statistics (i.e., mean, median, minum, maximum, and standard deviation) for features where this is available (e.g., numeric, date, time, currency, and length features). These summary statistics will be formatted in the same format as the data it summarizes.
* The DataRobot API now includes Rating Tables. A rating table is an exportable CSV representation of a model. Users can influence predictions by modifying them and creating a new model with the modified table.
* You can now set `scaleoutModelingMode` when setting a project target. It can be used to control whether scaleout models appear in the autopilot and/or available blueprints. Scaleout models are only supported in the Hadoop enviroment with the corresponding user permission set.
* You can now set `accuracyOptimizedBlueprints` when setting a project target. Accuracy optimized blueprints are longer running model blueprints that provide increased accuracy over the normal blueprints that run during autopilot.
* DataRobot now supports retrieving model blueprint charts via `GetModelBlueprintChart` and model blueprint documentation via `GetModelBlueprintDocumentation`. These are like regular blueprint charts and blueprint documentation, except for model blueprints, which are a reduced representation of the blueprint run by the model to only include the relevant branches actually executed by the model.
* The Datarobot API now supports generating and retrieving training predictions, which are predictions made by the model on out-of-fold training data. Users can start a job which will make training predictions and retrieve them. See the training predictions documentation in the web app for more information on how to use training predictions.

Enhancements:

* `CreateDatetimePartitionSpecification` now includes the optional `disableHoldout` flag that can be used to disable the holdout fold when creating a project with datetime partitioning.
* The advanced options available when setting the target have been extended to include the new parameters `offset` and `exposure` to allow specifying offset and exposure columns to apply to predictions generated by models within the project. See the user guide documentation in the web app for more information on offset and exposure columns.
* The advanced options available when setting the target have been extended to include the new parameter `eventsCount` to allow specifying the events count column. See the user guide documentation in the webapp for more information on events count.
* File URIs can now be used as sourcedata when creating a project or uploading a prediction dataset. The file URI must refer to an allowed location on the server, which is configured as described in the user guide documentation.
* If this package is used in RStudio v1.1 or higher, it is possible to use the RStudio Connections UI to open a DataRobot connection.
* When retrieving reason codes on a project using an exposure column, predictions that are adjusted for exposure can be retrieved.
* `ConnectToDataRobot` now supports an option `sslVerify` that turns off SSL verification if set to FALSE.

Bugfixes:

* Fixes a bug that prevented `GetReasonCodesMetadataFromJobId` from being called with a project directly (instead of a project id).
* Fixes a bug that prevented `RequestNewModel` from being called when `options(stringsAsFactors = TRUE)` is set.
* Fixes a bug that prevented more than one blueprint document from being returned by `GetBlueprintDocuments` (now named `GetBlueprintDocumentation`).

Deprecated and Defunct:

* The quickrun parameter on SetTarget, the ability to use GetFeatureInfo with feature IDs, the `GetRecommendedBlueprints` function, and the `RequestPredictions` function were all originally planned to be deprecated in version 3.0. These features and functions will now be deprecated in v2.10 instead.
* GetModelObject is replaced by GetModel and deprecated (and will be removed in v2.10).
* GetAllModels is replaced by ListModels and deprecated (and will be removed in v2.10).
* `GetBlueprintDocuments` is replaced by `GetBlueprintDocumentation` and deprecated (and will be removed in v2.10).

# datarobot v2.7.1

Documentation Changes:

* The `modelwordcloud` package is now available on CRAN, so the documentation has been updated to reflect CRAN installation instructions.

# datarobot v2.7.0

New features:

* Word cloud data for text processing models can be retrieved using `GetWordCloud` function.
* Scoring code JAR file can be downloaded for models supporting code generation using 'DownloadScoringCode` function.
* Lift Chart data can be retrieved using `GetLiftCharts` and `GetAllLiftCharts` function.
* Roc Curve data for binary classification projects can be retrieved using `GetRocCurve` and `GetAllRocCurves`
* Status and information about individual jobs can be retrieved using `GetPredictJob',`GetModelJob`,`GetJob` functions.  Any job can be retrieve via `GetJob` which is less specific.  Only prediction jobs can be retrieved with `GetPredictJob` and only modeling jobs can be retrieved with `GetModelJob`.

Enhancements:

* `GetModelParameters` now includes an additional key showing the coefficients for individual stages of multistage models (e.g. Frequency-Severity models).
* When training a `DatetimeModel` on a window of data, a `timeWindowSamplePct` can be specified to take a uniform random sample of the training data instead of using all data within the window.

Bugfixes:

* Fixed a bug where depending on what version of the R curl library was installed, the client could hang after requesting certain DataRobot jobs.
* DownloadTransferrableModel now correctly handles HTTP errors.

Dependency Changes:

* To support new features, `jsonlite` at version 1.0 or higher and `curl` at version 1.1 or higher are now required.

Deprecated and Defunct:

* Semi-automatic autopilot mode is removed. Quick or manual mode can be used instead to get a sparser autopilot.

# datarobot v2.6.0

New features:

* Function CreateDerivedFeatureIntAsCategorical has been added. It creates new categorical feature based on parent numerical feature while truncating numerical values to integer. (All of the data in the column should be considered categorical in its string form when cast to an int by truncation. For example the value ``3`` will be cast as the string ``3`` and the value ``3.14`` will also be cast as the string ``3``. Further, the value ``-3.6`` will become the string ``-3``. Missing values will still be recognized as missing.)
* Reason Codes, a new feature in DataRobot, is fully supported in the package through several new functions.
* Functions which allow to access blueprint chart and documentation have been added.
* Model parameters can now be retrieved using GetModelParameters function.
* A new partitioning method (datetime partitioning) has been added. The recommended workflow is to preview the partitioning by creating a `DatetimePartitioningSpecification` using CreateDatetimePartition and CreateBacktestSpecification function and passing it into GenerateDatetimePartition, inspect the results and adjust as needed for the specific project dataset by adjusting the `DatetimePartitioningSpecification` and re-generating, and then set the target by passing the final `DatetimePartitioningSpecification` object to the partitioning_method parameter of SetTarget.

Enhancements:

* The default value of the maxWait parameter used to control how long asynchronous routes are polled has been changed from 1 minute to 10 minutes.

API Changes:

* projectId has been added to Feature schema
* The UnpauseQueue function will not longer set the autopilot mode of a project to full autopilot.
  This means that projects using the (deprecated) SemiAuto autopilot mode will require the autopilot
  to be advanced via the webapp.

# datarobot v2.5.0

New features:

* Functions RequestFrozenModel, GetFrozenModel, GetFrozenModelFromJobId have been added. They allow user to create model with the same tuning parameters as parent model but with different data sample size and get information about frozen models in a project.
* Functions RequestBlender, GetBlenderModelFromJobId, GetBlenderModel have been added. They allow user to create blender models and get information about blender models in a project.
* Projects created via the API can now use smart downsampling when setting the target by passing smartDownsampled and majorityDownsamplingRate into the SetTarget function.

Enhancements:

* Meaningful error messages have been added when the DataRobot endpoint is incorrectly specified in a way that causes redirects (e.g. specifying http for an https endpoint).
* Previously it was not possible to use user partition columns with cross-validation without
  specifying a holdout level using the API. This can now be be done by either omitting the
  cvHoldoutLevel parameter or providing it as `NA`.

Bugfixes:

API Changes:

Deprecated and Defunct:

* Support for recommender models has been removed from the DataRobot API. The package has been updated to remove functionality that formerly used this feature.

Documentation Changes:

# datarobot v2.4.0

New features:

* The premium feature DataRobot Prime has been added. You can now approximate a model on the leaderboard and download executable code for it. Talk to your account representative if the feature is not available on your account. The new related functions are GetPrimeEligibility, RequestApproximation, ListPrimeModels, GetPrimeModel, GetRulesets, RequestPrimeModel, GetPrimeModelFromJobId, CreatePrimeCode, GetPrimeFileFromJobid, ListPrimeFiles, GetPrimeFile, DownloadPrimeCode
* A utility function, WaitForJobToComplete, has been added. It will block until the specified job finishes, or raise an error if it does not finish within a specified timeout.
* Functions SetupProjectFromMySQL, SetupProjectFromOracle, SetupProjectFromPostgreSQL and SetupProjectFromHDFS have been added. They allow user to create DataRobot projects from MySQL, Oracle, PostgreSQL and HDFS data sources.
* Functions RequestTransferrrableModel, DownloadTransferrableModel, UploadTransferrableModel, GetTransferrrableModel, ListTransferrrableModels, UpdateTransferrrableModel, DeleteTransferrrableModel have been added. They allow user to download models from modeling server and transfer them to special dedicated prediction server (those functions are only useful to users with on-premise environment)

Enhancements:

* An optional maxWait parameter has been added to GetModelFromJobId and GetFeatureImpactForJobId, to allow users to specify an amount of time to wait for the job to complete other than the default 60 seconds.
* Projects can now be run in quickrun mode (which skips some autopilot stages and longer-running models) by passing "quick" as the mode parameter, in the same way "auto" and "manual" modes can be specified.
* The client will now check the API version offered by the server specified in configuration, and  a warning if the client version is newer than the server version. The DataRobot server is always backwards compatible with old clients, but new clients may have functionality that is not implemented on older server versions. This issue mainly affects users with on-premise deployments of DataRobot.
* SetupProject and UploadPredictionDataset accept url as dataSource parameter now

Bugfixes:

* If a model job errors, GetModelFromJobId will now immediately raise an exception, rather than
  waiting for the timeout.
* The maxWait parameter on UploadPredictionDataset will now be correctly applied.

API Changes:

Deprecated and Defunct:

* The quickrun parameter on SetTarget is deprecated (and will be removed in 3.0). Pass "quick" as the mode parameter instead.

Documentation Changes:

# datarobot v2.3.0

Enhancements:

* When project creation using SetupProject times out, the error message now includes a URL to use with
  the new ProjectFromAsyncUrl function to resume waiting for the project creation.
* GetFeatureInfo now supports retrieving features by feature name. (For backwards compatibility,
  feature IDs are still supported until 3.0.)
* The package no longer relies on a particular version of the methods package. (This dependency was too strict and
  required some users to unnecessarily upgrade R.)
* The projectName argument of SetupProject no longer defaults to the string 'None'. (The new default is not
  to send a name, which results in the name 'Untitled Project'.)
* The maxWait argument for SetupProject now controls the timeout for the initial POST request and has a larger
  default value. The reason for this is that for large project creation file uploads, the server may take a
  longer-than-normal amount of time to respond, and waiting longer than the default timeout may be necessary.

Deprecated and Defunct:

* The ability to use GetFeatureInfo with feature IDs is deprecated (and will be removed in 3.0). Use
  feature names instead.
* GetRecommendedBlueprints is replaced by ListBlueprints and deprecated (and will be removed in 3.0).
* RequestPredictions is deprecated and replaced by RequestPredictionsForDataset. RequestPredictionsForDataset will be
  renamed to RequestPredictions in 3.0.
* DeletePendingJobs is removed; use DeleteModelJob instead
* GetFeatures is removed; use ListModelFeatures instead
* GetPendingJobs is removed; use GetModelJobs instead
* StartAutopilot is removed; use SetTarget instead
* parameter url is removed from ConnectToDataRobot
* parameter jobStatus is removed from GetModelJobs
* parameters saveFile and csvExtension are removed from RequestPredictions
* parameters saveFile and csvExtension are removed from SetupProject
* "semi" mode option (functions SetTarget, StartNewAutopilot) is deprecated (and will be removed in 3.0).

New features:

* The API now supports the new Feature Impact feature. Use RequestFeatureImpact to start a job to compute
  FeatureImpact, and GetFeatureImpactForModel or GetFeatureImpactForJobId to retrieve the completed Feature
  Impact results.
* The new functions CreateDerivedFeatureAsCategorical, CreateDerivedFeatureAsText, CreateDerivedFeatureAsNumeric
  can be used to create derived features as type transforms of existing features.
* The API now supports uploading (UploadPredictionDataset), listing (ListPredictionDatasets), and deleting
  (DeletePredictionDataset) datasets for prediction as well as requesting predictions (RequestPredictionsForDataset) against
  such datasets.

Bugfixes

* as.data.frame fixed for empty listOfBlueprints, listOfFeaturelists, listOfModels
* The documentation for SetTarget incorrectly referred to the 'semiauto' (rather than 'semi') autopilot setting.
  This is fixed.
* GetPredictions previously used a maxWait of 60, regardless of what maxWait the user specified. This is fixed.

# datarobot v2.2.33

Bugfixes

* GetModelJobFromId was broken by v2.2.32 and is now fixed.
* CreateFeaturelist was broken by v2.2.32 and is now fixed.

# datarobot v2.2.32

API Changes

* Package renamed to `datarobot`.

New features:

* ListJobs and DeleteJob functions added. ListJobs lists the jobs in the
  project queue (of any type). DeleteJob can be used to cancel
  one of these jobs.
* ListFeatureInfo (for all features) and GetFeatureInfo (for one feature) have
  been added for retrieving feature details.

Enhancements:

* In line with new functionality in version 2.2 of the DataRobot API,
  CreateUserPartition now allows `holdoutLevel` to be NULL (which results in not
  sending the holdout level, in line with backend API changes to allow user
  partitions to be created without a holdout level).
* Slices using `[` from objects of type listOfBlueprints, listOfFeaturelists, and listOfModels will now
  retain the appropriate type.
* Several functions (e.g. ConnectToDataRobot, DeleteModel, PauseQueue, etc.) used
  to return TRUE as their only possible return value. Now they return nothing instead.
* GetValidMetrics no longer has special-casing for the situation when the project is
  not yet ready to give you the valid metrics for a potential metric. In this case,
  an error will now be returned from the server.
* Error messages from the server now include additional detail.
* To improve error messages, in several places error messages no longer reference
  the top-level function the user called.
* The SetTarget function will now properly block execution until the server indicates
  the project has finished initializing and is ready to build models

Deprecated and Defunct:

* GetFeatures has been deprecated and renamed to ListModelFeatures (for more
  more clarity/consistency in naming and to avoid confusion with the now GetFeatureInfo
  and ListFeatureInfo)
* Support for authenticating via username/password has been removed. Use an API
  Token instead
* Removed broken UpdateDefaultPartition. To use one of the default partition methods
  with updated settings, please use CreateRandomPartition or CreateStratifiedPartition.

# datarobot v2.1.31

Enhancements

* Use of the WaitForAutopilot function will no longer trigger deprecation
  warnings

# datarobot v2.1.30

Bugfixes

* Due to a dependency on the methods package (which is loaded by default interactively
  but not running Rscript), RequestPredictions did not work when invoked with Rscript. This
  is fixed. The methods package is now in 'depends' instead of 'imports' to prevent this
  problem from ever occurring again.

# datarobot v2.1.29

Deprecated & Defunct

* Removed broken UpdateDefaultPartition. Please use the other partition-creating functions.

# datarobot v2.1.28

Bugfixes

* Due to a dependency on the methods package (which is loaded by default interactively
  but not running Rscript), some functions did not work when invoked with Rscript. This
  is fixed.
* SetupProject and GetPredictions now check for and displays errors in
  project creation (previously they would keep waiting and time out if
  there are errors)
* Previously errors would sometimes appear missing a space between two words. This is fixed.

# datarobot v2.1.27

Bugfixes

* Fixed a problem that caused an error when getting predictions if the
  installed version of the httr package was 1.0 and older.

# datarobot v2.1.26

Enhancements:

* HTTP requests now include User-Agent headers for logging purposes,
  e.g. "DataRobotRClient/2.0.25 (Darwin 14.5.0 x86_64)".
* We now provide a more informative error message after receiving HTML
  from the server when we expected JSON.
* We avoid httr encoding warning messages by specifying UTF-8.
* It is now possible to not specify the desired jobStatus in GetPendingJobs
  (by passing NULL for the jobStatus argument, which is now the default).
* GetPredictions now checks whether a prediction job has errored or been
  canceled and will error right away in that case (instead of waiting
  until the timeout)
* When specifying the data source as a dataframe (in RequestPredictions
  or SetupProject), the class may now be a subclass of dataframe
  (it need not be equal to dataframe).
* Previously GetModelJobs returned a dataframe when there are jobs but
  an empty list when there are none. Now it consistently returns a
  dataframe (with zero rows if there are no jobs) either way.

New features:

* ConnectToDataRobot can now read from a YAML config file.
* On package startup, we look for a config file in the default
  location, so the user does not need to call ConnectToDataRobot
  explicitly
* WaitForAutopilot function added. This function periodically checks
  whether Autopilot is finished and returns only after it is.
* SetupProject and RequestPredictions now default to using a tempfile
  instead of placing the file to be uploaded into the current working
  directory.
* New function StartNewAutopilot can be used to restart autopilot on a
  specific featurelist if it was previously running on a different one.
* New function SetTarget provides the functionality that StartAutopilot
  used to be responsible for. StartAutopilot is now deprecated, and
  SetTarget should be used instead. This function can now take a
  featurelistId argument, specifying which featurelist to use.

Bugfixes:

* GetPendingJobs (now deprecated in favor of GetModelJobs) was broken
  and is now fixed.
* GetValidMetrics was broken and is now fixed.
* GetProjectList no longer errors when there are no projects. It now
  returns an object whose structure matches the returned object when
  there are projects.

Deprecated and Defunct:

* The arguments controlling where the tempfile goes (in SetupProject
  and RequestPredictions) are now deprecated
* DeletePendingJob is deprecated (use DeleteModelJob instead)
* GetPendingJob is deprecated (use GetModelJob instead)
* jobStatus argument to GetModelJob/GetPendingJob is deprecated (use
  status instead)
* StartAutopilot is deprecated (use SetTarget instead).

API Changes Summary:

* Support for the experimental date partitioning has been removed in
  DataRobot API, so it is being removed from the client immediately -
  the CreateDatePartition function has been removed.

# datarobot v2.0.25

Enhancements:

* Codebase cleaned of many lint violations.

New Features:

* DeletePredictJob, GetPredictJobs, GetPredictions, RequestPredictions
  all added to control the prediction functionality created in v2.0
  featureset of the API.
* "quickrun" parameter added to StartAutopilot. This boolean enables
  use of the quickrun autopilot feature of DataRobot.

Bugfixes:
None

Deprecated and Defunct:
None

API Changes:
None

# datarobot v0.2.24

* fixes the maxWait parameter that was unsuccessfully introduced in 0.2.23

# datarobot v0.2.23

* maxWait parameter added to SetupProject to allow for datasets that take very
  long to initialize on the DataRobot server

# datarobot v0.2.22

* Documentation structure changed to use Roxygen2

<!-- markdownlint-disable-file MD025 -->
