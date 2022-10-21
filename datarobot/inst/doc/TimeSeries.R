## ---- echo = FALSE------------------------------------------------------------
library(knitr)
data <- data.frame(row = seq(9),
                   time = as.Date("2017-01-02") + seq(9),
                   target = c(16443, 3013, 1643, rep(NA, 6)),
                   temp = c(72, 72, 68, rep(NA, 6)))
kable(data)

## ---- echo = FALSE------------------------------------------------------------
library(knitr)
data <- data.frame(row = seq(9),
                   time = as.Date("2017-01-02") + seq(9),
                   target = c(16443, 3013, 1643, rep(NA, 6)),
                   holiday = c(TRUE, rep(FALSE, 5), TRUE, rep(FALSE, 2)))
kable(data)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
#  partition <- CreateDatetimePartitionSpecification(datetimePartitionColumn = "timestamp",
#                                                    useTimeSeries = TRUE)
#  StartProject(dataSource = data, target = "target", partition = partition, metric = "RMSE")

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
#  partition <- CreateDatetimePartitionSpecification(datetimePartitionColumn = "timestamp",
#                                                    featureDerivationWindowStart = -24,
#                                                    featureDerivationWindowEnd = -12,
#                                                    useTimeSeries = TRUE)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
#  partition <- CreateDatetimePartitionSpecification(datetimePartitionColumn = "timestamp",
#                                                    forecastWindowStart = 1,
#                                                    forecastWindowEnd = 10,
#                                                    useTimeSeries = TRUE)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
#  predictions <- Predict(timeSeriesModel, testData, forecastPoint = "1958-01-01")

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
#  partition <- CreateDatetimePartitionSpecification(datetimePartitionColumn = "timestamp",
#                                                    useTimeSeries = TRUE,
#                                                    featureSettings = list("featureName" = "holiday",
#                                                                           "knownInAdvance" = TRUE))
#  project <- StartProject(data,
#                          projectName = "test-TimeSeries",
#                          target = "target",
#                          partition = partition,
#                          metric = "RMSE")

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
#  partition <- CreateDatetimePartitionSpecification(datetimePartitionColumn = "timestamp",
#                                                    useTimeSeries = TRUE,
#                                                    featureSettings = list(list("featureName" = "holiday",
#                                                                                "knownInAdvance" = TRUE),
#                                                                           list("featureName" = "weekend",
#                                                                                "knownInAdvance" = TRUE)))

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
#  data <- read.csv(system.file("extdata", "multiseries.csv", package = "datarobot"))
#  partition <- CreateDatetimePartitionSpecification(datetimePartitionColumn = "timestamp",
#                                                    useTimeSeries = TRUE,
#                                                    multiseriesIdColumns = "series_id")
#  project <- StartProject(data,
#                          projectName = "test-TimeSeries",
#                          target = "target",
#                          partition = partition,
#                          metric = "RMSE",
#                          mode = AutopilotMode$Manual,
#                          targetType = "Regression")

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
#  partition <- CreateDatetimePartitionSpecification(datetimePartitionColumn = "timestamp",
#                                                    useTimeSeries = TRUE,
#                                                    featureSettings = list(list("featureName" = "sales",
#                                                                                "doNotDerive" = TRUE)))

