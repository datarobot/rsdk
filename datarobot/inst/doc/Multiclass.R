## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
#  library(datarobot)
#  endpoint <- "https://<YOUR DATAROBOT URL GOES HERE>/api/v2"
#  apiToken <- "<YOUR API TOKEN GOES HERE>"
#  ConnectToDataRobot(endpoint = endpoint, token = apiToken)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
#  project <- StartProject(iris,
#                          projectName = "multiclassExample",
#                          target = "Species",
#                          targetType = TargetType$Multiclass,
#                          maxWait = 600)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
#  blueprint <- ListBlueprints(project)[[1]]
#  RequestNewModel(project, blueprint)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
#  model <- ListModels(project)[[1]]
#  predictions <- Predict(model, iris)
#  print(table(predictions))

## ----results = "asis", echo = FALSE-------------------------------------------
message("request issued, waiting for predictions")
message("Multiclass with labels setosa, versicolor, virginica")
print(table(readRDS("multiclassPredictions.rds")))

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
#  predictions <- Predict(model, iris, type = "probability")
#  kable(head(predictions))

## ----results = "asis", echo = FALSE-------------------------------------------
message("request issued, waiting for predictions")
message("Multiclass with labels setosa, versicolor, virginica")
library(knitr)
kable(head(readRDS("multiclassPredictionProbs.rds")))

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
#  confusionChart <- GetConfusionChart(model, source = DataPartition$VALIDATION)
#  kable(capture.output(confusionChart))

## ----results = "asis", echo = FALSE-------------------------------------------
kable(capture.output(readRDS("confusionChart.rds")))

