## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
#  library(datarobot)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
#  ConnectToDataRobot(endpoint = "YOUR-ENDPOINT-HERE", token = "YOUR-API_TOKEN-HERE")

## ---- echo = FALSE, message = FALSE-------------------------------------------
library(MASS)
data(Boston)

## ---- echo = TRUE, message = FALSE--------------------------------------------
head(Boston)

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  project <- StartProject(dataSource = Boston,
#                          projectName = "BostonVignetteProject",
#                          target = "medv",
#                          wait = TRUE)

## ---- echo = FALSE------------------------------------------------------------
project <- readRDS("projectObject.rds")
project

## ---- echo = FALSE------------------------------------------------------------
library(datarobot)
listOfBostonModels <- readRDS("listOfBostonModels.rds")
fullFrame <- as.data.frame(listOfBostonModels, simple = FALSE)

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  listOfBostonModels <- ListModels(project)

## ---- echo = TRUE-------------------------------------------------------------
summary(listOfBostonModels)

## ---- echo = TRUE, fig.width = 7, fig.height = 6, fig.cap = "Horizontal barplot of modelType and validation set RMSE values for all project models"----
plot(listOfBostonModels, orderDecreasing = TRUE)

## ---- echo = TRUE-------------------------------------------------------------
modelFrame <- as.data.frame(listOfBostonModels)
head(modelFrame[, c("modelType", "validationMetric")])

## ---- echo = TRUE-------------------------------------------------------------
tail(modelFrame[, c("modelType", "validationMetric")])

## ---- echo = TRUE-------------------------------------------------------------
Filter(function(m) grepl("Ridge", m), modelFrame$expandedModel)

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  bestModel <- GetRecommendedModel(project)
#  bestPredictions <- Predict(bestModel, Boston)

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  bestModel$modelType

## ---- echo = FALSE, eval = TRUE-----------------------------------------------
"Gradient Boosted Greedy Trees Regressor (Least-Squares Loss)"

## ---- echo = FALSE, fig.width = 7, fig.height = 6-----------------------------
medv <- Boston$medv
bestPredictions <- readRDS("bestPredictions.rds")
plot(medv, bestPredictions, xlab="Observed medv value", ylab="Predicted medv value",
     ylim = c(0, 50))
abline(a = 0, b = 1, lty = 2, lwd = 3, col = "red")
title("Best model")

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  impact <- GetFeatureImpact(model)
#  head(impact)

## ---- echo = FALSE------------------------------------------------------------
impact <- readRDS("IntroFeatureImpact.RDS")
head(impact)

