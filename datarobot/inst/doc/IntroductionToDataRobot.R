## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
#  library(datarobot)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
#  ConnectToDataRobot(endpoint = "YOUR-ENDPOINT-HERE", token = "YOUR-API_TOKEN-HERE")

## ---- echo = FALSE, message = FALSE-------------------------------------------
library(AmesHousing)
Ames <- make_ames()
Ames <- Ames[sapply(Ames,is.numeric)]

## ---- echo = TRUE, message = FALSE--------------------------------------------
head(Ames)

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  project <- StartProject(dataSource = Ames,
#                          projectName = "AmesVignetteProject",
#                          target = "Sale_Price",
#                          wait = TRUE)

## ---- echo = FALSE------------------------------------------------------------
project <- readRDS("AmesprojectObject.rds")
project

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
library(datarobot)
listOfAmesModels <- readRDS("listOfAmesModels.rds")
fullFrame <- as.data.frame(listOfAmesModels, simple = FALSE)

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  listOfAmesModels <- ListModels(project)

## ---- echo = TRUE-------------------------------------------------------------
summary(listOfAmesModels)

## ---- echo = TRUE, fig.width = 7, fig.height = 6, fig.cap = "Horizontal barplot of modelType and validation set Gamma Deviance values for all project models"----
plot(listOfAmesModels, orderDecreasing = TRUE)

## ---- echo = TRUE-------------------------------------------------------------
modelFrame <- as.data.frame(listOfAmesModels)
head(modelFrame[, c("modelType", "validationMetric")])

## ---- echo = TRUE-------------------------------------------------------------
tail(modelFrame[, c("modelType", "validationMetric")])

## ---- echo = TRUE-------------------------------------------------------------
Filter(function(m) grepl("Elastic-Net", m), modelFrame$expandedModel)


## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  bestModel <- GetRecommendedModel(project,
#                                   type = RecommendedModelType$RecommendedForDeployment)
#  bestPredictions <- Predict(bestModel, Ames)

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  bestModel$modelType

## ---- echo = FALSE, eval = TRUE-----------------------------------------------
"eXtreme Gradient Boosted Trees Regressor (Gamma Loss)"

## ---- echo = FALSE, fig.width = 7, fig.height = 6-----------------------------
Sale_Price <- Ames$Sale_Price
bestPredictions <- readRDS("bestPredictionsAmes.rds")
plot(Sale_Price, bestPredictions, xlab="Observed Sale Price", ylab="Predicted Sale Price value",
     ylim = c(0, 800000))
abline(a = 0, b = 1, lty = 2, lwd = 3, col = "red")
title("Best model")

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  impact <- GetFeatureImpact(bestModel)
#  head(impact)

## ---- echo = FALSE------------------------------------------------------------
impact <- readRDS("IntroFeatureImpactAmes.RDS")
head(impact)

