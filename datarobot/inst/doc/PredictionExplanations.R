## ----results = "asis", message = FALSE, warning = FALSE-----------------------
library(httr)
library(knitr)
library(data.table)

## ----results = 'asis', message = FALSE, warning = FALSE, eval = FALSE---------
# library(datarobot)
# endpoint <- "https://<YOUR ENDPOINT HERE>/api/v2"
# apiToken <- "<YOUR API TOKEN HERE>"
# ConnectToDataRobot(endpoint = endpoint, token = apiToken)

## ----echo = FALSE, results = "asis", message = FALSE, warning = FALSE---------
Lending <- fread("https://s3.amazonaws.com/datarobot_public_datasets/10K_Lending_Club_Loans.csv")
EDA <- t(summary(Lending))
kable(EDA, longtable = TRUE, booktabs = TRUE, row.names = TRUE)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
# target <- "is_bad"
# projectName <- "Credit Scoring"
# 
# set.seed(1111)
# split <- sample(nrow(Lending), round(0.9 * nrow(Lending)), replace = FALSE)
# train <- Lending[split,]
# test <- Lending[-split,]
# 
# project <- StartProject(dataSource = train,
#                         projectName = projectName,
#                         target = target,
#                         workerCount = "max",
#                         wait = TRUE)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
# results <- as.data.frame(ListModels(project))
# kable(head(results), longtable = TRUE, booktabs = TRUE, row.names = TRUE)

## ----echo = FALSE, results = "asis", message = FALSE, warning = FALSE---------
results <- readRDS("PredictionExplanationsModelResults.rds")
kable(head(results), longtable = TRUE, booktabs = TRUE, row.names = TRUE)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
# bestModel <- GetRecommendedModel(project)
# bestPredictions <- Predict(bestModel, test, type = "probability")
# testPredictions <- data.frame(original = test$is_bad, prediction = bestPredictions)
# kable(head(testPredictions), longtable = TRUE, booktabs = TRUE, row.names = TRUE)

## ----echo = FALSE, results = "asis", message = FALSE, warning = FALSE---------
testPredictions <- readRDS("PredictionExplanationsTestPredictions.rds")
kable(head(testPredictions), longtable = TRUE, booktabs = TRUE, row.names = TRUE)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
# explanations <- GetPredictionExplanations(bestModel, test, maxExplanations = 3,
#                                           thresholdLow = 0.25, thresholdHigh = 0.75)
# kable(head(explanations), longtable = TRUE, booktabs = TRUE, row.names = TRUE)

## ----echo = FALSE, results = "asis", message = FALSE, warning = FALSE---------
explanations <- readRDS("PredictionExplanations.rds")
kable(head(explanations), longtable = TRUE, booktabs = TRUE, row.names = TRUE)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
# explanations <- GetPredictionExplanations(bestModel, test, maxExplanations = 3,
#                                           thresholdLow = 0.25, thresholdHigh = 0.75,
#                                           excludeAdjustedPredictions = FALSE)
# kable(head(explanations), longtable = TRUE, booktabs = TRUE, row.names = TRUE)

## ----echo = FALSE, results = "asis", message = FALSE, warning = FALSE---------
explanations <- readRDS("PredictionExplanationsExposure.rds")
kable(head(explanations), longtable = TRUE, booktabs = TRUE, row.names = TRUE)

