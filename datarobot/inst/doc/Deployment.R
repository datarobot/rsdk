## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
#  library(datarobot)
#  project <- GetProject("5506fcd38bd88f5953219da0")
#  model <- ListModels(project)[[1]]
#  predictionServer <- ListPredictionServers()[[1]]
#  deployment <- CreateDeployment(model,
#                                 label = "New Deployment",
#                                 description = "A new deployment for demo purposes",
#                                 defaultPredictionServerId = predictionServer)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
#  ListDeployments()

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
#  GetDeployment("5e319d2e422fbd6b58a5edad")

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
#  deployment <- GetDeployment("5e319d2e422fbd6b58a5edad")
#  DeleteDeployment(deployment)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
#  project <- GetProject("5506fcd38bd88f5953219da0")
#  newModel <- ListModels(project)[[2]]
#  deployment <- GetDeployment("5e319d2e422fbd6b58a5edad")
#  ReplaceDeployedModel(deployment, newModel, ModelReplacementReason$Accuracy)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
#  project <- GetProject("5506fcd38bd88f5953219da0")
#  newModel <- ListModels(project)[[2]]
#  deployment <- GetDeployment("5e319d2e422fbd6b58a5edad")
#  validation <- ValidateReplaceDeployedModel(deployment, newModel)
#  print(validation$status)  # Look here to see if passing
#  print(validation$checks)  # Look here if not passing to see why

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
#  deployment <- GetDeployment("5e319d2e422fbd6b58a5edad")
#  GetDeploymentDriftTrackingSettings(deployment)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
#  deployment <- GetDeployment("5e319d2e422fbd6b58a5edad")
#  UpdateDeploymentDriftTrackingSettings(deployment, targetDriftEnabled = TRUE)

