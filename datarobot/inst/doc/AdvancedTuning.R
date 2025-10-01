## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
# tuningJobId <- RunInteractiveTuning(myModel)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
# tunedModel <- GetModelFromJobId(myModel$projectId, tuningJobId)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
# parameters <- GetTuningParameters(myModel)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
# summary(GetTuningParameters(myModel))

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
# myXGBModel <- GetModel(projectId, modelId)
# RunTune <- StartTuningSession(myXGBModel)
# tuningJob <- RunTune(myXGBModel, colsample_bytree = 0.4, colsample_bylevel = 0.8)
# tunedModel <- GetModelFromJobId(projectId, tuningJob)

