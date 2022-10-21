fakeEndpoint <- "fake_endpoint"
fakeToken <- "fake_token"

fakeProjectId <- "project-id00000000000000"
fakeProject <- structure(list(projectName = "FakeProject",
                              projectId = fakeProjectId,
                              fileName = "fake.csv",
                              created = "faketimestamp"),
                         class = "dataRobotProject")
rootProjectsUrl <- UrlJoin(fakeEndpoint, "projects")
projectUrl <- UrlJoin(rootProjectsUrl, fakeProjectId)

fakeProjectJson <- fakeProject
fakeProjectJson$id <- fakeProject$projectId
fakeProjectJson$projectId <- NULL
class(fakeProjectJson) <- "list"
fakeProjectJson <- jsonlite::toJSON(fakeProjectJson, auto_unbox = TRUE)

fakeModelId <- "model-id0000000000000000"
fakeModel <- structure(list(projectId = fakeProjectId, modelId = fakeModelId),
                            class = "dataRobotModel")
fakePrimeModel <- structure(list(projectId = fakeProjectId, modelId = fakeModelId),
                            class = "dataRobotPrimeModel")
fakeDatetimeModel <- structure(list(projectId = fakeProjectId, modelId = fakeModelId),
                               class = "dataRobotDatetimeModel")
modelUrl <- UrlJoin(projectUrl, "models", fakeModelId)
rulesetsUrl <- UrlJoin(modelUrl, "primeRulesets")

fakeBlueprintId <- "fake-blueprint"
fakeBlueprint <- list(projectId = fakeProjectId,
                      processes = list("Some Bogus ML Crap"),
                      blueprintId = fakeBlueprintId,
                      modelType = "Fake Model")

fakeFeaturelistId <- "fake-featurelist"
fakeFeaturelistName <- "featurelist"
fakeFeature <- "feature"
fakeFeatures <- c("oneFeature", "twoFeature", "redFeature", "blueFeature")
fakeFeaturelist <- list("projectId" = fakeProjectId,
                        "featurelistId" = fakeFeaturelistId,
                        "name" = fakeFeaturelistName,
                        "features" = fakeFeatures)

fakeJobId <- "job"
jobUrl <- UrlJoin(projectUrl, "jobs", fakeJobId)
statusUrl <- datarobot:::UrlJoin(fakeEndpoint, "status", "some-status")

fakeTemplateId <- "template-id"
fakeTemplate <- structure(list(id = fakeTemplateId, name = "fake-template"),
                          class = "dataRobotComplianceDocTemplate")

fakeFilePath <- "fake_filepath.json"

fakeTarget <- "fake-target"

fakeDataStoreId <- "dataStore-id"
fakeDataSourceId <- "dataSource-id"
fakeDataSource <- structure(list(id = fakeDataSourceId, canonicalName = "foo"),
                            class = "dataRobotDataSource")
fakeDataStore <- structure(list(id = fakeDataStoreId, canonicalName = "foo"),
                           class = "dataRobotDataStore")
fakeDriverId <- "driver-id"
fakeJdbcUrl <- "jdbc:postgresql://my.db.address.org:5432/fake_db"

fakeUsername <- "username"
fakePassword <- "p@ssw0rd"

fakePredictionId <- "fakepredidmustbe24length"
getPredictionsUrl <- UrlJoin("projects", fakeProjectId, "predictJobs", fakeJobId)

fakeDatasetId <- "dataset-id"
fakeDataset <- structure(list(name = "fake", projectId = fakeProjectId, id = fakeDatasetId),
                         class = "dataRobotPredictionDataset")

fakeFoo <- structure(list(id = fakeDataSourceId), class = "dataRobotFoo")

fakeCalendarId <- "calendar-id0000000000000"
fakeCalendar <- structure(list(name = "calendar", projectIds = list(), id = fakeCalendarId),
                          class = "dataRobotCalendar")

fakeDateColumn <- "timestamp"
fakeMultiIdColumn <- "series_id"
fakeCrossIdColumn <- "category"

fakeDeploymentId <- "deployment-id00000000000"
fakeDeployment <- structure(list(id = fakeDeploymentId,
                                 model = fakeModel),
                            class = "dataRobotDeployment")

fakePredictionServerId <- "pred-server-id000000000"
fakePredictionServer <- structure(list(url = "fake-pred-server-url",
                                       id = fakePredictionServerId,
                                       dataRobotKey = "data-robot-key-yo"),
                                  class = "dataRobotPredictionServer")

#
# Helper functions for testing Datetime Models and Partitions
#
fakeDatetimeModelTrainingRowCount <- 200
