% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blueprints_apiWrapper.R
\name{ListProjectsModelsLogs}
\alias{ListProjectsModelsLogs}
\title{ListProjectsModelsLogs}
\usage{
ListProjectsModelsLogs(modelId, projectId, ...)
}
\description{
Retrieve an archive (tar.gz) of the logs produced and persisted by a model.
}
\details{
Retrieve an archive (tar.gz) of the logs produced and persisted by a model. Note that only blueprints with custom tasks create persistent logs - this will not work with any other type of model.
}
\seealso{
The method ProjectsModelsLogsList in [datarobot.apicore::BlueprintsApi], which this function wraps.

Other Blueprints: 
\code{\link{CreateUserBlueprintsBulkValidations}()},
\code{\link{CreateUserBlueprintsFromBlueprintId}()},
\code{\link{CreateUserBlueprintsFromCustomTaskVersionId}()},
\code{\link{CreateUserBlueprintsFromUserBlueprintId}()},
\code{\link{CreateUserBlueprintsProjectBlueprints}()},
\code{\link{CreateUserBlueprintsTaskParameters}()},
\code{\link{CreateUserBlueprintsValidations}()},
\code{\link{CreateUserBlueprints}()},
\code{\link{DeleteManyUserBlueprints}()},
\code{\link{DeleteUserBlueprints}()},
\code{\link{GetBlueprintChart}()},
\code{\link{GetBlueprintDocumentation}()},
\code{\link{GetBlueprint}()},
\code{\link{GetModelBlueprintChart}()},
\code{\link{GetModelBlueprintDocumentation}()},
\code{\link{ListBlueprints}()},
\code{\link{ListProjectsModelsTrainingArtifact}()},
\code{\link{ListUserBlueprintsInputTypes}()},
\code{\link{ListUserBlueprintsSharedRoles}()},
\code{\link{ListUserBlueprintsTasks}()},
\code{\link{ListUserBlueprints}()},
\code{\link{PatchManyUserBlueprintsSharedRoles}()},
\code{\link{PatchUserBlueprints}()},
\code{\link{RetrieveUserBlueprints}()}
}
\concept{Blueprints}