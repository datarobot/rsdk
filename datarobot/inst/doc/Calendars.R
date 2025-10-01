## -----------------------------------------------------------------------------
library(knitr)
calendar <- read.csv(system.file("extdata", "calendar.csv", package = "datarobot"))
kable(calendar)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
# library(datarobot)
# endpoint <- "https://<YOUR DATAROBOT URL GOES HERE>/api/v2"
# apiToken <- "<YOUR API TOKEN GOES HERE>"
# ConnectToDataRobot(endpoint = endpoint, token = apiToken)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
# calendar <- CreateCalendar("calendar.csv", name = "holidays")
# print(calendar)

## ----results = "asis", echo = FALSE-------------------------------------------
calendar <- readRDS("calendar.rds")
print(calendar)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
# calendars <- ListCalendars()
# calendar <- calendars[[1]]
# print(calendar)

## ----results = "asis", echo = FALSE-------------------------------------------
calendar <- readRDS("calendar.rds")
print(calendar)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
# newCalendar <- UpdateCalendar(calendar, name = "newName")
# print(newCalendar)

## ----results = "asis", echo = FALSE-------------------------------------------
calendar <- readRDS("calendar.rds")
calendar$name <- "newName"
print(calendar)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
# project <- SetupProject(timeSeriesData, projectName = "time series with calendar")
# cal <- CreateCalendar("calendar.csv")
# partition <- CreateDatetimePartitionSpecification("date",
#                                                   autopilotDataSelectionMethod = "duration",
#                                                   useTimeSeries = TRUE,
#                                                   calendar = cal)
# StartProject(project, partition = partition, target = "target")

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
# projectId <- "59dab74bbd2a54035786bfc0"
# calendar <- GetCalendarFromProject(project)
# print(calendar)

## ----results = "asis", echo = FALSE-------------------------------------------
calendar <- readRDS("calendar.rds")
print(calendar)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
# print(calendar$projectIds)

## ----results = "asis", echo = FALSE-------------------------------------------
calendar <- readRDS("calendar.rds")
calendar$projectIds <- list("59dab74bbd2a54035786bfc0")
print(calendar$projectIds)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
# Share(calendar, "other.person.email@your.company.com")

