#' Reformat paginated data returned from the server.
#' @param serverData list. Raw JSON parsed list returned from the server.
CleanServerData <- function(serverData) {
  serverData$nextPage <- serverData$`next`
  serverData$previousPage <- serverData$previous
  serverData$`next` <- NULL
  serverData$previous <- NULL
  serverData
}


#' Handle server side pagination.
#' @inheritParams CleanServerData
#' @param batchSize integer. The number of requests per page to expect.
GetServerDataInRows <- function(serverData, batchSize = 50) {
  serverData <- CleanServerData(serverData)
  count <- serverData$count
  rows <- serverData$data
  n <- 0
  # Limit will still return a next page, so we ensure there is sufficient data to merit a next
  # page and that there is a next page, before paginating.
  while (length(serverData$data) >= batchSize && length(serverData$nextPage) > 0) {
    serverData <- DataRobotGET(serverData$nextPage, addUrl = FALSE, simplifyDataFrame = FALSE)
    serverData$nextPage <- serverData$`next`
    count <- count + serverData$count
    rows <- append(rows, serverData$data)
  }
  rows
}
