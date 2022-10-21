## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
#  library(datarobot)
#  endpoint <- "https://<YOUR DATAROBOT URL GOES HERE>/api/v2"
#  apiToken <- "<YOUR API TOKEN GOES HERE>"
#  ConnectToDataRobot(endpoint = endpoint, token = apiToken)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
#  DownloadComplianceDocumentation(model, "path/to/filename.docx")

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
#  GetComplianceDocTemplate("path/to/filename.json")

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
#  DownloadComplianceDocTemplate("path/to/filename.json")
#  # ...then modify the compliance doc template in your favorite editor.
#  UploadComplianceDocTemplate(name = "myNewTemplate", filename = "path/to/modified_file.json")

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
#  sections <- list(list("title" = "Missing Values Report",
#                        "highlightedText" = "NOTICE",
#                        "regularText" = "This dataset had a lot of Missing Values. See the chart below: {{missingValues}}",
#                        "type" = "user"),
#                   list("title" = "Blueprints",
#                        "regularText" = "{{blueprintDiagram}} /n Blueprint for this model",
#                        "type" = "user"))
#  UploadComplianceDocTemplate(name = "myNewTemplateFromSections", sections = sections)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
#  myTemplate <- ListComplianceDocTemplates(namePart = "myNewTemplateFromSections")[[1]]
#  DownloadComplianceDocTemplate(myTemplate)

## ----results = "asis", message = FALSE, warning = FALSE, eval = FALSE---------
#  myTemplate <- ListComplianceDocTemplates(namePart = "myNewTemplate")[[1]]
#  CreateComplianceDocumentation(model, myTemplate)

