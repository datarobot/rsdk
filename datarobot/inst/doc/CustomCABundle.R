## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(eval = FALSE)

## ----verify-pem---------------------------------------------------------------
# # Quick sanity check: the file should start with the PEM header
# readLines("/path/to/my-ca-bundle.pem", n = 1)
# # Expected output: "-----BEGIN CERTIFICATE-----"

## ----caBundle-arg-------------------------------------------------------------
# library(datarobot)
# 
# ConnectToDataRobot(
#   endpoint = "https://your-datarobot-host/api/v2",
#   token    = "YOUR-API-TOKEN",
#   caBundle = "/path/to/my-ca-bundle.pem"
# )

## ----caBundle-yaml------------------------------------------------------------
# library(datarobot)
# 
# # Uses ~/.config/datarobot/drconfig.yaml automatically
# ConnectToDataRobot()
# 
# # Or point at a specific config file
# ConnectToDataRobot(configPath = "~/.config/datarobot/drconfig.yaml")

## ----caBundle-envvar----------------------------------------------------------
# Sys.setenv(CURL_CA_BUNDLE = "/path/to/my-ca-bundle.pem")
# 
# library(datarobot)
# ConnectToDataRobot()

## ----caBundle-and-ssl---------------------------------------------------------
# # Recommended: verify with your private CA
# ConnectToDataRobot(
#   endpoint = "https://your-datarobot-host/api/v2",
#   token    = "YOUR-API-TOKEN",
#   caBundle = "/path/to/my-ca-bundle.pem"
# )
# 
# # Not recommended: disables all certificate verification
# ConnectToDataRobot(
#   endpoint  = "https://your-datarobot-host/api/v2",
#   token     = "YOUR-API-TOKEN",
#   sslVerify = FALSE
# )

