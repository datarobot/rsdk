# datarobot: 'DataRobot' Predictive Modeling API

<!-- badges: start -->

<!-- https://shields.io/category/version -->
![GitHub R package version (subdirectory of monorepo)](https://img.shields.io/github/r-package/v/datarobot/rsdk?filename=datarobot%2FDESCRIPTION&label=datarobot&logo=R)
![GitHub R package version (subdirectory of monorepo)](https://img.shields.io/github/r-package/v/datarobot/rsdk?filename=datarobot.apicore%2FDESCRIPTION&label=datarobot.apicore&logo=R)

<!-- badges: end -->

A read-only public repository of the R API Client for the DataRobot Public API. For working with the 'DataRobot' predictive modeling platform's API &lt;https://www.datarobot.com/&gt;.

## Installation

To install the latest official release, use:

```R
library(remotes)
install_github("datarobot/rsdk", subdir = "datarobot.apicore", ref = github_release())
install_github("datarobot/rsdk", subdir = "datarobot", ref = github_release())
library(datarobot)
```

You can find specific installation instructions for each release under [GitHub Releases](https://github.com/datarobot/rsdk/releases).

## Changelog

The changelog for the R API Client can be found in [NEWS.md in the `datarobot` package](datarobot/NEWS.md).

## Feedback

If you have feedback or questions on the R API Client, email [the team](mailto:api-maintainer@datarobot.com).
