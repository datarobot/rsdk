% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{reorderColumns}
\alias{reorderColumns}
\title{Reorder the columns in a data.frame}
\usage{
reorderColumns(df, vars)
}
\arguments{
\item{df}{data.frame with named columns.}

\item{vars}{integer. named vector where the names represent column names in
df that should be moved. The value of each item is the new location of
that column.}
}
\value{
A copy of the input data.frame, with columns rearranged per vars
}
\description{
This function reorders columns in a data.frame without relying on dplyr or
data.table. You only need to specify the columns that should be moved; all
others will be slotted in the gaps. Invalid columns are ignored.
}
\examples{
{
  df <- data.frame(Time=c(1,2), In=c(2,3), Out=c(3,4), Files=c(4,5))
  datarobot:::reorderColumns(df, c("In" = 3, "Time" = 4))
}

}
