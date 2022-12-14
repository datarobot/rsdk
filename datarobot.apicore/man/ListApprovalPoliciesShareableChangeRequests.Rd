% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/governance_apiWrapper.R
\name{ListApprovalPoliciesShareableChangeRequests}
\alias{ListApprovalPoliciesShareableChangeRequests}
\title{ListApprovalPoliciesShareableChangeRequests}
\usage{
ListApprovalPoliciesShareableChangeRequests(
  approvalPolicyId,
  offset = 0,
  limit = 100,
  orderBy = "",
  ...
)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Retrieve associated Change Requests Info.
}
\details{
Get information about Change Requests submitted for a certain Approval Policy.

This method invokes \verb{GET /approvalPolicies/\{approvalPolicyId\}/shareableChangeRequests/} in the DataRobot Public API.
}
\seealso{
The method ApprovalPoliciesShareableChangeRequestsList in \link{GovernanceApi}, which invokes the same underlying API endpoint.
}
