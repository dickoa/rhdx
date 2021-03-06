% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/user.R
\name{list_users}
\alias{list_users}
\title{List all users}
\usage{
list_users(order_by = "number_created_packages", configuration = NULL, ...)
}
\arguments{
\item{order_by}{Logical user sorted is TRUE}

\item{configuration}{Configuration the configuration to use}

\item{...}{Extra parameters}
}
\description{
List all users
}
\examples{
\dontrun{
# Setting the config to use HDX default server
 set_rhdx_config()
 list_user()
}
}
