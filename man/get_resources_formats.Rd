% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataset.R
\name{get_resources_formats}
\alias{get_resources_formats}
\title{Dataset resources format}
\usage{
get_resources_formats(dataset)
}
\arguments{
\item{dataset}{Dataset}
}
\value{
Character Format of the resources
}
\description{
Gets format of all resources from the datasets
}
\examples{
\dontrun{
# Setting the config to use HDX default server
 set_rhdx_config()
 res <- search_dataset(rows = 3L)
 get_resources_formats(res[[1]])
}
}
