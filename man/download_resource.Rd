% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resource.R
\name{download_resource}
\alias{download_resource}
\title{Download an HDX resource}
\usage{
download_resource(
  resource,
  folder = NULL,
  filename = NULL,
  quiet = FALSE,
  force = FALSE,
  ...
)
}
\arguments{
\item{resource}{Resource, an HDX resource}

\item{folder}{character, path of the directory where you will store the data}

\item{filename}{(character), name of the file you will download}

\item{quiet}{(logical), no progress bar from download (default = \code{FALSE})}

\item{force}{(logical) force download (default = \code{FALSE})}

\item{...}{extra paramaters}
}
\value{
Resource
}
\description{
Download an HDX resource into a specific folder
}
\examples{
\dontrun{
#Setting the config to use HDX default server
 res <- read_resource("98aa1742-b5d3-40c3-94c6-01e31ded6e84")
 download_resource(res, folder = "/tmp")
}
}
