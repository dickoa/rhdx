% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resource.R
\name{get_resource_sheets}
\alias{get_resource_sheets}
\title{Get the names of the sheets of XLS(X) resources}
\usage{
get_resource_sheets(
  resource,
  format = NULL,
  download_folder = NULL,
  quiet = TRUE
)
}
\arguments{
\item{resource}{Resource, an HDX resource}

\item{format}{character; file format}

\item{download_folder}{character, path of the directory where you will store the data}

\item{quiet}{logical, no progress bar from download (default = FALSE)}
}
\value{
the names of the sheets of XLS(X) resources
}
\description{
Get the names of the sheets of XLS(X) resources
}
