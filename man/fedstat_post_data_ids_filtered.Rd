% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fedstat_post_data_ids_filtered.R
\name{fedstat_post_data_ids_filtered}
\alias{fedstat_post_data_ids_filtered}
\title{Post data filters ids to fedstat.ru and download specified subset of data}
\usage{
fedstat_post_data_ids_filtered(
  data_ids,
  ...,
  data_format = c("sdmx", "excel"),
  timeout_seconds = 180,
  retry_max_times = 3,
  httr_verbose = NULL
)
}
\arguments{
\item{data_ids}{data.frame, can be a result of \code{fedstat_get_data_ids} or
\code{fedstat_get_data_ids_special_cases_handle} to download all available data,
or a result of \code{fedstat_data_ids_filter} to download subset of available data}

\item{...}{other arguments passed to httr::POST}

\item{data_format}{string, one of sdmx, excel}

\item{timeout_seconds}{numeric, maximum time before a new POST request is tried}

\item{retry_max_times}{numeric, maximum number of tries to POST \code{data_ids}}

\item{httr_verbose}{\code{httr::verbose()} or NULL, outputs messages to the console
about the processing of the request}
}
\value{
raw bytes (sdmx or excel)
}
\description{
Creates a request body from \code{data_ids} and sends it to
https://www.fedstat.ru/indicator/data.do?format={\code{data_format}}.
Gets an sdmx or excel with data in binary format.

sdmx raw bytes can be passed to \code{fedstat_parse_sdmx_to_table} to create a
data.frame or to \code{rawToChar} and \code{writeLines} to create an xml file

excel raw bytes can be passed to \code{writeBin} to create an xls file
}
\examples{
\dontrun{
# Get data filters identificators for CPI
# filter the data_ids to get data for january of 2023
# for all goods and services for Russian Federation
# POST filters and download data in sdmx format
data <- fedstat_get_data_ids("31074") \%>\%
  fedstat_data_ids_filter(
    filters = list(
      "Territory" = "Russian Federation",
      "Year" = "2023",
      "Period" = "January",
      "Types of goods and services" = "*"
    )
  ) \%>\%
  fedstat_post_data_ids_filtered()

# Not actual filter field titles and filter values titles because of ASCII requirement for CRAN
}
}
\seealso{
\code{\link{fedstat_parse_sdmx_to_table}}
}
