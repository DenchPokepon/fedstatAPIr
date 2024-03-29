% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fedstat_parse_sdmx_to_table.R
\name{fedstat_parse_sdmx_to_table}
\alias{fedstat_parse_sdmx_to_table}
\title{Parse sdmx raw bytes to data.frame}
\usage{
fedstat_parse_sdmx_to_table(
  data_raw,
  return_type = c("data", "dictionary"),
  try_to_parse_ObsValue = TRUE
)
}
\arguments{
\item{data_raw}{sdmx raw bytes}

\item{return_type}{character, "data" or "dicionary", data for actual data,
dictionary for sdmx lookup table (full data codes dictionary)}

\item{try_to_parse_ObsValue}{logical, try to parse ObsValue column from character to R numeric type}
}
\value{
data.frame
}
\description{
Parses sdmx raw bytes received in response to POST request.
This function is a wrapper around \code{readsdmx::read_sdmx},
in addition to reading data, automatically adds columns with values from lookup tables.
Can also return full data codes dictionary for the indicator
}
\examples{
\dontrun{
# Get data filters identificators for CPI
# filter the data_ids to get data for january of 2023
# for all goods and services for Russian Federation
# POST filters and download data in sdmx format
# Parse raw sdmx to data.frame
data <- fedstat_get_data_ids("31074") \%>\%
  fedstat_data_ids_filter(
    filters = list(
      "Territory" = "Russian Federation",
      "Year" = "2023",
      "Period" = "January",
      "Types of goods and services" = "*"
    )
  ) \%>\%
  fedstat_post_data_ids_filtered() \%>\%
  fedstat_parse_sdmx_to_table()

# Not actual filter field titles and filter values titles because of ASCII requirement for CRAN
}
}
\seealso{
\code{\link{fedstat_parse_sdmx_to_table}}
}
