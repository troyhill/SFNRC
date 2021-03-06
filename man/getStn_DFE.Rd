% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dfe_stn.R
\name{getStn_DFE}
\alias{getStn_DFE}
\title{DataForEver station query}
\usage{
getStn_DFE(pattern  = NULL, dbname = "hydrology")
}
\arguments{
\item{pattern}{pattern to be matched in station names ('NULL' returns all stations). A case-insensitive grep-friendly single character element (e.g., 'S333|S197' to search for multiple stations).}

\item{dbname}{name of the database sought for inquiry. Currently only 'hydrology' and 'waterquality' are supported. A case-insensitive character string.}
}
\value{
dataframe \code{getStn_DFE} returns a dataframe with stations and all associated information (coordinates, notes)
}
\description{
Identifies DataForEver stations in hydrology or water quality databases
}
\examples{
\dontrun{
  getStn_DFE(pattern = "S333")
  getStn_DFE(pattern = "S333", dbname = "waterquality")
}

}
