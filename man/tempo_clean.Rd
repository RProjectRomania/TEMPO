% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tempo_clean.R
\name{tempo_clean}
\alias{tempo_clean}
\title{Clean a table downloaded from TEMPO Online database}
\usage{
tempo_clean(matrix)
}
\arguments{
\item{matrix}{- the R dataframe object to be cleaned, representing 
the table/matrix downloaded from TEMPO Online database}
}
\value{
Returns a R dataframe object.
}
\description{
tempo_clean cleans a table downloaded from 
TEMPO Online database
}
\details{
This function removes redundant columns or redundant information from columns
}
\examples{
tempo_clean(SOM101D)

}
