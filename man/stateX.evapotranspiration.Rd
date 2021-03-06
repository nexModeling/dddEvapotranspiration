% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stateX.evapotranspiration.R
\name{stateX.evapotranspiration}
\alias{stateX.evapotranspiration}
\title{Computes the evapotranspiration}
\usage{
stateX.evapotranspiration(htemp, sca)
}
\arguments{
\item{htemp}{temperature for each level zone}

\item{sca}{snow coverage for each level zone}
}
\value{
The output is a list with the evapotranspiraion (eatemp) as a value.
}
\description{
Computes the evapotranspiration. So far as a function of the snow coverage and the temperature.
 Both the snow coverage and the temperature are distributed over several level zones.
}
\examples{
\dontrun{
stateX()
}
}
\keyword{evapotranspiration}

