% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jmlR.toolbox.R
\name{packages.load}
\alias{packages.load}
\title{Function packages.load()}
\usage{
packages.load(package_name_list = "", b_load = T)
}
\arguments{
\item{package_name_list}{List of package(s) to install and/or load. Default: <empty>}

\item{b_load}{TRUE or FALSE load package(s) after install (in necessary). Default: TRUE}
}
\value{
Returns a vector of character strings representing the result of the operation.
}
\description{
This function install/loads a list of packages
}
\examples{
# pckgs=c("alfa", "beta", "gamma")
# packages.load(pckgs, b_load=F)
}
\author{
Jean Marc Lietaer, \email{jmlietaer.tech@gmail.com}
}
\keyword{utilities}
