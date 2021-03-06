% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jmlR.toolbox.R
\name{pause}
\alias{pause}
\title{Function pause()}
\usage{
pause(t = "Press a key to continue...", i = T)
}
\arguments{
\item{t}{Pause text. Default: "Press a key to continue..."}

\item{i}{TRUE or FALSE - Do not print your input. Default: TRUE}
}
\value{
A character string representing the result of the input.
}
\description{
This function pauses for keyboard input
}
\examples{
pause()
#  returns Press a key to continue... and waits for input
pause("test")
#  returns test and waits for input
pause("")
#  returns an empty string and waits for input
pause(i=FALSE)
#  returns Press a key to continue... and prints your input
#  [1] <your input>
s <- pause("type here = ")
#  returns type here =  and puts your input in variable s
s
#  [1] "test"
#  Credits - Based on:
#    http://stackoverflow.com/revisions/18746519/3
}
\author{
Jean Marc Lietaer, \email{jmlietaer.tech@gmail.com}
}
\keyword{utilities}
