% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gomp.R
\name{gomp}
\alias{gomp}
\title{Fit Gompertz model}
\usage{
gomp(y, time)
}
\arguments{
\item{y}{Proportion of the disease. Must be a value between 0 and 1.}

\item{time}{Measurement time}
}
\value{
r Growth rate

y0 Initial inoculum

time Measurement time

pred Predicted data

r2 Determination coefficient between the observed and predicted data
}
\description{
Fit Gompertz model
}
\examples{
dap <- seq(1, 70, 10)
inc <- c(0.01, 0.14, 0.40, 0.65, 0.88, 0.96, 0.98)
gomp(y = inc, time = dap)
}
