% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_testcount.R
\name{plot_testcount}
\alias{plot_testcount}
\title{Plot average daily number of test}
\usage{
plot_testcount(trajmwss, scale = 0, pop = NULL, iter = FALSE, ward = FALSE, daysint = 1)
}
\arguments{
\item{trajmwss}{List of data.table. Epidemiological trajectories simulated by the function \code{mwss::multisim}}

\item{scale}{Integer. Indicated the scale for statistics: 0/ whole facility, 1/ wards.}

\item{pop}{String. Define a subpopulation for statistics ("P": Test performed on patients, "H": Test performed on professionals ; default is FALSE: considering all tests).}

\item{iter}{Integer. Define a specific simulation. Default is FALSE, providing average values over all simulations.}

\item{ward}{String. Define a specific ward. Default is FALSE, considering all wards. At the facility scale '0' no ward can be specified.}

\item{daysint}{Integer. Indicated time step use to calculate average daily number of test (default is 1 day).}
}
\value{
Test count plot
}
\description{
\code{plot_testcount} returns an plot representing the average daily number of test performed
over the simulations at different scales and for different populations. The daily number of test is represented
by a barplot up to 30 time steps, then they are represented by dots and lines.
}
