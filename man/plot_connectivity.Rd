% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_connectivity.R
\name{plot_connectivity}
\alias{plot_connectivity}
\title{Plot the network based on matrix of contact}
\usage{
plot_connectivity(
  matContact,
  size,
  vertexcexrate = 3,
  vertexcol = "grey",
  edgewidthrate = 5,
  netobj = FALSE,
  verbose = TRUE
)
}
\arguments{
\item{matContact}{Square matrix reporting the average proportion of time spent by professionals of a given ward in the different wards. Sum of rows must be equal to 1. The line width scale for edges will depend on this argument.}

\item{size}{Vector of population size in each ward (beds, HCWS or sum of both). The size of the nodes/vertex/wards will depend on this argument.}

\item{vertexcexrate}{Integer, proportional coefficient to adjust vertex names size.}

\item{vertexcol}{Character, vector defining the colors of vertices in the plot.}

\item{edgewidthrate}{Integer, proportional coefficient to adjust edge width.}

\item{netobj}{Logical, define if the function return an igraph oject (TRUE) or a plot (FALSE). Default is FALSE.}

\item{verbose}{Logical, activate production of details messages.}
}
\value{
If netobj is FALSE, the function returns a plot of wards network. If netobj is TRUE, the function returns an igraph object.
}
\description{
The function \code{plot_connectivity} build an igraph object representing the wards connected by the professionals activities.
The function also generate a generic png plot.
}
\examples{

data("toydata")
list2env(toydata,envir=.GlobalEnv)
gdata <- build_gdata()

matContact <- randomContacts(pop_size_H, ward_names)$contactMat
plot_connectivity(matContact, pop_size_P)

}
