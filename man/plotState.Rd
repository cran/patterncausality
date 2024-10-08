% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotState.R
\name{plotState}
\alias{plotState}
\title{3D State Space Plot for Phase Space Reconstruction}
\usage{
plotState(statedata, ..., style = 2)
}
\arguments{
\item{statedata}{A matrix or data frame containing the reconstructed state space data, where each column corresponds to a different dimension. The input must have exactly 3 columns, corresponding to an embedding dimension (\code{E}) of 3.}

\item{...}{Some other settings for plot}

\item{style}{choose a style for the 3D plot}
}
\value{
A 3D scatter plot that visualizes the state space of the time series data. The plot provides insights into the underlying structure and behavior of the dynamical system being studied.
}
\description{
The \code{plotState} function generates a 3D plot of the reconstructed state space for time series data with an embedding dimension of 3. It uses the \code{scatter3D_fancy} function to create a detailed 3D scatter plot, which helps visualize the dynamics of the system in three dimensions.
}
\examples{
\donttest{
data(climate_indices)
state_data <- stateSpace(climate_indices$AAO, E = 3, tau = 2)
plotState(state_data,phi=20, style=1)
}
}
