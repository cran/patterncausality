% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pcMatrix.R
\name{pcMatrix}
\alias{pcMatrix}
\title{Pattern Causality Matrix Analysis}
\usage{
pcMatrix(dataset, E, tau, metric, h, weighted)
}
\arguments{
\item{dataset}{A data frame where each column represents a time series to be analyzed for causal relationships.}

\item{E}{An integer specifying the embedding dimension for reconstructing the state space of the time series.}

\item{tau}{An integer representing the time delay used in reconstructing the time series in the embedded space.}

\item{metric}{A character string specifying the distance metric to be used in the analysis (e.g., 'euclidean').}

\item{h}{An integer indicating the prediction horizon, i.e., the number of steps ahead for which predictions are made.}

\item{weighted}{A logical value indicating whether the analysis should apply a weighted approach when calculating causality strength.}
}
\value{
A list containing three matrices (\code{positive}, \code{negative}, \code{dark}) that represent the detected causal relationships between the time series in the dataset. Each matrix provides the strength of positive, negative, and dark causality, respectively.
}
\description{
This function performs dynamic network analysis to detect causal interactions between multiple time series within a dataset. It uses the Pattern Causality Model Mk. II to evaluate positive, negative, and dark causality relationships by analyzing the reconstructed state spaces of the time series. The function iterates through each time series in the dataset, comparing them to identify potential causal links.
}
\examples{
\donttest{
data(climate_indices)
dataset <- climate_indices[,-1] # remove the date column
result <- pcMatrix(dataset, E = 3, tau = 1, metric = "euclidean", h = 2, weighted = TRUE)
print(result)
}
}
