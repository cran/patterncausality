% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predictionY.R
\name{predictionY}
\alias{predictionY}
\title{Predict Signature and Pattern Vectors}
\usage{
predictionY(E, projNNy, zeroTolerance = (E + 1)/2)
}
\arguments{
\item{E}{Integer, the embedding dimension of the system, indicating the length of the
signature vector minus one.}

\item{projNNy}{A list containing two elements: \code{Signatures}, a matrix where each column
represents a component in the signature vector across different observations, and
\code{Weights}, a numeric vector representing the weights associated with each observation.}

\item{zeroTolerance}{A numeric value used to determine the sparsity threshold in the
signature matrix. Default is set to (E+1)/2.}
}
\value{
A dataframe with two columns: \code{predictedSignatureY} which contains the predicted
signature vector, and \code{predictedPatternY} which contains the corresponding pattern vector.
}
\description{
Uses neural network outputs to predict the state signatures and patterns
in a complex system. Adjusts for sparsity using zero tolerance.
}
\details{
Predict the signatures and patterns for a complex system

This function predicts the signature and pattern vectors for a given state based on
projections and weights derived from neural network outputs within a complex system.
The predictions adjust according to a specified zero tolerance level to manage sparsity.
}
\examples{
set.seed(123)
E <- 3
tau <- 1
Mx <- matrix(rnorm(300), nrow = 100)
My <- matrix(rnorm(300), nrow = 100)
Dx <- distanceMatrix(Mx, "minkowski")
Dy <- distanceMatrix(My, "minkowski")
SMx <- signatureSpace(Mx, E)
SMy <- signatureSpace(My, E)
PSMx <- patternSpace(SMx, E)
PSMy <- patternSpace(SMy, E)
CCSPAN <- (E - 1) * tau
NNSPAN <- E + 1
i <- 15
h <- 2
NNx <- pastNNsInfo(CCSPAN, NNSPAN, Mx, Dx, SMx, PSMx, i, h)
timesX <- NNx$times
projNNy <- projectedNNsInfo(My, Dy, SMy, PSMy, timesX, i, h)
predicted <- predictionY(E, projNNy)
print(predicted)
}
