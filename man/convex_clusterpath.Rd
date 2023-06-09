% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convex_clusterpath.R
\name{convex_clusterpath}
\alias{convex_clusterpath}
\title{Minimize the convex clustering loss function}
\usage{
convex_clusterpath(
  X,
  W,
  lambdas,
  tau = 0.001,
  center = TRUE,
  scale = TRUE,
  eps_conv = 1e-06,
  burnin_iter = 25,
  max_iter_conv = 5000,
  save_clusterpath = TRUE
)
}
\arguments{
\item{X}{An \eqn{n} x \eqn{p} numeric matrix. This function assumes that each
row represents an object with \eqn{p} attributes.}

\item{W}{A \code{sparseweights} object, see \link{sparse_weights}.}

\item{lambdas}{A vector containing the values for the penalty parameter.}

\item{tau}{Parameter to compute the threshold to fuse clusters. Default is
0.001.}

\item{center}{If \code{TRUE}, center \code{X} so that each column has mean
zero. Default is \code{TRUE}.}

\item{scale}{If \code{TRUE}, scale the loss function to ensure that the
cluster solution is invariant to the scale of \code{X}. Default is
\code{TRUE}. Not recommended to set to \code{FALSE} unless comparing to
algorithms that minimize the unscaled convex clustering loss function.}

\item{eps_conv}{Parameter for determining convergence of the minimization.
Default is 1e-6.}

\item{burnin_iter}{Number of updates of the loss function that are done
without step doubling. Default is 25.}

\item{max_iter_conv}{Maximum number of iterations for minimizing the loss
function. Default is 5000.}

\item{save_clusterpath}{If \code{TRUE}, store the solution that minimized
the loss function for each lambda. Is required for drawing the clusterpath.
Default is \code{FALSE}. To store the clusterpath coordinates, \eqn{n} x
\eqn{p} x \eqn{no. lambdas} have to be stored, this may require too much
memory for large data sets.}
}
\value{
A \code{cvxclust} object containing the following
\item{\code{info}}{A dataframe containing for each value for lambda: the
number of different clusters, and the value of the loss function at the
minimum.}
\item{\code{merge}}{The merge table containing the order at which the
observations in \code{X} are clustered.}
\item{\code{height}}{The value for lambda at which each reduction in the
number of clusters occurs.}
\item{\code{order}}{The order of the observations in \code{X} in order to
draw a dendrogram without conflicting branches.}
\item{\code{elapsed_time}}{The number of seconds that elapsed while
running the code. Note that this does not include the time required for
input checking and possibly scaling and centering \code{X}.}
\item{\code{coordinates}}{The clusterpath coordinates. Only part of the
output in case that \code{save_clusterpath=TRUE}.}
\item{\code{lambdas}}{The values for lambda for which a clustering was
found.}
\item{\code{eps_fusions}}{The threshold for cluster fusions that was used by
the algorithm.}
\item{\code{num_clusters}}{The different numbers of clusters that have been
found.}
\item{\code{n}}{The number of observations in \code{X}.}
}
\description{
Minimizes the convex clustering loss function for a given set of
values for lambda.
}
\examples{
# Load data
data(two_half_moons)
data = as.matrix(two_half_moons)
X = data[, -3]
y = data[, 3]

# Get sparse weights in dictionary of keys format with k = 5 and phi = 8
W = sparse_weights(X, 5, 8.0)

# Set a sequence for lambda
lambdas = seq(0, 2400, 1)

# Compute clusterpath
res = convex_clusterpath(X, W, lambdas)

# Get cluster labels for two clusters
labels = clusters(res, 2)

# Plot the clusterpath with colors based on the cluster labels
plot(res, col = labels)

}
\seealso{
\link{convex_clustering}, \link{sparse_weights}
}
