% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/07_simulate_scenario.R
\name{simulate_scenario}
\alias{simulate_scenario}
\title{Simulate scenario of SFC model object}
\usage{
simulate_scenario(
  model,
  scenario,
  max_iter = 350,
  periods = 100,
  hidden_tol = 0.1,
  tol = 1e-08,
  method = "Gauss"
)
}
\arguments{
\item{model}{SFC model object}

\item{scenario}{vector of strings or single string name of scenario(s) to simulate}

\item{max_iter}{numeric maximum iterations allowed per period, defaults to 350}

\item{periods}{numeric total number of rows (periods) in the model, defaults to 100}

\item{hidden_tol}{numeric error tolerance to accept the equality of hidden equations, defaults to 0.1.}

\item{tol}{numeric tolerance accepted to determine convergence, defaults to 1e-08}

\item{method}{string name of method used to find solution chosen from: 'Gauss', 'Newton', defaults to 'Gauss'}
}
\value{
updated model containing simulated scenario(s)
}
\description{
Simulate scenario of SFC model object
}