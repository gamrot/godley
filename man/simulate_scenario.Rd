% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/07_simulate_scenario.R
\name{simulate_scenario}
\alias{simulate_scenario}
\title{Simulate scenario of SFC model object}
\usage{
simulate_scenario(
  model,
  scenario,
  periods = NA,
  start_date = NA,
  method = "Gauss",
  max_iter = 350,
  tol = 1e-05,
  hidden_tol = 0.1,
  verbose = FALSE
)
}
\arguments{
\item{model}{SFC model object}

\item{scenario}{vector of strings or single string name of scenario(s) to simulate}

\item{periods}{numeric total number of rows (periods) in the model, defaults to 100}

\item{start_date}{character date to begin the simulation in the format "yyyy-mm-dd"}

\item{method}{string name of method used to find solution chosen from: 'Gauss', 'Newton', defaults to 'Gauss'}

\item{max_iter}{numeric maximum iterations allowed per period, defaults to 350}

\item{tol}{numeric tolerance accepted to determine convergence, defaults to 1e-05}

\item{hidden_tol}{numeric error tolerance to accept the equality of hidden equations, defaults to 0.1.}

\item{verbose}{logical to tell if additional model verbose should be displayed}
}
\value{
updated model containing simulated scenario(s)
}
\description{
Simulate scenario of SFC model object
}
