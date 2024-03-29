% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/10_plot_simulation.R
\name{plot_simulation}
\alias{plot_simulation}
\title{Plot simulations of multiple variables in multiple scenarios}
\usage{
plot_simulation(
  model,
  scenario = "baseline",
  take_all = FALSE,
  from = NA,
  to = NA,
  expressions = "Y"
)
}
\arguments{
\item{model}{SFC model object}

\item{scenario}{vector of strings or single string name of scenario(s) from which take variables values, defaults to 'baseline'}

\item{take_all}{logical indicating whether all scenarios containing the given scenario name string(s) should be used, defaults to FALSE}

\item{from}{numeric period number from which the plot should start, defaults to maximum value}

\item{to}{numeric period number on which the plot should end, defaults to minimum value}

\item{expressions}{vector of strings or single string name of variable(s) expression(s) to plot, defaults to 'Y'}
}
\value{
\code{plotly} plot
}
\description{
Plot simulations of multiple variables in multiple scenarios
}
