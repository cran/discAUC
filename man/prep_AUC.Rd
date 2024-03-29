% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AUC_prep.R
\name{prep_AUC}
\alias{prep_AUC}
\title{AUC data peperation helper function}
\usage{
prep_AUC(dat, indiff = NULL, x_axis, groupings = NULL, prob_disc = FALSE)
}
\arguments{
\item{dat}{Discounting data tibble}

\item{indiff}{Indifference points Variable}

\item{x_axis}{Delays/probabilities/social distance variable}

\item{groupings}{Variables for grouping (e.g., subject, expeirmental group)
as a character or vector of characters}

\item{prob_disc}{Boolean for probability discounting (MAYBE NOT NECESSARY
PULLED OUT ODDS AGAINST TO DIFFERENT FUNCTION)}
}
\value{
Tibble that is grouped and arranged by \code{groupings}
and \code{x_axis} (or \code{x_axis_against}, if probability
discounting)
}
\description{
Helper function to take AUC tibble and preprocess for other AUC
calculations
}
\examples{
library(dplyr)

# Prep single set of data
DD <- tibble(
  delay = c(1 / 7, 1, 2, 4, 26, 52),
  indiff = c(95, 75, 50, 20, 5, 1)
) \%>\%
  arrange(delay)

prep_AUC(dat = DD, indiff = "indiff", x_axis = "delay")
# Prep multiple subject data

# Create DD data disorganize by delay
DD <- tibble(
  delay = rep(c(1 / 7, 1, 2, 4, 26, 52), 2),
  indiff = c(c(95, 75, 50, 20, 5, 1), c(95, 75, 50, 20, 5, 1) + .25),
  sub = c(rep(1, 6), rep(2, 6))
) \%>\%
  arrange(delay)

# Group by subject and organize by subject and delay
prep_AUC(dat = DD, indiff = "indiff", x_axis = "delay", groupings = "sub")


# Probability discounting with subjects and different outcomes

# Create PD data and disorganize by probability
PD <- tibble(
  prob = rep(c(.1, 1 / 100, 1 / 300, 1 / 750, 1 / 1000, 1 / 3000), 4),
  value = rep(c(c(95, 75, 50, 20, 5, 1), c(95, 75, 50, 20, 5, 1) + .25), 2),
  sub = rep(c(rep(1, 6), rep(2, 6)), 2),
  outcome = c(rep("money", 12), rep("cigarettes", 12))
) \%>\%
  arrange(prob)

# Calculate odds against, organize by subject, outcome, odds against
prep_AUC(PD,
  indiff = "value",
  x_axis = "prob",
  groupings = c("sub", "outcome"),
  prob_disc = TRUE
)
}
