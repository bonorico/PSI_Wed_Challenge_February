% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeplot_shiny.R
\name{makeplot}
\alias{makeplot}
\title{Plot Wednesday challenge data (no animation)}
\usage{
makeplot(df, basehaz)
}
\arguments{
\item{df}{data.frame - output of 'duration_sim'}

\item{basehaz}{numeric - value of 'baseline_haz' variable. Range should
restrict to output of 'duration_sim'}
}
\value{
ggplot
}
\description{
Plot Wednesday challenge data (no animation)
}
\details{
Meant to be used within Shiny app
}
