% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mark_dist.R
\name{mark_dist}
\alias{mark_dist}
\title{Marking Distribution Graph}
\usage{
mark_dist(marking, by = FALSE)
}
\arguments{
\item{marking}{list containing a dataframe (marks)}

\item{by}{facet graphs by question or marker (default FALSE)}
}
\value{
ggplot
}
\description{
\code{mark_dist} create a graph of the marking distribution
}
\examples{
mark_dist(marking_example)
}
