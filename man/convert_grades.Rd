% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_grades.R
\name{convert_grades}
\alias{convert_grades}
\title{Convert Grades to Marks}
\usage{
convert_grades(grades, to = NA, scale = glasgow22())
}
\arguments{
\item{grades}{vector of letter or number grades}

\item{to}{convert to `letters` or `numbers` (default NA converts to other type)}

\item{scale}{conversion table containing `letters` and `numbers`}
}
\value{
vector
}
\description{
\code{convert_grades} convert letter grades to numeric marks
}
\examples{
convert_grades(marking_example$marks$Grade)
}
