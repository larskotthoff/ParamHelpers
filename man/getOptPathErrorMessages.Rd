% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/OptPath_getter.R
\name{getOptPathErrorMessages}
\alias{getOptPathErrorMessages}
\title{Get error-message vector from the optimization path.}
\usage{
getOptPathErrorMessages(op, dob, eol)
}
\arguments{
\item{op}{[\code{\link{OptPath}}]\cr
Optimization path.}

\item{dob}{[\code{integer}]\cr
Vector of date-of-birth values to further subset the result.
Only elements with a date-of-birth included in \code{dob} are selected.
Default is all.}

\item{eol}{[\code{integer}]\cr
Vector of end-of-life values to further subset the result.
Only elements with an end-of-life included in \code{eol} are selected.
Default is all.}
}
\value{
[\code{character}].
}
\description{
Get error-message vector from the optimization path.
}
\seealso{
Other optpath: \code{\link{OptPath}},
  \code{\link{OptPathDF}}, \code{\link{makeOptPathDF}};
  \code{\link{addOptPathEl}};
  \code{\link{getOptPathBestIndex}};
  \code{\link{getOptPathCols}};
  \code{\link{getOptPathCol}}; \code{\link{getOptPathDOB}};
  \code{\link{getOptPathEOL}}; \code{\link{getOptPathEl}};
  \code{\link{getOptPathExecTimes}};
  \code{\link{getOptPathLength}};
  \code{\link{getOptPathParetoFront}};
  \code{\link{getOptPathX}}; \code{\link{getOptPathY}};
  \code{\link{setOptPathElDOB}};
  \code{\link{setOptPathElEOL}}
}

