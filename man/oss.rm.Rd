% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oss.R
\name{oss.rm}
\alias{oss.rm}
\title{oss.rm}
\usage{
oss.rm(x, ...)
}
\arguments{
\item{x}{oss path, Bucket}

\item{...}{}

\item{confirm}{auto confirm}
}
\description{
oss.rm
}
\examples{
oss.rm(oss('oss://ross-test/test.txt'))
oss.rm('oss://ross-test/test.txt', confirm=T)
oss.rm('oss://ross-test/', confirm=T)
oss.rm('oss://ross-test/upload/', confirm=T)
b <- Bucket$new('ross-test')
oss.rm(b)
}
