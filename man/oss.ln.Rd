% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oss.R
\name{oss.ln}
\alias{oss.ln}
\title{oss.ln}
\usage{
oss.ln(x, ...)
}
\arguments{
\item{...}{}
}
\description{
oss.ln
}
\examples{
oss.ln('oss://ross-test/linked-test.txt', 'oss://ross-test/test.txt')
oss.ln('oss://ross-test/linked-test.txt')
oss.ln('oss://ross-test/linked-test.txt', '/ross-test/test2.txt')
oss.ln('oss://ross-test/linked-test.txt')

o <- Object$new('ross-test', 'linked-test2.txt')
oss.ln(o, '/ross-test/test3.txt')
oss.ln(o)
}
