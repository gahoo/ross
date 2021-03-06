% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrapper.R
\name{uploadObject}
\alias{uploadObject}
\title{uploadObject}
\usage{
uploadObject(bucketname, src, dest = NULL, resume = TRUE, split = 5,
  maxPartSize = 20 * 1024^2, minMultiSize = 10 * 1024^2,
  .progressbar = TRUE, ...)
}
\arguments{
\item{src}{Path to the local file to be uploaded.}

\item{dest}{Destination path on bucket. Ended with "/" means its a folder.}

\item{resume}{Auto resume from last failed upload or not.}

\item{split}{How many parts to be splited. Will be recacluated along with maxPartSize.}

\item{maxPartSize}{The max size of each part.}

\item{minMultiSize}{File szie greater than minMultiSize will be splited automatically.}

\item{.progressbar}{Show progress bar or not. progress bar only work with multipart upload.}

\item{...}{Other arguments pass to InitiateMultipartUpload.}
}
\description{
uploadObject
}
\examples{
uploadObject('ross-test', 'test.zip')
uploadObject('ross-test', 'test.zip', 'test/test.zip')
}
