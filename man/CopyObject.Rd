% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/object-api.R
\name{CopyObject}
\alias{CopyObject}
\title{CopyObject}
\usage{
CopyObject(source, bucketname, key, encryption = NULL, acl = "private",
  meta.directive = "COPY", ETag = NULL, ETag.match = TRUE, since = NULL,
  modified.since = TRUE, ..., .meta = NULL)
}
\arguments{
\item{source}{The source object. /bucketname/objectname}

\item{bucketname}{Destinate bucket}

\item{key}{Destinate path on bucket}

\item{encryption}{server side encryption algorithm AES256}

\item{acl}{Access control: private, public-read, public-read-write}

\item{meta.directive}{COPY or REPLACE meta info of object. All source meta will be ignore when this value set to REPLACE.}

\item{ETag}{ETag of object.}

\item{ETag.match}{Copy when ETag matches or copy when Etag not match.}

\item{since}{POSIXct time.}

\item{modified.since}{Copy when object was modified since a time, or copy when object was not modified since a time.}

\item{...}{Set header directly. Cache-Control, Content-Disposition, Content-Encoding, Expires, ...}

\item{.meta}{Other meta info set to the object, < 8k.}
}
\description{
CopyObject
}
\examples{
CopyObject('/ross-test/test.txt', 'ross-test', 'test2.txt')
CopyObject('/ross-test/test.txt', 'ross-test', 'test2.txt', encryption = 'AES256')
CopyObject('/ross-test/test.txt', 'ross-test', 'test2.txt', acl = 'public-read')
CopyObject('/ross-test/test.txt', 'ross-test', 'test2.txt', meta.directive = 'REPLACE', .meta = list(owner='igenecode.com'))
CopyObject('/ross-test/test.txt', 'ross-test', 'test2.txt', ETag = 'AAAA', ETag.match = F)
CopyObject('/ross-test/test.txt', 'ross-test', 'test2.txt', since = Sys.time(), modified.since = F)
}
