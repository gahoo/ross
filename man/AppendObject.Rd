% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/object-api.R
\name{AppendObject}
\alias{AppendObject}
\title{AppendObject}
\usage{
AppendObject(bucketname, key, body = "", position = 0, encryption = NULL,
  acl = "private", ..., .md5 = TRUE, .meta = NULL)
}
\arguments{
\item{bucketname}{The bucket name}

\item{key}{The file path of object on oss.}

\item{body}{The content of object, character or upload_file(file_path).}

\item{position}{The position of of body to append. Should be the same as existing file length.}

\item{encryption}{server side encryption algorithm AES256}

\item{acl}{Access control: private, public-read, public-read-write}

\item{...}{Set header directly. Cache-Control, Content-Disposition, Content-Encoding, Expires, ...}

\item{.md5}{boolean Calculate body md5sum automatically or not.}

\item{.meta}{Other meta info set to the object, < 8k.}
}
\description{
AppendObject
}
\examples{
AppendObject('ross-test', 'test-append.txt', body='1', position = 0)
AppendObject('ross-test', 'test-append.txt', body='2', position = 1, acl = 'public-read', encryption = 'AES256')
AppendObject('ross-test', 'test-append.txt', body='3', position = 2, .md5 = F)
AppendObject('ross-test', 'test-append.txt', body='4', position = 3, .meta = list(location='beijing'))
AppendObject('ross-test', 'test-append.txt', body='5', position = 4, "Content-Encoding"='UTF-8')
}
