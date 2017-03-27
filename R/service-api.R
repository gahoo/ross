#' GetService
#'
#' @param prefix The prefix of buckets to filter.
#' @param marker Which index to start with.
#' @param max_keys Max number of buckets.
#'
#' @return
#' @export
#'
#' @examples
GetService <- function(prefix=NULL, marker=NULL, max_keys=100){
  response <- .sign.header('GET', "http://oss.aliyuncs.com", "/",
                           query=list(prefix=prefix,
                                      marker=marker,
                                      "max-keys"=max_keys))

  .check.http_error(response)

  response
}

#' ListBucket
#'
#' @inheritParams GetService
#'
#' @describeIn GetService
#'
#' @return
#' @export
#'
#' @examples
listBucket <- function(...) {
  GetService(...)
}
