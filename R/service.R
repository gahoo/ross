#' Title
#'
#' @param prefix
#' @param marker
#' @param max_keys
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
