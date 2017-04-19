#' Sign Methods
#'
#' @describeIn .build.signature
#' @describeIn .sign.header
#' @describeIn .sign.url
#'
#' @name sign
#' @seealso \code{\link{.build.signature}}, \code{\link{.sign.header}}, \code{\link{.sign.url}}
NULL

#' Build signature for header or url.
#'
#' The signature building function.
#'
#' @param method HTTP request Method. Including PUT, GET, POST, HEAD, DELETE.
#' @param ossresource The resouce path on OSS.
#' @param content_md5 Base64 encoded 128bit MD5 for request content.
#' @param content_type The type of request content. "application/octet-stream" etc.
#' @param expires UNIX time indicating when will the url expires. 1141889060 etc. The current time will be used if NULL.
#' @param ossheaders "x-oss-" prefixed http header dict order.
#' @param SecurityToken Temp token for accessing resource.
#' @param AccessKeySecret Aliyun AccessKeySecret. The secret string to sign.
#'
#' @return signed signature for constructing url or header.
#' @import httr
#' @export
#'
#' @examples
#' .build.signature('GET', '/')
#' .build.signature('GET', '/igenecode/ross/ross.txt', expires = as.integer(Sys.time()) + 60, AccessKeySecret=AccessKeySecret)
#'
#' @seealso \code{\link{.sign.header}}, \code{\link{.sign.url}}
#' @keywords signature
#'
.build.signature <- function(method, ossresource, content_md5 = "", content_type = "",
                            expires = NULL, ossheaders = "", SecurityToken = NULL,
                            AccessKeySecret = Sys.getenv("AccessKeySecret") ){
  if(AccessKeySecret == ""){
    stop('AccessKeySecret is invalid.')
  }

  if(is.null(expires)){
    # Sign in Header, using Date instead of Expires
    expires <- http_date(Sys.time())
  }

  if(!is.null(SecurityToken)){
    # STS sign
  }

  string2sign<- paste(
    method,
    content_md5,
    content_type,
    expires,
    paste0(ossheaders, ossresource),
    sep = '\n')

  if(getOption('ross.debug')){
    print(string2sign)
  }

  hmac_sha1(AccessKeySecret, string2sign)

}

.check.accesskey <- function(AccessKeyId, AccessKeySecret){
  if(AccessKeyId == ""){
    stop('AccessKeyId is invalid.')
  }

  if(AccessKeySecret == ""){
    stop('AccessKeySecret is invalid.')
  }
}

#' Sign header for requests.
#'
#' @inheritParams .build.signature
#' @param url The URL wanted to sign.
#' @param AccessKeyId Aliyun AccessKeyId
#' @param ...
#'
#' @describeIn .build.signature
#'
#' @return response of requests
#' @import httr
#' @export
#'
#' @examples
#' @seealso \code{\link{.sign.url}}
#' @keywords sign.header
.sign.header <- function(method, url, ossresource,
                         AccessKeyId = Sys.getenv("AccessKeyId"),
                         AccessKeySecret = Sys.getenv("AccessKeySecret"),
                         query = NULL,
                         body = NULL,
                         .headers = character(),
                         ...
                         ){

  .check.accesskey(AccessKeyId, AccessKeySecret)

  date <- http_date(Sys.time())
  signature <- .build.signature(method, ossresource,
                                content_md5 = .extract.header('Content-MD5', .headers),
                                content_type = .extract.header('Content-Type', .headers),
                                expires = date,
                                AccessKeySecret = AccessKeySecret,
                                ...)
  authorization <- sprintf("OSS %s:%s", AccessKeyId, signature)
  headers <- add_headers(date = date, authorization = authorization, .headers=.headers)

  response <- do.call(method, args = list(url, headers, query = query, body=body, user_agent("ross 0.0.1")))
  .check.http_error(response)
  response
}

#' Sign URL for requests.
#'
#' @inheritParams .build.signature
#' @inheritParams .sign.header
#' @param expires How long will the url expires in seconds.
#' @param ...
#' @param .url boolean Only URL will return when TRUE.
#'
#' @return response of requests
#' @export
#'
#' @examples
#' @seealso \code{\link{.sign.header}}
#' @keywords sign.url
.sign.url <- function(method, url, ossresource, expires = 60,
                      AccessKeyId = Sys.getenv("AccessKeyId"),
                      AccessKeySecret = Sys.getenv("AccessKeySecret"),
                      query = NULL,
                      body = NULL,
                      .headers = character(),
                      ..., .url=FALSE){
  .check.accesskey(AccessKeyId, AccessKeySecret)

  expires <- as.integer(Sys.time()) + expires
  signature <- .build.signature(method, ossresource,
                                content_md5 = .extract.header('Content-MD5', .headers),
                                content_type = .extract.header('Content-Type', .headers),
                                expires = expires,
                                AccessKeySecret = AccessKeySecret,
                                ...)

  query <- list(
    OSSAccessKeyId = AccessKeyId,
    Expires = expires,
    Signature = signature)

  if(.url){
    modify_url(url, query=query)
  }else{
    response <- do.call(method, args = list(url = url, query = query))
    .check.http_error(response)
    response
  }

}

