#' Bucket
#'
#' @docType class
#' @format An R6 class object.
#' @importFrom R6 R6Class
#' @import xml2
#' @import httr
#' @export
#' @name Bucket
#'

Bucket <- R6::R6Class("Bucket",
  public = list(
    Name = NULL,
    CreationDate = NULL,
    Location = NULL,
    ExtranetEndpoint = NULL,
    IntranetEndpoint = NULL,
    StorageClass = NULL,
    initialize = function(Name, CreationDate, Location, StorageClass) {
      self$Name = Name
      self$CreationDate = CreationDate
      self$Location = Location
      self$ExtranetEndpoint = .build.endpoint(Location, internal=FALSE)
      self$IntranetEndpoint = .build.endpoint(Location, internal=TRUE)
      self$StorageClass = StorageClass
    },
    print = function(...) {
      bucket_text <- sprintf(paste(
          "<Bucket>",
          "Name: %s",
          "CreationDate: %s",
          "Location: %s",
          "Endpoint: %s",
          "StorageClass: %s",
          sep = "\n"),
          self$Name, self$CreationDate, self$Location, self$ExtranetEndpoint, self$StorageClass)
      cat(bucket_text)
    }
  ),
  private = list(
  )
)
