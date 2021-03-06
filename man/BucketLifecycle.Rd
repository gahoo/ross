% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bucket.R
\docType{class}
\name{BucketLifecycle}
\alias{BucketLifecycle}
\alias{BucketLifecycle}
\alias{BucketCORS}
\title{BucketLifecycle}
\format{An R6 class object.}
\usage{
BucketLifecycle

BucketCORS
}
\description{
Convenient ways to manipulate lifecycle rules

Convenient ways to manipulate CORS rules
}
\examples{

life<-BucketLifecycle$new('ross-test', autoSave=T)
life$add('upload_', Object.CreatedBeforeDate = "2017-04-01")
life$add('upload_', Object.Days=5)
life$add('upload_', Object.Days=7) # Modify on add
life$add('backup_', ID='backup-1', Object.Days=90)
life$add('Backup_', ID='backup-1', Object.Days=90) # Overwirte ID backup-1 rules
life
# Remove rules
life$remove('upload_')
life$remove(ID='backup-1') #By ID
# Clean up all rules
life$clear()

# speedup without autoSave
life<-BucketLifecycle$new('ross-test', F)
life$add('backup1_', ID='backup-1', Object.Days=90)
life$add('backup2_', ID='backup-2', Object.Days=90)
life$add('backup3_', ID='backup-3', Object.Days=90)
life$save()
life

}
\keyword{datasets}
