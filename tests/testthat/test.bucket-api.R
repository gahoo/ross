test_that("Bucket", {
  b <- Bucket$new('igenecode', '2017-03-15T07:37:06.000Z', 'oss-cn-beijing', 'Standard')
  expect_equal(b$Name, 'igenecode')
})

###### PUT
test_that("PutBucket", {
  r <- PutBucket('ross-test')
  expect_equal(r$status_code, 200)
  expect_warning(r <- PutBucket('ross-test', StorageClass = 'IA'))
  expect_equal(r$status_code, 409)
  expect_error(PutBucket('ross-test', acl = 'wrong-acl'))
  expect_error(PutBucket('ross-test', StorageClass = 'wrong-class'))
  expect_warning(PutBucket('test'))
})

test_that("PutBucketLogging", {
  r <- PutBucketLogging('ross-test', 'log-', 'ross-test')
  expect_equal(r$status_code, 200)
  r <- PutBucketLogging('ross-test', 'log-')
  expect_equal(r$status_code, 200)
  r <- PutBucketLogging('ross-test', 'log-', on = F)
  expect_equal(r$status_code, 200)
})

test_that("PutBucketWebsite", {
  r <- PutBucketWebsite('ross-test', key='error.html', suffix='index.html')
  expect_equal(r$status_code, 200)
})

test_that("PutBucketReferer", {
  r <- PutBucketReferer('ross-test')
  expect_equal(r$status_code, 200)
  r <- PutBucketReferer('ross-test', AllowEmptyReferer=FALSE, RefererList='http://*.aliyun.com')
  expect_equal(r$status_code, 200)
  r <- PutBucketReferer('ross-test', AllowEmptyReferer=FALSE)
  expect_equal(r$status_code, 200)
})

test_that("PutBucketLifecycle", {
  r <- PutBucketLifecycle('ross-test', Prefix = 'upload_', Object.Days = 30)
  expect_equal(r$status_code, 200)
  r <- PutBucketLifecycle('ross-test', Prefix = 'upload_', Object.CreatedBeforeDate = Sys.Date()+7)
  expect_equal(r$status_code, 200)
  r <- PutBucketLifecycle('ross-test', Prefix = 'upload_', Multpart.Days = 5)
  expect_equal(r$status_code, 200)
  r <- PutBucketLifecycle('ross-test', Prefix = 'upload_', Object.Days = 30, Multpart.Days = 5)
})

###### Get
test_that("GetBucket", {
  r <- GetBucket('ross-test')
  expect_equal(r$status_code, 200)
  r <- GetBucket('ross-test', prefix='test/', marker = 'test/2.txt', max_keys = 5, delimiter = '/')
  expect_equal(r$status_code, 200)
})

test_that("GetBucketAcl", {
  r<-GetBucketAcl('ross-test')
  expect_equal(r$status_code, 200)
})

test_that("GetBucketLocation", {
  r<-GetBucketLocation('ross-test')
  expect_equal(r$status_code, 200)
})

test_that("GetBucketInfo", {
  r<-GetBucketInfo('ross-test')
  expect_equal(r$status_code, 200)
})

test_that("GetBucketLogging", {
  r<-GetBucketLogging('ross-test')
  expect_equal(r$status_code, 200)
})

test_that("GetBucketWebsite", {
  r<-GetBucketWebsite('ross-test')
  expect_equal(r$status_code, 200)
})

test_that("GetBucketReferer", {
  r<-GetBucketReferer('ross-test')
  expect_equal(r$status_code, 200)
})

test_that("GetBucketLifecycle", {
  r<-GetBucketLifecycle('ross-test')
  expect_equal(r$status_code, 200)
})

###### Delete
test_that("DeleteBucketLogging", {
  r<-DeleteBucketLogging('ross-test')
  expect_equal(r$status_code, 204)
})

test_that("DeleteBucketWebsite", {
  r<-DeleteBucketWebsite('ross-test')
  expect_equal(r$status_code, 204)
})

test_that("DeleteBucketLifecycle", {
  r<-DeleteBucketLifecycle('ross-test')
  expect_equal(r$status_code, 204)
})

test_that("DeleteBucket", {
  r <- PutBucket('ross-test')
  expect_equal(r$status_code, 200)
  r <- DeleteBucket('ross-test')
  expect_equal(r$status_code, 204)
})
