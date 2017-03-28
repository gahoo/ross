test_that("Bucket", {
  b <- Bucket$new('igenecode', '2017-03-15T07:37:06.000Z', 'oss-cn-beijing', 'Standard')
  expect_equal(b$Name, 'igenecode')
})

test_that("PutBucket", {
  r <- PutBucket('igenecode')
  expect_equal(r$status_code, 200)
  expect_error(PutBucket('igenecode', acl = 'wrong-acl'))
  expect_error(PutBucket('igenecode', StorageClass = 'wrong-class'))
  expect_warning(PutBucket('test'))
})

test_that("DeleteBucket", {
  r <- PutBucket('ross-test', StorageClass = 'IA')
  expect_equal(r$status_code, 200)
  r <- DeleteBucket('ross-test')
  expect_equal(r$status_code, 204)
})

test_that("PutBucketLogging", {
  r <- PutBucket('ross-test')
  r <- PutBucketLogging('ross-test', 'log-', 'ross-test')
  expect_equal(r$status_code, 200)
  r <- PutBucketLogging('ross-test', 'log-')
  expect_equal(r$status_code, 200)
  r <- PutBucketLogging('ross-test', 'log-', on = F)
  expect_equal(r$status_code, 200)
})

test_that("", {

})
