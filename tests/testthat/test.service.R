test_that("GetService", {
  r<-GetService()
  expect_false(http_error(r))
})

test_that("BucketList", {
  b<-BucketList$new()
  expect_false(http_error(b$response))
})
