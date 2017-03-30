test_that("Bucket", {
  expect_error(b <- Bucket$new('ross-test'))
  expect_error(b$rm())
  expect_message(b <- Bucket$new('ross-test', autoCreate=T))
  expect_silent(b$rm())
  # autoCreate=F
  expect_error(b <- Bucket$new('ross-test', autoCreate=F))
  # create
  expect_message(b$create(acl='public-read', StorageClass = 'IA'))
  expect_silent(b$refresh())
  expect_silent(b$list())
  expect_silent(b$rm())

})


test_that("Bucket$acl", {
  expect_message(b<-Bucket$new('ross-test', acl='public-read', autoCreate=T))
  expect_equal(b$acl, 'public-read')
  expect_silent(b$acl<-"private")
  expect_equal(b$acl, "private")
  expect_silent(b$rm())
})


test_that("Bucket$logging", {
  expect_message(b <- Bucket$new('ross-test', autoCreate=T))
  conf <- list(TargetBucket='ross-test', TargetPrefix='log-')
  expect_silent(b$logging <- conf)
  expect_equal(b$logging, conf)
  expect_silent(b$logging <- list())
  expect_silent(b$logging <- list(TargetPrefix='log-'))
  expect_equal(b$logging, conf)
  expect_silent(b$logging <- NULL)
  expect_silent(b$rm())
})
