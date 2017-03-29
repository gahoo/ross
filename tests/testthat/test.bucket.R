test_that("Bucket", {
  expect_silent(b <- Bucket$new('ross-test'))
  expect_warning(b$rm())
  expect_message(b <- Bucket$new('ross-test', autoCreate=T))
  expect_silent(b$rm())
  # autoCreate=F
  expect_silent(b <- Bucket$new('ross-test', autoCreate=F))
  # create
  expect_message(b$create(acl='public-read', StorageClass = 'IA'))
  expect_silent(b$refresh())
  expect_silent(b$list())
  # acl
  expect_equal(b$acl, 'public-read')
  expect_silent(b$acl<-"private")
  expect_equal(b$acl, "private")
  expect_silent(b$rm())
})
