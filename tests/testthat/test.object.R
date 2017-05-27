test_that("Object",{
  createBucket('ross-test')
  expect_silent(o <- Object$new('ross-test', 'test.txt'))
  # write
  expect_silent(r <- o$write('test'))
  expect_equal(r$status_code, 200)
  # read
  expect_identical(o$read(), 'test')
  # url
  expect_silent(o$url())
  # append
  expect_warning(o$append('1'), 'ObjectNotAppendable')
  expect_silent(o$delete())
  expect_silent(o$append('1'))
  expect_silent(o$append('2'))
  expect_silent(o$append('3'))
  expect_identical(o$read(), '123')
  # save
  a <- 1; b <- 2
  o$save(a, b)
  # load
  e <- new.env()
  expect_silent(o$load(envir = e))
  expect_true(all(c('a', 'b') %in% ls(e)))
  # saveRDS
  expect_silent(o$saveRDS(1:10))
  # readRDS
  expect_identical(o$readRDS(), 1:10)
  # copy
  o$copyTo('ross-test', 'test2.txt')
  o$copyFrom('ross-test', 'test2.txt')
  # link
  expect_error(o$link)
  expect_silent(ol <- Object$new('ross-test', 'linked-test.txt'))
  expect_silent(ol$link <- '/ross-test/test.txt')
  expect_equal(ol$link, '/ross-test/test.txt')
  # delete
  expect_silent(o$delete())
  deleteBucket('ross-test')
})

