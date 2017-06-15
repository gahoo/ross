test_that("Object",{
  createBucket('ross-test')
  expect_silent(o <- Object$new('ross-test', 'test.txt'))
  # write
  expect_silent(r <- o$write('test'))
  expect_equal(r$status_code, 200)
  # read
  expect_identical(o$read(), 'test')
  expect_identical(o$read(2), 'te')
  expect_identical(o$read(3), 'st')
  expect_equal(length(o$read(2)), 0)
  expect_silent(o$seek(1))
  expect_equal(o$read(3), 'est')
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
  # download
  tmp <- tempfile()
  expect_silent(o$download(tmp, quiet=T))
  # upload
  expect_silent(o$upload(tmp))
  # copy
  o$copyTo('ross-test', 'test2.txt')
  o$copyFrom('ross-test', 'test2.txt')
  # acl
  expect_equal(o$acl, 'private')
  expect_silent(o$acl <- 'public-read')
  expect_equal(o$acl, 'public-read')
  # link
  expect_error(o$link)
  expect_silent(ol <- Object$new('ross-test', 'linked-test.txt'))
  expect_silent(ol$link <- '/ross-test/test.txt')
  expect_equal(ol$link, '/ross-test/test.txt')
  # meta
  expect_silent(o$meta <- list(a=1, b=2))
  expect_identical(o$meta, list(a='1', b='2'))
  expect_silent(o$meta <- list(c=3))
  expect_identical(o$meta, list(a='1', b='2', c='3'))
  expect_silent(o$meta <- list(c=NULL))
  expect_identical(o$meta, list(a='1', b='2'))
  expect_silent(o$meta <- NULL)
  expect_length(o$meta, 0)
  # exists
  expect_true(o$exists())
  # move
  expect_silent(o$moveTo('ross-test', 'mv.txt'))
  expect_equal(o$key, 'mv.txt')
  # delete
  expect_silent(o$delete())
  expect_false(o$exists())
  deleteBucket('ross-test')
})

