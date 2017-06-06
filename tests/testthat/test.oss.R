test_that("oss", {
  expect_silent(x <- oss('oss://ross-test'))
  expect_true(is.null(x$key))
  expect_silent(x <- oss('oss://ross-test/test.txt'))
  expect_silent(oss(x))
  expect_equal(x$bucket, 'ross-test')
  expect_equal(x$key, 'test.txt')
  expect_error(oss('ross'))
})

test_that("oss.ls", {
  expect_silent(oss.ls())
  try(oss.rm('oss://ross-test', confirm=T, .all=T))
  oss.mb('oss://ross-test')
  oss.write('oss://ross-test/test.txt', 'test')
  suppressMessages(expect_equal(nrow(oss.ls('oss://ross-test')), 1))
  oss.write('oss://ross-test/test/1.txt', 'test')
  oss.write('oss://ross-test/test/2.txt', 'test')
  suppressMessages(expect_equal(nrow(oss.ls('oss://ross-test/test/')), 2))
  b <- Bucket$new('ross-test')
  expect_true('test.txt' %in% oss.ls(b, .output='character'))
  o <- Object$new('ross-test', 'test.txt')
  expect_true("test.txt" %in% oss.ls(o, .output='character'))
  oss.rm('oss://ross-test', confirm=T, .all=T)
})

test_that("oss.mb", {
  oss.rm('oss://ross-test', confirm=T, .all=T)
  oss.mb('oss://ross-test')
  expect_true(oss.exists('oss://ross-test'))
  oss.rm('oss://ross-test', confirm=T, .all=T)
  expect_false(oss.exists('oss://ross-test'))
  b <- Bucket$new('ross-test')
  oss.mb(b)
  oss.rm(b)
})

test_that("oss.rm/oss.exists", {
  try(oss.rm('oss://ross-test', confirm=T, .all=T))
  oss.mb('oss://ross-test')
  oss.write('oss://ross-test/test.txt', '')
  oss.write('oss://ross-test/test2.txt', '')
  oss.write('oss://ross-test/test3.txt', '')

  expect_true(oss.exists('oss://ross-test/test.txt'))
  expect_silent(oss.rm('oss://ross-test/test.txt', confirm=T))
  expect_false(oss.exists('oss://ross-test/test.txt'))

  o <- Object$new('ross-test', 'test2.txt')
  expect_true(oss.exists(o))
  expect_silent(oss.rm(o))
  expect_false(oss.exists(o))

  expect_warning(oss.rm('oss://ross-test/', confirm=T))
  expect_silent(oss.rm('oss://ross-test/', confirm=T, .all=T))

  b <- Bucket$new('ross-test', autoCreate=T)
  expect_true(oss.exists(b))
  expect_silent(oss.rm(b))
  expect_false(oss.exists(b))
})

test_that("oss.cp", {
  is_file <- function(x){file.exists(x) && !dir.exists(x)}
  is_online <- function(x){
    r <- HeadObject('ross-test', x)
    r$status_code == 200
  }
  createBucket('ross-test')
  removeObjects('ross-test', confirm = T)
  tempfiles <- c('test.txt', sprintf('folder/test%s.txt', 1:3), sprintf('deep/folder2/test%s.txt', 1:3))
  for(tmpfile in tempfiles){
    r<-PutObject('ross-test', tmpfile)
  }
  dir.create('/Volumes/RamDisk/ross')
  # Single File Download
  # dest absolute path
  oss.cp('oss://ross-test/test.txt', '/Volumes/RamDisk/ross/')
  expect_true(is_file('/Volumes/RamDisk/ross/test.txt'))
  oss.cp('oss://ross-test/test.txt', '/Volumes/RamDisk/ross/test')
  expect_true(is_file('/Volumes/RamDisk/ross/test'))
  oss.cp('oss://ross-test/folder/test1.txt', '/Volumes/RamDisk/ross/folder/')
  expect_true(is_file('/Volumes/RamDisk/ross/folder/test1.txt'))
  oss.cp('oss://ross-test/folder/test2.txt', '/Volumes/RamDisk/ross/folder/test22.txt')
  expect_true(is_file('/Volumes/RamDisk/ross/folder/test22.txt'))
  # dest relative path
  old<-getwd()
  setwd('/Volumes/RamDisk/ross')
  oss.cp('oss://ross-test/folder/test3.txt', '.')
  expect_true(is_file('test3.txt'))
  oss.cp('oss://ross-test/folder/test3.txt', 'dtest3/deep/')
  expect_true(is_file('dtest3/deep/test3.txt'))

  # Multiple files download
  # dest absolute path
  oss.cp('oss://ross-test/deep/folder2', '/Volumes/RamDisk/ross/')
  expect_true(is_file('folder2/test1.txt'))
  oss.cp('oss://ross-test/deep', '/Volumes/RamDisk/ross/')
  expect_true(is_file('deep/folder2/test1.txt'))
  oss.cp('oss://ross-test/deep', '/Volumes/RamDisk/ross/deep2')
  expect_true(is_file('deep2/folder2/test1.txt'))
  # dest relative path
  oss.cp('oss://ross-test/folder', '.')
  expect_true(is_file('folder/test1.txt'))
  oss.cp('oss://ross-test/folder', 'renamed')
  expect_true(is_file('renamed/test1.txt'))
  oss.cp('oss://ross-test/folder', 'deepper/')
  expect_true(is_file('deepper/folder/test1.txt'))
  oss.cp('oss://ross-test/deep/folder2', 'deep/renamed')
  expect_true(is_file('deep/renamed/test1.txt'))
  oss.cp('oss://ross-test/deep', 'deep3/')
  expect_true(is_file('deep3/deep/folder2/test1.txt'))

  # Single File upload
  # src relative
  removeObjects('ross-test', confirm = T)
  oss.cp('test.txt', 'oss://ross-test/')
  expect_true(is_online('test.txt'))
  oss.cp('test.txt', 'oss://ross-test/test')
  expect_true(is_online('test'))
  oss.cp('folder/test1.txt', 'oss://ross-test/test_folder/')
  expect_true(is_online('test_folder/test1.txt'))
  oss.cp('folder/test2.txt', 'oss://ross-test/test_folder/test')
  expect_true(is_online('test_folder/test'))
  # src absolute
  oss.cp('/Volumes/RamDisk/ross/folder/test3.txt', 'oss://ross-test/test_folder/test3.txt')
  expect_true(is_online('test_folder/test3.txt'))

  # Multiple file upload
  # src relative
  oss.cp('folder', 'oss://ross-test/')
  expect_true(is_online('folder/test1.txt'))
  oss.cp('folder2/', 'oss://ross-test/')
  expect_true(is_online('folder2/test1.txt'))
  oss.cp('folder2/', 'oss://ross-test/folder3')
  expect_true(is_online('folder3/test1.txt'))
  oss.cp('folder2/', 'oss://ross-test/folder4/')
  expect_true(is_online('folder4/folder2/test1.txt'))
  oss.cp('deep3/deep', 'oss://ross-test/')
  expect_true(is_online('deep/folder2/test1.txt'))
  oss.cp('deep3/deep', 'oss://ross-test/deepper')
  expect_true(is_online('deepper/folder2/test1.txt'))
  oss.cp('deep3/deep', 'oss://ross-test/deepper/')
  expect_true(is_online('deepper/deep/folder2/test1.txt'))
  # src absolute
  oss.cp('/Volumes/RamDisk/ross/deep2/', 'oss://ross-test/')
  expect_true(is_online('deep2/folder/test1.txt'))
  oss.cp('/Volumes/RamDisk/ross/deep2/', 'oss://ross-test/deep4')
  expect_true(is_online('deep4/folder/test1.txt'))
  oss.cp('/Volumes/RamDisk/ross/deep2/', 'oss://ross-test/deep4/')
  expect_true(is_online('deep4/deep2/folder/test1.txt'))

  # Online Copy
  # Single small file copy
  removeObjects('ross-test', confirm = T)
  r <- PutObject('ross-test', 'test.txt')
  oss.cp('oss://ross-test/test.txt', 'oss://ross-test/test2.txt')
  expect_true(is_online('test2.txt'))
  oss.cp('oss://ross-test/test.txt', 'oss://ross-test/another/test2.txt')
  expect_true(is_online('another/test2.txt'))

  removeObjects('ross-test', confirm = T)
  unlink('/Volumes/RamDisk/ross/')
  deleteBucket('ross-test')
})

test_that("oss.ln", {
  oss.rm('oss://ross-test', confirm=T, .all=T)
  oss.mb('oss://ross-test')
  expect_silent(oss.ln('oss://ross-test/linked-test.txt', 'oss://ross-test/test.txt'))
  expect_equal(oss.ln('oss://ross-test/linked-test.txt'), "/ross-test/test.txt")
  expect_silent(oss.ln('oss://ross-test/linked-test.txt', '/ross-test/test2.txt'))
  expect_equal(oss.ln('oss://ross-test/linked-test.txt'), "/ross-test/test2.txt")
  o <- Object$new('ross-test', 'linked-test2.txt')
  expect_silent(oss.ln(o, '/ross-test/test3.txt'))
  expect_equal(oss.ln(o), "/ross-test/test3.txt")
  oss.rm('oss://ross-test', confirm=T, .all=T)
})

test_that("oss.restore", {
  oss.mb('oss://ross-test-arch', StorageClass='Archive')
  oss.write('oss://ross-test-arch/test.txt', 'test')
  expect_silent(r <- oss.restore('oss://ross-test-arch/test.txt'))
  expect_equal(r$status_code, 202)
  o <- Object$new('ross-test-arch', 'test.txt')
  expect_warning(oss.restore(o))
  oss.rm('oss://ross-test-arch', confirm=T, .all=T)
})

test_that("oss.acl" {
  oss.mb('oss://ross-test')
  oss.write('oss://ross-test/test.txt', '')
  expect_equal(oss.acl('oss://ross-test/test.txt'), 'private')
  expect_silent(oss.acl('oss://ross-test/test.txt', 'public-read'))
  expect_equal(oss.acl('oss://ross-test/test.txt'), 'public-read')
  oss.rm('oss://ross-test')
})
