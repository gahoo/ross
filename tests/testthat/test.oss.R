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
  oss.cp('oss://ross-test/deep/folder', 'deep/renamed')
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
})
