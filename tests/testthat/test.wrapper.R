test_that("aclBucket", {
  expect_message(createBucket('ross-test'), 'ross-test with private')
  expect_equal(aclBucket('ross-test'), 'private')
  expect_silent(aclBucket('ross-test', 'public-read'))
  expect_equal(aclBucket('ross-test'), 'public-read')
  removeObjects('ross-test', confirm=TRUE)
  deleteBucket('ross-test')
})

test_that("aclObject", {
  expect_message(createBucket('ross-test'), 'ross-test with private')
  expect_silent(r<-PutObject('ross-test', 'test.txt'))
  expect_equal(aclObject('ross-test', 'test.txt'), 'private')
  expect_silent(aclObject('ross-test', 'test.txt', 'public-read'))
  expect_equal(aclObject('ross-test', 'test.txt'), 'public-read')
  removeObjects('ross-test', confirm=TRUE)
  deleteBucket('ross-test')
})

test_that("usageBucket", {
  createBucket('ross-test')
  removeObjects('ross-test', confirm=TRUE)
  upload_path <- 'tests/test_upload/multiplefiles/will_success/'
  expect_output(r <- uploadMultipleObjects('ross-test', upload_path,  .parallel = T), '100%')
  expect_equal(usageBucket('ross-test'), 3.814697e-05)
  expect_equal(usageBucket('ross-test', 'will_success/1/'), 7.629395e-06)
  expect_equal(usageBucket('ross-test', unit = 'B'), 40)
  expect_equal(usageBucket('ross-test', unit = 'KB'), 0.0390625)
  expect_warning(deleteBucket('ross-test'), 'not empty')
  deleteBucket('ross-test')
})

test_that("listBucket, removeObjects, usageBucket", {
  createBucket('ross-test')
  removeObjects('ross-test', confirm=TRUE)
  # listBucket
  upload_path <- 'tests/test_upload/multiplefiles/will_success/'
  expect_output(r <- uploadMultipleObjects('ross-test', upload_path,  .parallel = T), '100%')
  expect_true('ross-test' %in% listBucket()$Name)
  expect_equal(nrow(listBucket('ross-test')), 1)
  expect_equal(nrow(listBucket('ross-test', 'will_success')), 1)
  expect_equal(nrow(listBucket('ross-test', 'will_success/')), 5)
  expect_equal(nrow(listBucket('ross-test', 'will_success/', delimiter = '')), 20)
  expect_message(listBucket('ross-test', 'will_success/', marker = 'will_success/3', delimiter = '/', max_keys = 1), '3 objects listed.')
  listBucket('ross-test', 'will_success/', marker = 'will_success/3', delimiter = '/', max_keys = 1, .all = F)
  # removeObjects
  expect_silent(removeObjects('ross-test', 'will_success', confirm=TRUE))
  expect_equal(nrow(listBucket('ross-test', 'will_success')), 1)
  expect_silent(removeObjects('ross-test', 'will_success/1/', confirm=TRUE))
  expect_equal(nrow(listBucket('ross-test', 'will_success/')), 4)
  expect_silent(removeObjects('ross-test', confirm=TRUE))
  expect_equal(nrow(listBucket('ross-test')), 0)
  deleteBucket('ross-test')
})

test_that("upload/download", {
  createBucket('ross-test')
  removeObjects('ross-test', confirm=TRUE)
  # uploadObject Auto Path test
  expect_silent(r <- uploadObject('ross-test', 'tests/test_upload/test.txt'))
  r <- HeadObject('ross-test', 'tests/test_upload/test.txt')
  expect_equal(r$status_code, 200)
  expect_silent(r <- uploadObject('ross-test', 'tests/test_upload/test.txt', 'test/'))
  r <- HeadObject('ross-test', 'test/test.txt')
  expect_equal(r$status_code, 200)
  expect_silent(r <- uploadObject('ross-test', 'tests/test_upload/test.txt', 'test/test2.txt'))
  r <- HeadObject('ross-test', 'test/test2.txt')
  expect_equal(r$status_code, 200)
  expect_silent(r <- uploadObject('ross-test', file.path(getwd(),'tests/test_upload/test.txt'), 'test/test3.txt'))
  r <- HeadObject('ross-test', 'test/test3.txt')
  expect_equal(r$status_code, 200)
  expect_silent(r <- uploadObject('ross-test', 'tests/test_upload/test.txt', 'test/test4.txt', resume = F))
  r <- HeadObject('ross-test', 'test/test4.txt')
  expect_equal(r$status_code, 200)
  expect_error(r <- uploadObject('ross-test', 'tests/test_upload/not-exist', 'test/test4.txt', resume = F))

  # Multipart Upload
  expect_output(r <- uploadObject('ross-test', 'tests/test_upload/bigfiles/jingyi.pdf', 'bigfiles/', resume = F), '100%')
  r <- HeadObject('ross-test', 'bigfiles/jingyi.pdf')
  expect_equal(r$status_code, 200)
  expect_output(r <- uploadObject('ross-test', 'tests/test_upload/bigfiles/jingyi.pdf', 'bigfiles/', split = 10, maxPartSize = 1024^2), '100%')
  expect_silent(r <- uploadObject('ross-test', 'tests/test_upload/bigfiles/jingyi.pdf', 'bigfiles/', .progressbar = F))
  expect_equal(r$status_code, 200)
  expect_silent(r <- uploadObject('ross-test', 'tests/test_upload/bigfiles/jingyi.pdf', 'bigfiles/', minMultiSize = 1024^3))
  expect_equal(r$status_code, 200)

  old <- getwd()
  setwd('/Volumes/RamDisk/')
  expect_silent(r<-downloadObject('ross-test', 'test/not-exist', quiet = T))
  expect_equal(r, 3)

  expect_silent(r<-downloadObject('ross-test', 'test/test2.txt', quiet = T))
  expect_equal(r, 0)
  expect_true(file.exists('test2.txt'))

  expect_silent(r<-downloadObject('ross-test', 'test/test2.txt', 'test/test2.txt', quiet = T))
  expect_equal(r, 0)
  expect_true(file.exists('test/test2.txt'))

  dir.create('test2')
  expect_silent(r<-downloadObject('ross-test', 'test/test2.txt', 'test2/', quiet = T))
  expect_equal(r, 0)
  expect_true(file.exists('test2/test2.txt'))

  expect_silent(r<-downloadObject('ross-test', 'test/test2.txt', resume = F, method='wget', extra = '-q'))
  expect_equal(r, 0)

  expect_silent(r<-downloadObject('ross-test', 'test/test2.txt', method='wget', quiet = T))
  expect_equal(r, 0)

  r<-downloadObject('ross-test', 'test/test2.txt', '/Volumes/RamDisk/test2.txt', resume = F, method = 'wget', quiet = T)
  expect_equal(r, 0)
  expect_true(file.exists('/Volumes/RamDisk/test2.txt'))
  setwd(old)

  removeObjects('ross-test', confirm=TRUE)
  deleteBucket('ross-test')
})

test_that("mulitple upload/download", {
  createBucket('ross-test')
  removeObjects('ross-test', confirm=TRUE)
  # Auto Path test
  expect_output(r <- uploadMultipleObjects('ross-test', 'tests/test_upload/multiplefiles/will_success/', split=10), '100%')
  expect_output(r <- uploadMultipleObjects('ross-test', 'tests/test_upload/multiplefiles/will_success/', .parallel = F), '100%')
  r <- HeadObject('ross-test', 'will_success/1/1.txt')
  expect_equal(r$status_code, 200)

  expect_output(r <- uploadMultipleObjects('ross-test', 'tests/test_upload/multiplefiles/will_success/', 'success'), '100%')
  r <- HeadObject('ross-test', 'success/1/1.txt')
  expect_equal(r$status_code, 200)

  expect_output(r <- uploadMultipleObjects('ross-test', 'tests/test_upload/multiplefiles/will_success/', 'success/'), '100%')
  r <- HeadObject('ross-test', 'success/will_success/1/1.txt')
  expect_equal(r$status_code, 200)

  # absolute path test
  removeObjects('ross-test', confirm=TRUE)
  upload_path <- file.path(getwd(), 'tests/test_upload/multiplefiles/will_success/')
  expect_output(r <- uploadMultipleObjects('ross-test', upload_path,  .parallel = T), '100%')
  r <- HeadObject('ross-test', 'will_success/1/1.txt')
  expect_equal(r$status_code, 200)

  expect_output(r <- uploadMultipleObjects('ross-test', upload_path, 'success'), '100%')
  r <- HeadObject('ross-test', 'success/1/1.txt')
  expect_equal(r$status_code, 200)

  expect_output(r <- uploadMultipleObjects('ross-test', upload_path, 'success/'), '100%')
  r <- HeadObject('ross-test', 'success/will_success/1/1.txt')
  expect_equal(r$status_code, 200)

  # should fail
  removeObjects('ross-test', confirm=TRUE)
  upload_path <- 'tests/test_upload/multiplefiles/will_fail/'
  .state$upload <- list()
  expect_warning(r <- uploadMultipleObjects('ross-test', upload_path, 'fail', .progressbar = F))
  r <- HeadObject('ross-test', 'fail/1/1.txt')
  expect_equal(r$status_code, 200)
  r <- HeadObject('ross-test', 'fail/3/b-3.txt')
  expect_equal(r$status_code, 404)
  # auto resume
  expect_warning(expect_message(r <- uploadMultipleObjects('ross-test', upload_path, 'fail', .progressbar = F), '6 files to upload'))

  # pattern
  removeObjects('ross-test', confirm=TRUE)
  upload_path <- file.path(getwd(), 'tests/test_upload/multiplefiles/will_success/')
  expect_output(uploadMultipleObjects('ross-test', upload_path, 'success/', pattern='a-.*.txt$', .parallel = T), '100%')
  r <- HeadObject('ross-test', 'success/will_success/1/a-1.txt')
  expect_equal(r$status_code, 200)
  r <- HeadObject('ross-test', 'success/will_success/1/1.txt')
  expect_equal(r$status_code, 404)

  # download
  expect_output(r <- uploadMultipleObjects('ross-test', 'tests/test_upload/multiplefiles/will_success/', split=10), '100%')
  old <- getwd()
  dir.create('/Volumes/RamDisk/ross')
  setwd('/Volumes/RamDisk/ross')
  expect_output(r<-downloadMultipleObjects('ross-test', 'will_success'), '100%')
  expect_true(file.exists('will_success/1/1.txt'))

  expect_output(r<-downloadMultipleObjects('ross-test', 'will_success', 'success'), '100%')
  expect_true(file.exists('success/1/1.txt'))

  expect_output(r<-downloadMultipleObjects('ross-test', 'will_success', 'success/'), '100%')
  expect_true(file.exists('success/will_success/1/1.txt'))

  expect_output(r<-downloadMultipleObjects('ross-test', 'will_success/1/', '/Volumes/RamDisk/ross/', resume = F, .parallel = T), '100%')
  expect_true(file.exists('1/1.txt'))

  expect_message(r<-downloadMultipleObjects('ross-test', 'will_success/', '/Volumes/RamDisk/ross/pattern', pattern='a-.*.txt$', .progressbar = F), '6 files to download')
  expect_true(file.exists('pattern/1/a-1.txt'))

  expect_message(r<-downloadMultipleObjects('ross-test', 'will_success/2', '/Volumes/RamDisk/ross/', split=10, .progressbar = F))
  expect_true(file.exists('2/a-2.txt'))

  expect_output(r<-downloadMultipleObjects('ross-test', 'will_success/1', '/Volumes/RamDisk/ross/'), '100%')
  expect_true(!file.exists('11/a-1.txt'))

  expect_silent(r<-PutObject('ross-test', 'will_success/2'))
  expect_output(r<-downloadMultipleObjects('ross-test', 'will_success/2', '/Volumes/RamDisk/ross/22'), '100%')
  expect_true(!dir.exists('22') && file.exists('22'))

  expect_silent(r<-PutObject('ross-test', 'will_success/6'))
  expect_output(r<-downloadMultipleObjects('ross-test', 'will_success/6'), '100%')
  expect_true(file.exists('6'))

  expect_output(r<-downloadMultipleObjects('ross-test', 'will_success/2', '21/'), '100%')
  expect_true(file.exists('21/2'))

  removeObjects('ross-test', confirm=TRUE)
  setwd(old)
  unlink('/Volumes/RamDisk/ross/', recursive = T)
  deleteBucket('ross-test')
})

test_that("listMultipartUploads, abortMultipartUpload",{
  createBucket('ross-test')
  r <- InitiateMultipartUpload('ross-test', 'abort-test')
  r <- InitiateMultipartUpload('ross-test', 'abort-test2')
  r <- InitiateMultipartUpload('ross-test', 'abort-test')
  r <- InitiateMultipartUpload('ross-test', 'abort-test2')
  expect_equal(nrow(listMultipartUploads('ross-test')), 4)
  expect_message(abortMultipartUpload('ross-test', 'abort-test2'))
  expect_equal(nrow(listMultipartUploads('ross-test')), 2)
  expect_message(abortMultipartUpload('ross-test'))
  expect_equal(nrow(listMultipartUploads('ross-test')), 0)
  removeObjects('ross-test', confirm=TRUE)
  deleteBucket('ross-test')
})

test_that("copyObjects", {
  # Upload
  expect_output(r<-copyObjects('tests/test_upload/multiplefiles/will_success/', 'success', dest_bucket = 'ross-test'), "100%")
  r <- HeadObject('ross-test', 'success/1/1.txt')
  expect_equal(r$status_code, 200)
  # Download
  expect_output(r<-copyObjects('success', '/Volumes/RamDisk/', src_bucket = 'ross-test'), "100%")
  expect_true(file.exists('/Volumes/RamDisk/success/1/1.txt'))
  # Online Copy
  expect_slient(copyObjects('success/1/1.txt', 'cp/1/1.txt', 'ross-test', 'ross-test'))
  r <- HeadObject('ross-test', 'cp/1/1.txt')
  expect_equal(r$status_code, 200)
  # Local Copy
  expect_silent(copyObjects('/Volumes/RamDisk/success/1/1.txt', '/Volumes/RamDisk/1.txt'))
})