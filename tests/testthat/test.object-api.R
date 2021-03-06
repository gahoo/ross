###### PUT
test_that("PutObject", {
  r <- PutBucket('ross-test')
  expect_equal(r$status_code, 200)
  r <- PutObject('ross-test', 'test.txt', 'test')
  expect_equal(r$status_code, 200)
  r <- PutObject('ross-test', 'test.txt', 'test', encryption = 'AES256')
  expect_equal(r$status_code, 200)
  r <- PutObject('ross-test', 'test.txt', 'test', acl = 'public-read')
  expect_equal(r$status_code, 200)
  r <- PutObject('ross-test', 'test.txt', 'test', .md5 = F)
  expect_equal(r$status_code, 200)
  r <- PutObject('ross-test', 'test.txt', 'test', .meta = list(location='beijing', owner='igenecode.com'))
  expect_equal(r$status_code, 200)
  expect_error(PutObject('ross-test', 'test.txt', 'test', .overwrite = F))
  r <- DeleteObject('ross-test', 'test.txt')
  expect_equal(r$status_code, 204)
})

test_that("CopyObject", {
  r <- PutBucket('ross-test')
  expect_equal(r$status_code, 200)
  r <- PutObject('ross-test', 'test.txt', 'test', .meta = list(location='beijing'))
  expect_equal(r$status_code, 200)
  r <- CopyObject('/ross-test/test.txt', 'ross-test', 'test2.txt')
  expect_equal(r$status_code, 200)
  r <- CopyObject('/ross-test/test.txt', 'ross-test', 'test2.txt', encryption = 'AES256')
  expect_equal(r$status_code, 200)
  r <- CopyObject('/ross-test/test.txt', 'ross-test', 'test2.txt', acl = 'public-read')
  expect_equal(r$status_code, 200)
  r <- CopyObject('/ross-test/test.txt', 'ross-test', 'test2.txt', meta.directive = 'REPLACE', .meta = list(owner='igenecode.com'))
  expect_equal(r$status_code, 200)
  r <- CopyObject('/ross-test/test.txt', 'ross-test', 'test2.txt', ETag = 'AAAA', ETag.match = F)
  expect_equal(r$status_code, 200)
  expect_warning(r <- CopyObject('/ross-test/test.txt', 'ross-test', 'test2.txt', ETag = 'AAAA', ETag.match = T))
  expect_equal(r$status_code, 412)
  r <- CopyObject('/ross-test/test.txt', 'ross-test', 'test2.txt', since = Sys.time(), modified.since = T)
  expect_equal(r$status_code, 304)
  r <- CopyObject('/ross-test/test.txt', 'ross-test', 'test2.txt', since = Sys.time(), modified.since = F)
  expect_equal(r$status_code, 200)
  r <- DeleteMultipleObjects('ross-test', c('test.txt', 'test2.txt'))
  expect_equal(r$status_code, 200)
})

test_that("GetObject", {
  r <- PutBucket('ross-test')
  expect_equal(r$status_code, 200)
  r <- PutObject('ross-test', 'test.txt', 'test', .meta = list(location='beijing'))
  expect_equal(r$status_code, 200)
  r <- GetObject('ross-test', 'test.txt')
  expect_equal(httr::content(r, encoding = 'UTF-8'), 'test')
  r <- GetObject('ross-test', 'test.txt', Range = '0-1')
  expect_equal(httr::content(r, encoding = 'UTF-8'), 'te')
  r <- GetObject('ross-test', 'test.txt', ETag = 'AAAA', ETag.match = F)
  expect_equal(r$status_code, 200)
  expect_warning(r <- GetObject('ross-test', 'test.txt', ETag = 'AAAA', ETag.match = T))
  expect_equal(r$status_code, 412)
  r <- GetObject('ross-test', 'test.txt', since = Sys.time(), modified.since = T)
  expect_equal(r$status_code, 304)
  r <- GetObject('ross-test', 'test.txt', since = Sys.time(), modified.since = F)
  expect_equal(r$status_code, 200)
  r <- DeleteObject('ross-test', 'test.txt')
  expect_equal(r$status_code, 204)
})

test_that("AppendObject", {
  r <- PutBucket('ross-test')
  expect_equal(r$status_code, 200)
  r <- DeleteObject('ross-test', 'test-append.txt')
  r <- AppendObject('ross-test', 'test-append.txt', body='1', position = 0)
  expect_equal(r$status_code, 200)
  r <- AppendObject('ross-test', 'test-append.txt', body='2', position = 1, acl = 'public-read', encryption = 'AES256')
  expect_equal(r$status_code, 200)
  r <- AppendObject('ross-test', 'test-append.txt', body='3', position = 2, .md5 = F)
  expect_equal(r$status_code, 200)
  r <- AppendObject('ross-test', 'test-append.txt', body='4', position = 3, .meta = list(location='beijing'))
  expect_equal(r$status_code, 200)
  r <- AppendObject('ross-test', 'test-append.txt', body='5', position = 4, "Content-Encoding"='UTF-8')
  expect_equal(r$status_code, 200)
  r <- DeleteObject('ross-test', 'test-append.txt')
  expect_equal(r$status_code, 204)
})

test_that("DeleteObject", {
  r <- PutBucket('ross-test')
  expect_equal(r$status_code, 200)
  r <- PutObject('ross-test', 'test.txt')
  expect_equal(r$status_code, 200)
  r <- DeleteObject('ross-test', 'test.txt')
  expect_equal(r$status_code, 204)
})

test_that("DeleteMultipleObjects", {
  r <- PutBucket('ross-test')
  expect_equal(r$status_code, 200)
  r <- PutObject('ross-test', 'test.txt')
  expect_equal(r$status_code, 200)
  r <- PutObject('ross-test', 'test2.txt')
  expect_equal(r$status_code, 200)
  r<-GetBucket('ross-test')
  expect_equal(r$status_code, 200)
  keys<-unlist(xpath2list(httr::content(r), '/ListBucketResult/Contents/Key'))
  r<-DeleteMultipleObjects('ross-test', keys, FALSE)
  expect_equal(r$status_code, 200)
})

test_that("HeadObject", {
  r <- PutObject('ross-test', 'test.txt')
  expect_equal(r$status_code, 200)
  etag <- gsub('"', '', r$headers$etag)
  r <- HeadObject('ross-test', 'test.txt')
  expect_equal(r$status_code, 200)
  r <- HeadObject('ross-test', 'test.txt', ETag=etag, ETag.match=T)
  expect_equal(r$status_code, 200)
  r <- HeadObject('ross-test', 'test.txt', ETag=etag, ETag.match=F)
  expect_equal(r$status_code, 304)
  r <- HeadObject('ross-test', 'test.txt', ETag="AAA", ETag.match=T)
  expect_equal(r$status_code, 412)
  r <- HeadObject('ross-test', 'test.txt', ETag="AAA", ETag.match=F)
  expect_equal(r$status_code, 200)
})

test_that("PutObjectMeta", {
  r <- PutObject('ross-test', 'test.txt')
  expect_equal(r$status_code, 200)
  r <- PutObjectMeta('ross-test', 'test.txt', .meta=list(test='a'))
  expect_equal(r$status_code, 200)
  r <- HeadObject('ross-test', 'test.txt')
  expect_equal(r$headers$`x-oss-meta-test`, 'a')
})

test_that("GetObjectMeta", {
  r <- PutObject('ross-test', 'test.txt')
  expect_equal(r$status_code, 200)
  r <- GetObjectMeta('ross-test', 'test.txt')
  expect_equal(r$status_code, 200)
})

test_that("PutObjectACL", {
  r <- PutBucket('ross-test')
  expect_equal(r$status_code, 200)
  r <- PutObject('ross-test', 'test.txt', 'test')
  expect_equal(r$status_code, 200)
  r <- PutObject('ross-test', 'test.txt', acl = 'public-read')
  expect_equal(r$status_code, 200)
})

test_that("GetObjectACL", {
  r <- PutBucket('ross-test')
  expect_equal(r$status_code, 200)
  r <- PutObject('ross-test', 'test.txt', 'test', acl = 'public-read')
  expect_equal(r$status_code, 200)
  r <- GetObjectACL('ross-test', 'test.txt')
  grant <- unlist(xpath2list(httr::content(r, encoding = 'UTF-8'), '/AccessControlPolicy/AccessControlList/Grant'))
  expect_equal(grant, 'public-read')
})

test_that("PutSymlink", {
  r <- PutBucket('ross-test')
  expect_equal(r$status_code, 200)
  r <- PutObject('ross-test', 'test.txt')
  expect_equal(r$status_code, 200)
  r <- PutSymlink('ross-test', 'test-linked.txt', 'test.txt')
  expect_equal(r$status_code, 200)
  r <- PutSymlink('ross-test', 'test-no-exist-linked.txt', 'not-exist-test.txt')
  expect_equal(r$status_code, 200)
})

test_that("GetSymlink", {
  r <- PutBucket('ross-test')
  expect_equal(r$status_code, 200)
  r <- PutObject('ross-test', 'test.txt')
  expect_equal(r$status_code, 200)
  r <- PutSymlink('ross-test', 'test-linked.txt', 'test.txt')
  expect_equal(r$status_code, 200)
  r <- GetSymlink('ross-test', 'test-linked.txt')
  expect_equal(r$headers$`x-oss-symlink-target`, 'test.txt')
  r <- PutSymlink('ross-test', 'test-no-exist-linked.txt', 'not-exist-test.txt')
  expect_equal(r$status_code, 200)
  r <- GetSymlink('ross-test', 'test-no-exist-linked.txt')
  expect_equal(r$status_code, 200)
})

test_that("RestoreObject",{
  r <- PutBucket('ross-test-archive', StorageClass = 'Archive')
  expect_equal(r$status_code, 200)
  r <- PutObject('ross-test-archive', 'test.txt', 'test')
  expect_equal(r$status_code, 200)
  r <- RestoreObject('ross-test-archive', 'test.txt')
  expect_equal(r$status_code, 202)
  r <- RestoreObject('ross-test-archive', 'test.txt')
  expect_equal(r$status_code, 409)
  removeObjects('ross-test-archive', confirm = T)
  r <- DeleteBucket('ross-test-archive')
  expect_equal(r$status_code, 204)
})
