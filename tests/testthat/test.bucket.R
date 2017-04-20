test_that("Bucket", {
  expect_message(b <- Bucket$new('ross-test', autoCreate=T))
  expect_silent(b$rm())
  # autoCreate=F
  expect_error(b <- Bucket$new('ross-test', autoCreate=F))
  expect_warning(expect_error(b$rm()))
  # create
  expect_message(b$create(acl='public-read', StorageClass = 'IA'))
  expect_silent(b$refresh())
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

test_that("Bucket$website", {
  expect_message(b <- Bucket$new('ross-test', autoCreate=T))
  expect_silent(b$website <- list(Suffix='index.html', Key='404.html'))
  expect_equal(b$website, list(Suffix='index.html', Key='404.html'))
  expect_message(b$website <- list(Suffix='Index.html'))
  expect_equal(b$website, list(Suffix='Index.html', Key='404.html'))
  expect_silent(b$website <- list(Suffix="", Key='error.html'))
  expect_equal(b$website, list(Key='error.html'))
  expect_silent(b$website <- list())
  expect_silent(b$website <- NULL)
  expect_equal(length(b$website), 0)
  expect_silent(b$rm())
})

test_that("Bucket$referer", {
  expect_message(b <- Bucket$new('ross-test', autoCreate=T))
  expect_silent(b$referer <- list(AllowEmptyReferer=T, RefererList=c('*.igenecode.com', 'aliyun.com')))
  b$referer <- list(AllowEmptyReferer=F)
  expect_equal(b$referer$AllowEmptyReferer, 'false')
  b$referer <- list(RefererList=c('*.igenecode.com', 'aliyun.com'))
  expect_equal(b$referer$AllowEmptyReferer, 'true')
  expect_equal(b$referer$RefererList, c(Referer='*.igenecode.com', Referer='aliyun.com'))
  b$referer <- NULL
  b$referer <- list()
  expect_silent(b$rm())
})

test_that("BucketLifecycle", {
  r <- PutBucket('ross-test')
  expect_silent(life<-BucketLifecycle$new('ross-test', autoSave=T))
  expect_equal(life$length, 0)
  # add
  expect_silent(life$add('upload_', Object.CreatedBeforeDate = "2017-04-01"))
  expect_silent(life$add('upload_', Object.Days=5))
  expect_silent(life$add('upload2_', Multpart.Days = 5))
  expect_silent(life$add('Backup_', ID='backup-1', Object.Days=90, Multpart.Days = 5))
  expect_error(life$add('upload2_'))
  expect_equal(life$length, 3)
  # remove
  expect_silent(life$remove('upload_'))
  expect_silent(life$remove(ID='backup-1'))
  expect_equal(life$length, 1)
  # clear
  expect_silent(life$clear())
  expect_equal(life$length, 0)
  # autoSave=F
  expect_silent(life<-BucketLifecycle$new('ross-test', F))
  expect_silent(life$add('backup1_', ID='backup-1', Object.Days=90))
  expect_silent(life$add('backup2_', ID='backup-2', Object.Days=90))
  expect_silent(life$add('backup3_', ID='backup-3', Object.Days=90))
  expect_silent(life$save())
  expect_equal(life$length, 3)
  r <- DeleteBucket('ross-test')
})

test_that("BucketCORS", {
  r <- PutBucket('ross-test')
  r <- DeleteBucketcors('ross-test')
  expect_silent(cors <- BucketCORS$new('ross-test'))
  expect_silent(cors$add('*', 'GET'))
  expect_equal(cors$length, 1)
  expect_silent(cors$add('*', 'GET'))
  expect_equal(cors$length, 1)
  expect_silent(cors$add('igenecode.com', c('GET', 'PUT')))
  expect_equal(cors$length, 2)
  expect_silent(cors$add('www.igenecode.com', c('GET', 'PUT'),
           AllowedHeader='Authorization',
           ExposeHeader=c('x-oss-meta1', 'x-oss-meta2'),
           MaxAgeSeconds=100))
  expect_equal(cors$length, 3)
  expect_silent(cors$remove('*', 'GET'))
  expect_equal(cors$length, 2)
  expect_silent(cors$remove('igenecode.com', c('GET', 'PUT')))
  expect_equal(cors$length, 1)
  expect_silent(cors$clear())
  expect_equal(cors$length, 0)
  # autoSave=F
  expect_silent(cors <- BucketCORS$new('ross-test', F))
  expect_silent(cors$add('*', 'GET'))
  expect_equal(cors$length, 1)
  expect_silent(cors$add('*', 'GET'))
  expect_equal(cors$length, 1)
  expect_silent(cors$add('igenecode.com', c('GET', 'PUT')))
  expect_equal(cors$length, 2)
  expect_silent(cors$save)
  expect_equal(cors$length, 2)
  expect_silent(cors$clear())
  expect_equal(cors$length, 0)
})

test_that("Bucket$list", {
  expect_silent(b$list())
})
