ross封装了大部分的阿里云OSS API，实现了在R中直接调用OSS的大部分功能。

# Overview
ross对阿里云OSS API进行了三层封装。
- 原始的OSS API，与阿里官方保持一致。
- wrapper函数，更符合R用户的使用习惯。
- ossutil函数与R6对象，前者提供了类似[ossutil](https://github.com/aliyun/ossutil)的使用体验，后者提供了面向对象的操作方式。

> 建议开发者使用wrapper函数或R6对象，普通用户使用ossutil函数或R6对象。ossutil函数更适合交互式操作。

# Installation
目前ross没有收录在CRAN中，需要用devtools直接从GitHub安装。
```r
devtools::install_github('gahoo/ross', build_vignettes = TRUE)
library(ross)
```
查看[完整文档](Guide)。
```r
browseVignettes('ross')
```

# Settings
操作OSS需要`AccessKeyId`和`AccessKeySecret`进行签名，在使用ross包之前需要先设置这两个环境变量。
```r
Sys.setenv(
  AccessKeyId="xxxxxxxxxxxxxxxx",
  AccessKeySecret="xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
)
```
也可以选择将环境变量写入`~/.Renviron`文件。
```
AccessKeyId=xxxxxxxxxxxxxxxx
AccessKeySecret=xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
```

# Quick Start
```r
# 创建Bucket
oss.mb('oss://ross-test')
# 写入文件
oss.write('oss://ross-test/test.txt', 'test')
# 读取文件
oss.read('oss://ross-test/test.txt')
# 保存对象
oss.saveRDS('oss://ross-test/test.rds', 1:5)
# 读取对象
oss.readRDS('oss://ross-test/test.rds')
# 下载
oss.cp('oss://ross-test/test.txt', '/tmp/')
# 上传
oss.cp('/tmp/test.txt', 'oss://ross-test/test.txt')
```

# wrapper & ossutil & R6
同样的功能可以用三类不同的函数实现，可以挑选自己喜欢的方法。

wrapper函数将常用的OSS功能进行了封装，方便调用。ossutil和R6均基于wrapper函数构建。ossutil函数与官方的ossutil命令行工具使用方法类似，适合交互式的使用模式。此外，ross以Bucket，Obejct等为对象封装了与之相关的大部分功能，诸如`lifecycle`，`logging`等关于Bucket的设置更适合用这些R6对象。

```r
# wrapper
createBucket('ross-test')
# ossutil
oss.mb('oss://ross-test')
# R6
b <- Bucket$new('ross-test')
b$create()
```
