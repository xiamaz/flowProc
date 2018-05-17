DownloadS3 <- function(filename, inputpath, temppath) {
  s3.path <- file.path(inputpath, filename)
  target.path <- file.path(temppath, filename)
  # create possible intermediary directories
  intermed.dirs <- dirname(target.path)
  dir.create(intermed.dirs, recursive = T, showWarnings = F)
  aws.s3::save_object(s3.path, file = target.path)
  return(target.path)
}

UploadS3 <- function(fileobj, filename, outpath, write.fun, temppath) {
  source.path <- file.path(temppath, filename)
  target.path <- file.path(s3path, filename)

  # create temporary output in temp directory
  write.fun(fileobj, source.path)
  aws.s3::put_object(source.path, target.path)
}

CheckS3 <- function(path) {
  return(grepl("s3://", path, ignore.case = T))
}
