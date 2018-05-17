#' Automatically check if file needs to be downloaded prior to usage
#'
#' Build a filename and automatically try to resolve remote filepaths
#'
#' @export
GetFile <- function(filename, dirpath, ...) {
  if (CheckS3(dirpath)) {
    return(DownloadS3(filename, dirpath, ...))
  } else {
    return(file.name(filename, dirpath))
  }
}

#' Automatically check if outputpath needs uploading to S3
#'
#' Use write function to put a file object to a destination. Check automatically
#' if the destination is a remote url with the need to execute specific
#' functions.
#'
#' @export
PutFile <- function(fileobj, filename, outpath, write.fun, ...) {
  if (CheckS3(dirpath)) {
    UploadS3(fileobj, filename, outpath, write.fun, ...)
  } else {
    dir.create(outpath, recursive = T, showWarnings = F)
    filepath <- file.path(outpath, filename)
    write.fun(fileobj, filepath)
  }
}

#' Check if path exists
#'
#' Supports remote addresses. Executes given functions and return boolean truth
#' values.
#'
#' @export
PathExists <- function(path, true.fun = NULL, false.fun = NULL, ...) {
  if (CheckS3(path)) {
    valid <- S3PathExists(path)
  } else {
    valid <- file.exists(path)
  }
  # return function result or validity of path
  if (!is.null(true.fun) && valid) {
    return(true.fun(...))
  } else if (!is.null(false.fun) && (!valid)) {
    return(false.fun(...))
  } else {
    return(valid)
  }
}

#' Get filenames in path
#'
#' Get all filenames in path as list. Accepts remote urls too.
#'
#' @export
GetFiles <- function(path) {
  if (CheckS3(path)) {
    return(S3GetFiles(path))
  } else {
    list.files(path)
  }
}
