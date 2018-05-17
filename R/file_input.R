#' Read jSON info file with additional case metainformation
#'
#' Create list:groups->list:case->S4:flowData
#'
#' @export
ReadDatasetJson <- function(json.path, dir.path = "", temp.path = "") {
  json.data <- jsonlite::fromJSON(json.path, simplifyVector = F, simplifyMatrix = F, simplifyDataFrame = F)
  flow.list <- JsonToS4(json.data, dir.path, temp.path)
  return(flow.list)
}


#' Read all datasets
#'
#' Data used by flowProc has to be organized into a specific hierarchy.
#'  BASE_DIR - Root directory of the data
#'  DATASET - Set of data containing different cohorts
#'  COHORTS - folder name is used to denote cohort affiliation
#'  FILES - filename used to infer label and tube_set
#'
#' @param path Dataset root directory.
#' @export
ReadDatasets <- function(path, ...) {
  datasets <- list.dirs(path, recursive = F, full.names = F)
  datasets <- lapply(datasets, function(dset) {
                       message("Reading data from dataset ", dset)
                       dpath <- file.path(path, dset)
                       dset <- ReadDataset(dpath, dataset = dset, ...)
  })
  # flatten list, dataset can be retrieved from slot
  return(do.call(c, datasets))
}


#' Read single dataset
#'
#' Convenient wapper to extract a single set without duplicates from directory file structure.
#'
#' @inheritParams GetDir
#' @param remove.duplicates Remove duplicate file entries.
#' @param filters Optional filters used on slot of files.
#' @return File matrix with specified set and no duplicates.
#' @useDynLib flowProc, .registration = TRUE
#' @export
ReadDataset <- function(path, remove.duplicates = T, filters = list(), dataset = "", ...) {
  # entry.list <- cGetDir(path, dataset)
  # entry.list <- GetDir(path, ...)
  entry.list <- .Call("c_get_dir", path, dataset)

  if (is.null(entry.list) | length(entry.list) == 0) {
    print(sprintf("No files found in given directory %s", path))
    return(entry.list)
  }
  if (remove.duplicates) {
    entry.list <- RemoveDuplicates(entry.list)
  }

  if (length(filters) > 0) {
    entry.list <- entry.list[FilterEntries(entry.list, filters)]
  }
  return(entry.list)
}


#' Get file structure information from a given directory, using the lower directories as groupings.
#'
#' @param path Top directory which contains the lower directories serving as different groups.
#' @param ext File extension of files to be included. Generally this will be either lmd or fcs (case sensitive)
#' @param dataset Dataset name.
#' @return Matrix containing filepath, group, label (regex currently hardcoded for specific naming schemata)
#' @examples
#' GetDir('/data/flowData', 'FCS')
#' GetDir('../data/fcs', 'lmd')
#' files = GetDir(testdir, 'LMD', cluster)
#' files = GetDir(testdir, 'LMD')
#' @export
GetDir <- function(path, ext = "LMD", dataset = "", ...) {
  lfunc <- CreateLapply(...)
  # message("Listing files")
  filelist <- list.files(path, pattern = ext, full.names = TRUE, recursive = TRUE)
  # message("Regexing files")
  f <- lfunc(filelist, function(x) {
         r <- regexec("^.*/(\\w+)/(\\d+-\\d+)-(\\w+) CLL 9F (\\d+).*.LMD$", x, perl = TRUE)
         if ("-1" %in% r) {
             return(NA)
         }
         m <- regmatches(x, r)
         filepath <- x
         group <- m[[1]][[2]]
         label <- m[[1]][[3]]
         material <- m[[1]][[4]]
         tube_set <- as.numeric(m[[1]][[5]])
         fe <- methods::new("FlowEntry",
              filepath = filepath, group = group, label = label, material = material, tube_set = tube_set,
              dataset = dataset)
         return(fe)
  })
  # message("Choosing correct files")
  f <- f[!is.na(f)]
  return(f)
}


#' Group list by specific field
#'
#' @param upsampled.list List of entries.
#' @param group.on Slot to group entries on if using the default indexer and
#' comparator.
#' @param acc Alternative function to access the information for grouping.
#' @examples
#' GroupBy(all.files, 'label')
#' GroupBy(all.files, 'label', thread.num=12)
#' @export
GroupBy <- function(upsampled.list, group.on, acc = slot, ...) {
  group.names <- unique(sapply(upsampled.list, function(x) {
                                 acc(x, group.on)
  }))
  # fork into multiple threads if desired
  lapply.func <- CreateLapply(...)

  # group all files into list of lists
  groups <- lapply.func(group.names,
                   function(x) {
                     f <- list(x)
                     names(f) <- group.on
                     upsampled.list[FilterEntries(upsampled.list, f, acc = acc, ...)]
                   })
  names(groups) <- group.names
  return(groups)
}

#' Return boolean vector for a list based on filter
#'
#' @param file.list List of S4 objects implementing the names in the filter.
#' @param filters List of named vectors.
#' @param acc Optional parameter to override the function used to access the
#' compared value.
#' @return boolean vector used for subsetting the list.
#' @examples
#' all.files[FilterOnAttr(all.files, list(group='CLL'))]
#' @export
FilterEntries <- function(entry.list, ...) {
  sapply(entry.list, function(x) {
           FilterEntry(x, ...)
                             })
}

#' Return bool for entry depending on whether it matches filter
#'
#' @param entry Entry type S4 class.
#' @param filters List of vectors containing acceptable values.
#' @param acc Function to access the value being compared.
#' @return Boolean whether filter is fulfilled.
FilterEntry <- function(entry, filters, acc, ...) {
  if (missing(acc)) {
    acc <- methods::slot
  }
  # check if all criteria in filters, a named list of values is fulfilled
  for (slot.name in names(filters)) {
    if (!acc(entry, slot.name) %in% filters[[slot.name]]) {
      return(F)
    }
  }
  return(T)
}

#' Remove duplicates with same filename from file matrix.
#'
#' @param File matrix as output by GetDir()
#' @return File matrix with duplicates removed. All occurrences are removed.
#' @examples
#' t = RemoveDuplicates(files)
#' @export
RemoveDuplicates <- function(fcs_entries) {
  # remove duplicates until we have a better idea
  filenames <- sapply(fcs_entries, function(entry) {
              basename(entry@filepath)
  })
  file_freq <- table(filenames)
  file_freq <- file_freq[file_freq > 1]
  message("Removed ", length(file_freq), " duplicates.")
  # print(paste("Removed duplicates", names(file_freq)))
  duplicate <- filenames %in% names(file_freq)
  return(fcs_entries[!duplicate])
}
