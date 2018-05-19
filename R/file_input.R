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
