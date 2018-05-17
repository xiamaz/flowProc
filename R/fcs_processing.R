#' Load and filter single file info entry
#'
#' @param file.entry File info row from a file matrix.
#' @param selected.channels Marker names selected from flowframe.
#' @param trans String name for transformation function.
#' @param remove_margins Boolean for removal of events with 0 or maximum values.
#' @return File row or NULL if marker name not in flowframe.
#' @export
#' @examples
#' t = ProcessSingle(files[[1]],selection)
ProcessSingle <- function(file.entry, selected.channels, trans = "logicle", remove_margins = TRUE, ...) {
  file.entry <- ReadFile(file.entry, ...)

  if (!isS4(file.entry)) {
    return(NA)
  }
  if (!missing(selected.channels)) {
    file.entry <- FcsSelectMarkers(file.entry, selected.markers = selected.channels)
    if (!isS4(file.entry)) {
      return(NA)
    }
  }
  if (remove_margins) {
    file.entry <- RemoveMarginal(file.entry, ...)
    if (!isS4(file.entry)) {
      return(NA)
    }
  }
  selection.function <- function(x) { !grepl("LIN", x) }
  if (trans == "logicle") {
    transform.function <- flowCore::logicleTransform()
  } else {
    transform.function <- flowCore::logTransform(transformationId = "log10-transformation", logbase = 10, r = 1, d = 1)
  }
  file.entry@fcs <- TransformChannels(file.entry@fcs, selection.function, transform.function)
  return(file.entry)
}

#' Load single fcs file into a file matrix
#'
#' @export
ReadFile <- function(flow.entry, simple.marker.names = F, dataset = 1, ...) {
  flow.entry@fcs <- flowCore::read.FCS(GetFilepath(flow.entry), dataset = dataset)
  # use simplified markernames, this might be an inappropriate simplification
  m <- flowCore::markernames(flow.entry@fcs)
  curnames <- flowCore::colnames(flow.entry@fcs)
  if (length(m) > length(curnames)) {
    stop(sprintf("Length of %d markernames does not match length %d of current names", curnames, m))
  }
  for (s in 1:length(m)) {
    curnames[s] <- m[[s]]
  }

  if (simple.marker.names) {
    m <- strsplit(m, "-")
    m <- sapply(m, function(x) { x[[1]] } )
  }
  flowCore::colnames(flow.entry@fcs) <- curnames

  return (flow.entry)
}


#' Reduce flowframe to specified set of marker channels.
#'
#' @param fcs_entry File matrix containing loaded flowframes.
#' @param selection Vector of marker names, which will be used for selection.
#' @return Flowframe with filtered flowframes. Flowframes not having all specified markers will be replaced with NA.
FcsSelectMarkers <- function(fcs_entry, selected.markers, ...) {
  ffn <- flowCore::colnames(fcs_entry@fcs)
  if (!any(is.na(match(selected.markers, ffn)))) {
    fcs_entry@fcs <- fcs_entry@fcs[,selected.markers]
    return(fcs_entry)
  } else {
    return(NA)
  }
}


#' Exclude flowframes without common channels.
#'
#' @param fcs_info File matrix with loaded flowframes.
#' @param threshold Minimum ratio for marker to be common.
#' @return File matrix with removed channels.
#' @examples
#' t = FilterCommonChannels(files[1:10], 0.8)
#' t = FilterCommonChannels(files, 0.8, cluster)
#' @export
FilterCommonChannels <- function(flow_entries, ...) {
  lfunc <- CreateLapply(...)
  loaded.entries <- lfunc(flow_entries, function(x) { read_file(x) })

  return(FilterChannelMajority(loaded.entries, ...))
}


#' Return flowframes with common channels and selected channels.
#'
#' @param flow.entries File matrix with loaded flowframes.
#' @param threshold Minimum ratio for marker to be common.
#' @return List with filtered list and the names of the selected channels.
#' @export
FilterChannelMajority <- function(flow.entries, threshold = 0.9, ...) {
  marker.matrix <- MarkerOccurences(flow.entries)
  marker.sums <- colSums(marker.matrix)
  # get marker names over threshold
  selected.markers <- names(marker.sums)[marker.sums / nrow(marker.matrix) >= threshold]
  # select entries with all markers present
  selected.matrix <- marker.matrix[, selected.markers]
  selected.files <- flow.entries[rowSums(selected.matrix) == length(selected.markers)]
  return(list(entries = selected.files, markers = selected.markers))
}


#' Remove marginal cells
#'
#' Removes all events with at least one border value.
#' The border is defined via the parameter range or as the min/max of the dataset.
#'
#' @param flow_entry Flow data with loaded fcs slot.
#' @param lower Boolean to enable lower boundary filtering.
#' @param upper Boolean to enable upper boundary filtering.
#' @return Flow entry with marginal events removed.
#' @examples
#' RemoveMarginal(testflow)
#' @export
RemoveMarginal <- function(flow_entry, lower = TRUE, upper = TRUE, ...) {
  cols <- range(flow_entry@fcs)
  ex <- flowCore::exprs(flow_entry@fcs)
  sel <- sapply(1:ncol(cols), function(i) {
    if(lower & upper) {
      ex[, i] > max(cols[1, i], min(ex[, i])) &
      ex[, i] < min(cols[2, i], max(ex[, i]))
    } else if (lower) {
      ex[, i] > max(cols[1, i], min(ex[, i]))
    } else if (upper) {
      ex[, i] < min(cols[2, i], max(ex[, i]))
    } else {
      ex[, i] & TRUE
    }
  })
  if (nrow(ex) <= 1) {
    print(ex)
    flow_entry <- NA
  } else {
    ex <- ex[rowSums(sel) == ncol(sel),]
    if (!is.matrix(ex))  {
      return(NA)
    }
    flowCore::exprs(flow_entry@fcs) <- ex
  }
  return(flow_entry)
}
#' Get distribution of marker channels across whole dataset.
#'
#' @param flow_entries Flow entry list with loaded fcs files.
#' @return Table with occurrences of marker channels as 1/0 matrix.
#' @examples
#' t = marker_occurrences(files, cluster)
#' t = marker_occurrences(files[1:10])
#' @export
MarkerOccurences <- function(flow.entries) {
  # save colnames as vector of ones
  colmatrix <- lapply(flow.entries, function(x) {
               fnames <- flowCore::read.FCSheader(GetFilepath(x))[[1]]
               par.names <- names(fnames)
               fnames <- fnames[par.names[grepl("\\$P\\d+S", par.names, perl = T)]]
               fnamevec <- rep(1, length(fnames))
               names(fnamevec) <- fnames
               return(t(fnamevec))
  })
  colmatrix <- plyr::rbind.fill.matrix(colmatrix)
  colmatrix[is.na(colmatrix)] <- 0
  return(colmatrix)
}


#' Transform flowcytometric marker channels.
#'
#' Apply a transformation function to the channels specified by the selection function.
#'
#' @param flowframe FCS Flowframe.
#' @param selection.function Selection function to determine transform application, which returns for an input name either TRUE or FALSE
#' @param transform.function Transformation function, which will be applied.
#' @return File Matrix with transformed flow frames.
TransformChannels <- function(flowframe, selection.function, transform.function, ...) {
  markernames <- flowCore::colnames(flowframe)
  transform.markers <- markernames[selection.function(markernames)]
  transform.list <- flowCore::transformList(transform.markers, transform.function)
  flowframe <- flowCore::transform(flowframe, transform.list)
  return(flowframe)
}
