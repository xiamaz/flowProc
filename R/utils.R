#' Parallelize functions with lapplies
#'
#' Create a new cluster context for function to be parallelized. This creates
#' considerable overhead, but is easier to debug and maintain correct object
#' dependencies in complex lapply functions. All functions needing this should
#' be condidered for more elaborate preformance optimization.
#'
#' @param thread.num Number of threads specified for operation. A number of 1
#' will use normal lapply.
#' @return Correctly parallelized lapply function.
CreateLapply <- function(thread.num = 1, ...) {
  if (thread.num > 1) {
    lfunc <- function(x, y) {
      cluster <- parallel::makeCluster(thread.num, type = "FORK")
      result <- parallel::parLapply(cluster, x, y)
      parallel::stopCluster(cluster)
      return(result)
    }
  } else {
    lfunc <- lapply
  }
  return(lfunc)
}


#' Convert a single case row to a list of S4 objects for this case. Each
#' Filepath will be converted to a separate object.
RowToS4 <- function(case.row, dir.path, temp.path) {
  infiltration <- as.numeric(case.row[["infiltration"]])
  if (is.na(infiltration)) {
    message("Parsing failure ", infiltration, " ", case.row[["infiltration"]])
  }
  dest.s4.fun <- function(dest.row) {
    s4.obj <- FlowEntry(temppath = temp.path,
                        dirpath = dir.path,
                        filepath = dest.row[["path"]],
                        material = dest.row[["material"]],
                        tube_set = as.numeric(dest.row[["tube"]]),
                        group = case.row[["cohort"]],
                        label = case.row[["id"]],
                        infiltration = infiltration,
                        diagnosis = case.row[["diagnosis"]])
    return(s4.obj)
  }
  case.list <- lapply(case.row[["destpaths"]], dest.s4.fun)
  return(case.list)
}


#' Convert list of data from json to S4 objects
#' Keep grouping of cohort - case-id - s4-objs
JsonToS4 <- function(json.list, dir.path = "", temp.path = "") {
  group.list <- lapply(json.list, function(cases) {
    case.list <- lapply(cases, function(case) {RowToS4(case, dir.path, temp.path)})
    return(case.list)
  })
  return(group.list)
}


#' Get Filepath from FlowEntry
GetFilepath <- function(entry) {
  return(GetFile(entry@filepath, entry@dirpath, entry@temppath))
}


#' S4 class representing a flowframe with additional metadata
#'
#' @slot filepath Path to fcs file.
#' @slot group Group of single case. Most often corresponding to the disease label.
#' @slot label Unique identifier matching a single case.
#' @slot material Sample material used in analysis.
#' @slot tube_set Multiple tubes can be used to capture a larger number of channels per sample.
#' @slot fcs Slot to load fcs file.
#' @slot dataset Original dataset of the file.
#' @importClassesFrom flowCore flowFrame
FlowEntry <- setClass("FlowEntry",
     representation(
      filepath = "character",
      dirpath = "character",
      temppath = "character",
      group = "character",
      label = "character",
      material = "character",
      tube_set = "numeric",
      fcs = "flowFrame",
      infiltration = "numeric",
      diagnosis = "character",
      dataset = "character"
      )
     )
