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
      group = "character",
      label = "character",
      material = "character",
      tube_set = "numeric",
      fcs = "flowFrame",
      dataset = "character"
      )
     )
