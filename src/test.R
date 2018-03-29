library(flowCore)
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

dyn.load("get_dir.so")
system.time(r <- .Call("c_get_dir", "../../mll_data/2018_01_normal", ""))
