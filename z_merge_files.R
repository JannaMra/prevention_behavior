###### TAKEN FROM https://github.com/mc-schaaf/rainR/blob/master/R/merge_files.R
#' @title Merges Files of the Same Format
#'
#' @description reads in the files supplied and merges the data into one data
#' frame via appending. Hence, files must have the same data format
#' (number and identity of columns).
#'
#' @param files iterable of paths with names of the to-be-merged files
#' @param ... additional arguments that will be supplied to the underlying
#' \code{\link[data.table]{fread}} function
#'
#' @return merged dataset, with the additional information of the files'
#' names and -paths
#'
#' @export
#' @importFrom data.table "fread"
#'

merge_files <- function(files, ...) {
  for (file in files) {
    if (exists("dataset")) {
      temp_dataset <-  data.table::fread(file, ...)
      temp_dataset$fileName <- paste(file, sep = "")
      temp_dataset$path <- paste(getwd(), sep = "")
      #dataset <- rbind(dataset, temp_dataset, use.names = FALSE)
      rm(temp_dataset)
    }
    if (!exists("dataset")) {
      dataset <- data.table::fread(file, ...)
      dataset$fileName <- paste(file, sep = "")
      #dataset$path <- paste(getwd(), sep = "")
    }
  }
  return(dataset)
}