readNewData <- function(fileinfo) {
  if (is.null(fileinfo)) {
    fileinfo <- list(name = "Assays.txt", size = 1, type = "text/txt", datapath = "data/Assays.txt")
  }
  newdata<- read.table(file = fileinfo$datapath, sep='\t', fileEncoding = 'UTF-8', header = T, stringsAsFactors = F)
  return(newdata)
}

