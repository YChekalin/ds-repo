
pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  data <- data.frame()

  file_list <- list.files(directory, full.names=TRUE)

  for(fname in file_list) {
    tmp <- read.table(fname, header = TRUE, sep = ",")
    tmp <- subset(tmp, ID %in% id, select = pollutant)
    data <- rbind(data,tmp)
  }
  
  return(mean(data[!is.na(data),]))
}