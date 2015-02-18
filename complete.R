
complete <- function(directory, id = 1:332) {
  
  data <- data.frame()
  nobs <- data.frame(id=id,"nobs"=0)

  file_list <- list.files(directory, full.names=TRUE)

  for(fname in file_list) {
    tmp <- read.table(fname, header = TRUE, sep = ",")
    tmp <- subset(tmp, ID %in% id)
    data <- rbind(data,tmp)
  }
  
  
  for(id2 in id) {
    tmp2 <- complete.cases(data[data$ID==id2,])
    nobs[id==id2,2] <- length(tmp2[tmp2==TRUE])
  }
  return(nobs)
}