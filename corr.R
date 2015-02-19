
corr <- function(directory, threshold = 0) {
  
  id = 1:332
  
  data <- data.frame()
  nobs <- data.frame(id=id,"nobs"=0)

  file_list <- list.files(directory, full.names=TRUE)

  for(fname in file_list) {
    tmp <- read.table(fname, header = TRUE, sep = ",")
#    tmp <- subset(tmp, ID %in% id)
    data <- rbind(data,tmp)
  }
  
  
  for(id2 in id) {
    tmp2 <- complete.cases(data[data$ID==id2,])
    nobs[id==id2,2] <- length(tmp2[tmp2==TRUE])
  }

  ids_for_corr_calc <- nobs[nobs$nobs > threshold,"id"]
  
  data_for_corr <- data[which(data$ID %in% ids_for_corr_calc), ]
  data_for_corr_complete <- data_for_corr[complete.cases(data_for_corr), ]
  
  cor_result <- 0

  for(i in 1:length(ids_for_corr_calc)) { 
    cor_ids <- which(data_for_corr_complete$ID==ids_for_corr_calc[i])
    cor_result[i] <- cor(data_for_corr_complete[cor_ids,"sulfate"],
                         data_for_corr_complete[cor_ids,"nitrate"]) 
  }
  
cr <- cor_result[!is.na(cor_result)]
#options(digits=4)

  return(cr)
}
