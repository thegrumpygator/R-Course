pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  library(stringr)
  id2 <- str_pad(id,3,pad="0")
  id_str <- paste(directory, "/", id2, ".csv", sep = "")
  
  data <- sapply(id_str, read.csv, simplify=FALSE)
  data <- do.call(rbind, data)
  round(mean(data[,pollutant], na.rm=TRUE), digits=3)
}