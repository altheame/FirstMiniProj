##1 Pollutant Mean

pollutantmean <- function(directory, pollutant, id = 1:332) {
  ##getting the files from specdata directory
  files <- list.files(directory, full.names = TRUE) 
  
  ##creating an empty data frame
  dat <- data.frame()
  
  
  for (i in id) {
    ##row binding .csv files from directory
    dat <- rbind(dat, read.csv(files[i]))
  }
  ##computing the mean value, ignoring empty rows
  mean(dat[, pollutant], na.rm = TRUE)
}

##TEST SAMPLE
##pollutantmean("C:/Users/acer/Desktop/rprog_data_specdata/specdata, "sulfate", 1:10)

##2 

complete <- function(directory, id = 1:332) {
  ##getting the files from specdata directory
  my_files <- list.files(directory, full.names = TRUE)
  ##creating an empty data frame
  my_data <- data.frame()
  
  for (i in id) {
    ##getting .csv files from files_full
    mon <- read.csv(my_files[i])
    ##computing the number of complete cases
    nobs <- sum(complete.cases(mon))
    ##assigning data frame into var temp
    temp <- data.frame(i, nobs)
    ##row binding my_data and temp
    my_data <- rbind(my_data, temp)
  }
  
  ##assigning column names
  colnames(my_data) <- c("id", "nobs")
  my_data
}
##TEST SAMPLE
##complete("C:/Users/acer/Desktop/rprog_data_specdata/specdata", 1)

##3 

corr <- function(directory, threshold = 0) {
 
   ##getting all the files from directory
  files <- list.files(directory, full.names = TRUE)
  ##creating a numeric vector with length 0
  my_data <- vector(mode = "numeric", length = 0)
  
  for (i in 1:length(files)) {
    ##getting all .csv files from the directory
    mon <- read.csv(files[i])
    ##getting the sum of available sulfate AND nitrate values from var mon
    data_sum <- sum((!is.na(mon$sulfate)) & (!is.na(mon$nitrate)))
    
    if (data_sum > threshold) {
      ##storing available values for pollutant-sulfate from vector mon
      temp <- mon[which(!is.na(mon$sulfate)), ]
      
      ##storing available values for pollutant-sulfate from vector temp
      submon <- temp[which(!is.na(temp$nitrate)), ]
     
       ##getting the correlation between the pollutants
      my_data <- c(my_data, cor(submon$sulfate, submon$nitrate))
    }
  }
  
  my_data
}

##cr <- corr("C:/Users/acer/Desktop/rprog_data_specdata/", 150)
##head(cr)
