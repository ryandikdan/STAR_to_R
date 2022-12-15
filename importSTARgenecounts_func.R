importSTARgenecounts <- function(data_folder) {
  
  library(tibble)
  library(readr)
  
  # Pulls out files the Gene counts file names 
  files <- list.files(data_folder, pattern = ".*ReadsPerGene.out.tab",full.names = T, recursive = FALSE)
  
  # Makes a sample_names variable using the file names by removing the beginning and end
  sample_names<-sub(data_folder,"",sub(".ReadsPerGene.out.tab", "",files))
  
  # Name the files by their sample names as metadata
  names(files)<-sample_names
  
  # This is a bit messy but does the job
  # It loads in the first values to make a tibble of the appropriate size
  temp_STAR_table<-read_table(files[[1]],col_names = c('ensgene','total mapped','for mapped','rev mapped'))
  
  # Then it loads in all the rest
  for (name in names(files)) {
    temp_STAR_table[name] <- read_table(files[name],col_names = c('ensgene','total mapped','for mapped','rev mapped'))$`rev mapped`
  }
  
  # Then it removes the initialization stuff
  STAR_table <- subset(temp_STAR_table,select=-c(`total mapped`,`for mapped`,`rev mapped`))
  
  # Then remove the N_data
  STAR_table <- STAR_table[-1:-4, ]
  
  return(STAR_table)
}