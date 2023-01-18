##############################################################
# by Aubrey Jensen                                           #
# functions to help with phenotyping, analysis, and plotting #
# last updated: March 30 2021                                #
##############################################################


#read_plus
  #Accepts a character filename, reads in the data in the file, and adds a column to the data with the name of the file.
  #Useful when using e.g. map_df to read multiple data files in to a single data frame/table, and tracking where each observation came from.
  #The code to do this is like follows:
  #mult_data <-list.files(path = "/path/", pattern = "*pattern.txt", full.names = F) %>% map_df(~read_plus(.)) 
  read_plus <- function(flnm) {
    fread(flnm, header=T) %>% #, nrows=1000 for testing
      mutate(filename = flnm)
  }

  
#read_plus_test
#Same as read_plus, except only the first 500 rows are read from each file. 
#Useful for testing code without long wait times for reading in data.
  read_plus_test <- function(flnm) {
    fread(flnm, header=T, nrows=500) %>% 
      mutate(filename = flnm)
  }
  
  
  #read_plus_plink
  #Force correct interpretation of data types of plink2 output.
  read_plus_plink <- function(flnm) {
    fread(flnm, header=T) %>% 
      mutate(filename = flnm) %>%
      mutate_at(vars(contains(c("OR", "BETA"))), as.numeric) %>%
      mutate(P = as.numeric(P))
  }
  
# Underscore if exists
  us <- function(x) { #x is name of variable in quotes
    if (get0(x, ifnotfound="") != "") {
      return(paste0("_", get0(x))) 
    } else return("")
  }
  
# Space if exists
  space <- function(x) { #x is name of variable in quotes
    if (get0(x, ifnotfound="") != "") {
      return(paste0(" ", get0(x))) 
    } else return("")
  }
# cleandates
  # Accepts a Vector of dates and a vector of Dates of birth (DOB).
  # Based on UKB special date values, replaces special dates with appropriate meanings.
  # 1900-01-01 means the date is unknown.  1901-01-01 = date given is before patients DOB. 2037-07-07 = event given is in the future (probable placeholder).
  # The above dates are replaced with missing.
  # 1902-02-02 means the date matches the patients DOB.  1903-03-03 means it was in the same year as their DOB. These were masked due to potentially sensitive info.
  # The above dates are replaced with the DOB.
  cleandates <- function(dates, DOBs) {
    dates[dates %in% as.Date(c("1900-01-01", "1901-01-01", "2037-07-07"))] <- NA 
    dates[dates %in% as.Date(c("1902-02-02", "1903-03-03"))] <- DOBs[dates %in% as.Date(c("1902-02-02", "1903-03-03"))] 
    return(dates)
  }
  

# get.code.index
  # Accepts a regular expression for use in grep, and a vector to search for the expression.
  # Returns the index(es) of the vector where the expression is true, or NA if the expression is not true for any element of the vector.
  get.code.index <- function(expression, vec){
    index <- grep(expression, vec)
    if (length(index)==0) {index <- NA}
    return(index)}

  get.code.date <- function(expression, codes_df, dates_df, date="first"){
    indexes <- apply(codes_df, 1, get.code.index, expression=expression)
      #tic()
     codedates <- c()
    for (i in 1:nrow(codes_df)) {
    #for (i in c(5:15,40,496027)) {
      # if (i %in% c(15, 40, 496027)) {
      #   print(i)
      #   print(indexes[[i]])
      #   print(is.na(indexes[i]))}
      if (is.na(indexes[i])) { #no matches
        #codedates[i] <- as.Date(NA, origin=origin)
        codedates[i] <- NA
      } else if (date=="first") {
          #codedates[i] <-  min(as.Date(unlist(dates_df[i, indexes[[i]]]), origin=origin), na.rm=T)
          codedates[i] <-  min(unlist(dates_df[i, indexes[[i]]]), na.rm=T)
          #print(codedates[i])
      } else if (date=="last")  {
          #codedates[i] <-  max(as.Date(unlist(dates_df[i, indexes[[i]]]), origin=origin), na.rm=T)
          codedates[i] <-  max(unlist(dates_df[i, indexes[[i]]]), na.rm=T)
      } 
    }
    codedates <- as.Date(codedates, origin=origin)
    return(codedates)
    #toc()
  }
  
# check4code
  #Accepts a regular expression and data.frame.  
  #Returns a vector with length equal to nrow(data), with value of 1 if that row contains the expression, 0 otherwise.
  check4code <- function(expression, data){ 
    has_code <- as.numeric(!!rowSums(sapply(data[1:ncol(data)], grepl, pattern = expression)))
  }

  
# yearsto
  #Accepts 3 vectors of binary phenotypes (0,1,NA), event dates corresponding to the phenotype/event, and index dates.
  #Returns the time in years from the index date to the date of the event, if event==1.
  #Returns the time in years from the index date to the date of censoring/death, if event==0.
  #Returns NA if event== NA.
    yearsto <- function(phenotype, event_date, start_date, right_censor_date){ 
      ec <- numeric(length(phenotype))
      ec[phenotype == 1 & !is.na(phenotype)] <- event_date[phenotype == 1 & !is.na(phenotype)]
      ec[phenotype == 0 & !is.na(phenotype)] <- right_censor_date[phenotype == 0 & !is.na(phenotype)]
      yearsto_ec <- decimal_date(as.Date(ec, origin=origin)) - decimal_date(start_date)
      return(yearsto_ec)
    }


# correct4date
    #Accepts 3 vectors of index dates (e.g. date of DM), binary events, and dates of events. 
    #Returns a vector equal to the binary events, except where the date of the event preceded the index date.  
    #In the latter case, the binary event is replaced with NA.
    correct4date <- function(indexdate.col, Case.col, Casedate.col){
      Case.col[Casedate.col < indexdate.col] <- NA #do NOT want as controls or cases
      return(Case.col)
    }
