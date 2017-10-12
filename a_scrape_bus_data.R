
# function to parse page
getTables <- function(page) {
  page %>% 
    read_html() %>%
    html_table()
}

# function to scrape bus data
scrapeBusData <- function(urls) {
  
  # get pages
  pages <- RCurl::getURI(urls)

  # ----- Get tables ------ 
  
  # setup vector
  tables <- vector(mode = "list", length = length(pages))
  
  for(i in 1:length(pages)) {
    tables[[i]] <- getTables(pages[[i]])
    print(length(tables[[i]]))
  }
  
  return(tables)
}

extractSundayService <- function(bustables) {
  
  # this is is a hack - we can look at the bus timetables and infer from the number of tabes
  # on  the page, which tabels are the sunday service
  # For example, if there are 12 tables, we know its a 2 way route, with tables for Mon-Fri, Sat, sun
  # Half the tables are data, the other are just used to construct the page
  
  # And of course, this is poor R, but sometimes working code is better than good code!
  
  sundayBuses <- vector(mode = "list", length = 100)
  
  j <- 0
  
  for(i in 1:length(bustables)) {
    if(length(bustables[[i]]) == 12) {
      # need 10 and 12 
      sundayBuses[j] <- bustables[[i]][10]
      sundayBuses[j+1] <- bustables[[i]][12]
      j <- j + 2
      
    } else if(length(bustables[[i]]) == 10) {
      # need 10 and 8 
      sundayBuses[j] <- bustables[[i]][10]
      sundayBuses[j+1] <- bustables[[i]][8]
      j <- j + 2
    
    } else if(length(bustables[[i]]) == 8) {
      # need 6 and 8 
      sundayBuses[j] <- bustables[[i]][6]
      sundayBuses[j+1] <- bustables[[i]][8]
      j <- j + 2
      
    } else if(length(bustables[[i]]) == 6) {
      # need 6 
      sundayBuses[j] <- bustables[[i]][6]
      j <- j + 1
      
    } else {
        stop("Unknown table config")
    }
  }
    
  # Reduce our list to just thoese we found
  sundayBusesFinal <- sundayBuses[1:j]
  
  return(sundayBusesFinal)
  
}

getAllowedJourneys <- function(sundayBusesFinal) {
  # Setup a data frame to put our data in - covering all pairs of start and end locations
  # that are possible on a Sunday
  pairdf <- data.frame(locA=character(), 
                             locB=character(), 
                             stringsAsFactors=FALSE)
  
  # Again, loop, because it's easier to code
  for(j in 1:length(sundayBusesFinal)) {
    
    tmp <- sundayBusesFinal[[j]] 
    
    for(k in 1:length(sundayBusesFinal[[j]]$X2)) {
      
      for(l in k:length(sundayBusesFinal[[j]]$X2)) {
  
        pairdf <- rbind(pairdf, data.frame(as.character(tmp[k,2]), as.character(tmp[l,2])))
        
      }
      
    }
    
  }
  
  
  
  # Ony want pairs of data
  pairdfDist <- distinct(pairdf)
  
  # rename
  names(pairdfDist) <- c('fromPub','toPub')
  
  
  return(pairdfDist)
}




