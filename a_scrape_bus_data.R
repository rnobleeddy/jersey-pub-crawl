library(RCurl)
library(rvest)
library(dplyr)
library(statnet)

# hardcode a list of buses
# 2A removed as page broken, also 20, 21
# this is fine, these servoces have no sunday servie
# 7a, 8, 19 removed as no sunday service
# remove the 23 as well, but we need this later
routes <- c('1', '1A','1G','2','3','4','5','7',
            '9','12','12A','13','14','15','16',
            '22','23','27','28')

# create urls
urls <- paste0("https://libertybus.je/routes_times/timetables/",routes, "/FALSE")

# get pages
pages <- RCurl::getURI(urls)

# function to parse page
getTables <- function(page) {
  page %>% 
    read_html() %>%
    html_table()
}

# Get tables

# setup vector
tables <- vector(mode = "list", length = length(pages))

for(i in 1:length(pages)) {
  tables[[i]] <- getTables(pages[[i]])
  print(length(tables[[i]]))
}

#routes[19]
noTables <- data.frame(routes,unlist(lapply(tables, length)))


#getTables(pages[[19]])

#tables[[19]] <- pages[[19]] %>% read_html() %>% html_nodes("table") %>% .[11:12] %>% html_table()
  
#View(tables[[18]][2])

sundayBuses <- vector(mode = "list", length = 100)
j <- 0

for(i in 1:length(tables)) {
  if(length(tables[[i]]) == 12) {
    # need 10 and 12 
    sundayBuses[j] <- tables[[i]][10]
    sundayBuses[j+1] <- tables[[i]][12]
    j <- j + 2
    
  } else if(length(tables[[i]]) == 10) {
    # need 10 and 8 
    sundayBuses[j] <- tables[[i]][10]
    sundayBuses[j+1] <- tables[[i]][8]
    j <- j + 2
  
  } else if(length(tables[[i]]) == 8) {
    # need 6 and 8 
    sundayBuses[j] <- tables[[i]][6]
    sundayBuses[j+1] <- tables[[i]][8]
    j <- j + 2
    
  } else if(length(tables[[i]]) == 6) {
    # need 6 
    sundayBuses[j] <- tables[[i]][6]
    j <- j + 1
    
  } else {
      stop("Unknown table config")
  }
}
  

sundayBusesFinal <- sundayBuses[1:j]



pairdf <- df <- data.frame(locA=character(), 
                           locB=character(), 
                           stringsAsFactors=FALSE)

for(j in 1:length(sundayBusesFinal)) {
  
  tmp <- sundayBusesFinal[[j]] 
  
  for(k in 1:length(sundayBusesFinal[[j]]$X2)) {
    
    for(l in k:length(sundayBusesFinal[[j]]$X2)) {

      pairdf <- rbind(pairdf, data.frame(as.character(tmp[k,2]), as.character(tmp[l,2])))
      
    }
    
  }
  
}

pairdfDist <- distinct(pairdf)


pairMatrix <- table(pairdfDist)

pairNetwork <- as.network(pairMatrix)

plot(pairNetwork)

#?statnet

# network is fun, but unnecessary
names(pairdfDist) <- c('fromPub','toPub')


busNoLookUp <- c("1", "1", "1A", "1A", "1G", "1G", "2","2", "3","3", "4","4", "5","5",
                 "7","7", "9","9","12", "12", "12A","12A", "13", "13",  "14", "14", "15", 
                 "15", "16", "22", "22", "23", "23", "27", "27", "28", "28"
                 )
