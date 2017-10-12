library(RCurl)
library(rvest)
library(dplyr)
library(statnet)

source("a_scrape_bus_data.R")

# hardcode a list of buses
# I removed those without a sunday service by hand
routes <- c('1', '1A','1G','2','3','4','5','7',
            '9','12','12A','13','14','15','16',
            '22','23','27','28')

# create urls - there is no API, so we scrape the website
urls <- paste0("https://libertybus.je/routes_times/timetables/",routes, "/FALSE")

# Get the sunday routes - this parses the bus data  
allBusData <- scrapeBusData(urls)

# Pull out the Sunday routes - this is a list of timetables, albeit in a messy format
sundayBusData <- extractSundayService(allBusData)

# From the timetable, create all pairs of journeys possible
allowedRoutes <- getAllowedJourneys(sundayBusData)

# Once we do this, we need a way to map our list of routes back to bus numbers. We did this by hand
busNoLookUp <- c("1", "1", "1A", "1A", "1G", "1G", "2","2", "3","3", "4","4", "5","5",
                 "7","7", "9","9","12", "12", "12A","12A", "13", "13",  "14", "14", "15", 
                 "15", "16", "22", "22", "23", "23", "27", "27", "28", "28"
)


# Look at the network of locations oin Jersey
pairMatrix <- table(pairdfDist)
pairNetwork <- as.network(pairMatrix)
plot(pairNetwork)

# Get pub data
pubs <- getPubs()

# Reduce our list of journeys to just pub-to-pub journeys
pubConnections <- filterBusRoutesToPubs(pubs, allowedRoutes)  

# Reduce our bus time tables to just pub-to-pub journeys  
pub2pubBusTimetables <- reduceBusData(sundayBusData, pubConnections)
  


pairMatrix2 <- table(pairDFdist2)

pairNetwork2 <- as.network(pairMatrix2, vertex.attrnames = colnames(pairMatrix2))

plot(pairNetwork2, displaylabels = T, label.pos  = 5)

