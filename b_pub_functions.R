# --------------------------------------------------------------------------------------------
# Reads in handmade pub data around which pubs we like to go to!
# Returns:
#   pubs =  a data frame of contender pubs
getPubs <- function() {
  pubs <- read.csv("12pcbusstops.csv", stringsAsFactors = F)
  
  pubs <- pubs[,1:4]
  names(pubs) <- c("parish", "pub", "bus", "extra")
  
  # Hardcode a correction 
  pubs[pubs$bus == "LA Hocq Slipway W", ]$bus <- "Le Hocq Slipway W"
  pubs[pubs$bus == "La Hocq Slipway E", ]$bus <- "Le Hocq Slipway E"
  
  return(pubs)
}


# --------------------------------------------------------------------------------------------
# Filter our list of possible bus journeys to just those that are at pubs we want to go to
# Parameters:
#   pubs = a dataframe of pubs we want to visit
#   pairdfDist = a list of distinct bus journeys across all of Jersey
# Returns:
#   pairDFdist2 = a cust down version of pairdfDist. a list of distinct bus journey between pubs
filterBusRoutesToPubs <- function(pubs, pairdfDist) {
  
  # But we only need to worry about stations at pubs
  pubStops2a <-  pairdfDist[toupper(pairdfDist$fromPub) %in% toupper(pubs$bus), ]$fromPub %>% unique()
  pubStops2b <-  pairdfDist[toupper(pairdfDist$toPub) %in% toupper(pubs$bus), ]$toPub %>% unique()
  
  # Create a list of journeys that are to/from pubs of interest
  pairDFdist2 <- pairdfDist[toupper(pairdfDist$fromPub) %in% toupper(pubStops2a) & toupper(pairdfDist$toPub) %in% toupper(pubStops2b) , ]
  
  # Fix types
  pairDFdist2$fromPub <- as.character(pairDFdist2$fromPub)
  pairDFdist2$toPub <- as.character(pairDFdist2$toPub)
  
  # Hard code some fixes
  pairDFdist2$fromPub <- replace(x = pairDFdist2$fromPub , 
                                 list = grepl("Trinity Church", pairDFdist2$fromPub),
                                 value  = "Trinity Church" )
  
  pairDFdist2$toPub <- replace(x = pairDFdist2$toPub , 
                               list = grepl("Trinity Church", pairDFdist2$toPub),
                               value  = "Trinity Church" )
  
  # Get rid of dupes (ie two buses service this route - may be unnecessary)
  pairDFdist2 <- distinct(pairDFdist2)     
  
  return(pairDFdist2)
}

# --------------------------------------------------------------------------------------------
# Reduce our bus timetables to just those relevent for where we're going
# Parameters:
#   sundayBusesFinal = a dataframe of all bus timetables
#   pairDFdist2 = a dataframe of all the pub to pub journeys we might make
# Returns :
#   sundayBusesFinalReplace = a cut down version of sundayBusesFinal for just the journeys of interest
reduceBusData <- function(sundayBusesFinal, pairDFdist2) {
  
  sundayBusesFinalReplace <- sundayBusesFinal
  
  # Hardcode some fixes
  # replace trinity stops
  for(b in 1:length(sundayBusesFinalReplace)) {
    sundayBusesFinalReplace[[b]]$X2 <- replace(x = sundayBusesFinalReplace[[b]]$X2 , 
                                               list = grepl("Trinity Church",sundayBusesFinalReplace[[b]]$X2),
                                               value  = "Trinity Church" )
  }
  
  # Get all stops
  rbind(df, setNames(rev(df), names(df)))
  allStops <- c(pairDFdist2$fromPub, pairDFdist2$toPub) %>% unique()
  
  # Reduce to just rows at relevent stops
  for(b in 1:(length(sundayBusesFinalReplace) - 1)) {
    print(b)
    sundayBusesFinalReplace[[b]] <- sundayBusesFinalReplace[[b]][sundayBusesFinalReplace[[b]]$X2 %in% allStops, ]
  }

  return(sundayBusesFinalReplace)

}



# Add a St H transit bus option

pubs2 <- rbind(pubs, c("ST_H_TRANSIT","ST_H_TRANSIT","Bus Station", FALSE))
pubs3 <- rbind(pubs2, c("ST_H_TRANSIT2","ST_H_TRANSIT2","Bus Station", FALSE))

pubs <- pubs3  
