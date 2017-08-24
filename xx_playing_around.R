library(dplyr)

'%!in%' <- function(x,y)!('%in%'(x,y))


pubsClean <- pubs
pubsClean$bus  <- replace(x = pubsClean$bus , 
                          list = grepl("Trinity Church", pubsClean$bus),
                          value  = "Trinity Church" )


pairDFdist2$fromPub <- toupper(pairDFdist2$fromPub)
pairDFdist2$toPub <- toupper(pairDFdist2$toPub)
pubsClean$bus <- toupper(pubsClean$bus)




fullPubBusData <- left_join(pairDFdist2, pubsClean,  by = c("fromPub" = "bus"))


names(fullPubBusData) <- c("fromBus", "toBus",   "parish",  "pub",     "extra") 

save(fullPubBusData, sundayBusesFinalReplace, file = 'base.Rdata')

# Pick a random starting point

# Create a 2nd st helier for bus transitting
sth2copy <- fullPubBusData[fullPubBusData$parish == "St Helier", ]
sth2copy$parish <- "ST_H_TRANSIT"
sth2copy$pub <- "ST_H_TRANSIT"

fullPubBusDataExtraSP <- rbind(fullPubBusData, sth2copy)

sth2copy$parish <- "ST_H_TRANSIT2"
sth2copy$pub <- "ST_H_TRANSIT2"

fullPubBusDataExtraSP <- rbind(fullPubBusDataExtraSP, sth2copy)

winningSolutions <- 0

for(i in 1:10000000) {
  
  if(i %% 100 == 0) print(i)
  
  start <- sample_n(fullPubBusDataExtraSP, 1)
  
  visitedParishes <- start$parish
  visitedPubs <- start$pub
  fromStops <- c()
  toStops <- c()
  
  complete <- TRUE
  
  # Find all possible jumps
  move <- fullPubBusDataExtraSP[fullPubBusDataExtraSP$parish %!in% visitedParishes & fullPubBusDataExtraSP$fromBus == start$toBus, ]
  
  
  for(a in 1:length(unique(fullPubBusDataExtraSP$parish))) {
    
    # does the move work?
    
    
              
    if(nrow(move)==0 & a != length(unique(fullPubBusDataExtraSP$parish))) {
      #print(a)
      complete <- FALSE
      break()
    } else {
      
      
      
      
      if(a==length(unique(fullPubBusDataExtraSP$parish))) {
       
         print(' ---- win ----- ')
         #print(visitedParishes)
         #print(visitedPubs)
         #print(move)
        
      } else {
      
        move <- sample_n(move, 1)
        
        toStops <- c(toStops, move$fromBus)
        fromStops <- c(fromStops, move$toBus)
        
        visitedParishes <- c(visitedParishes, move$parish)
        visitedPubs <- c(visitedPubs, move$pub) 
        
        #toStops <- c(toStops, move$toBus)
        
       
        # update move 
        
        move <- fullPubBusDataExtraSP[fullPubBusDataExtraSP$parish %!in% visitedParishes & fullPubBusDataExtraSP$toBus == move$fromBus, ]
        
        
        
      }
    }
  }
  
  if(complete) {
    library(chron)
   
    blIWon <- TRUE
    
    curTime <- times("11:00:00")
    pubMoveCounter <- 0
    
    outputText <- ""

    
    for(p in 1:(length(unique(fullPubBusDataExtraSP$parish))-1)) {
      
      # half hour per pub
      if(visitedParishes[p] != "ST_H_TRANSIT" & visitedParishes[p] != "ST_H_TRANSIT2") {
        curTime <- curTime + times("00:20:00")
      }
      
      stopsA <- pubs[pubs$pub == visitedPubs[p],]$bus
      stopsB <- pubs[pubs$pub == visitedPubs[p+1],]$bus
      
      earliestDepTime <- times("23:30:00")
      earliestArrTime <- times("23:31:00")
      
      winningRouteIndice <- -1
      
      for(b in 1:length(sundayBusesFinalReplace)) {
      
        
        
        lclTable <- sundayBusesFinalReplace[[b]]
        
        if(!is.null(nrow(lclTable))) {
          possRoutes <- rbind(lclTable[toupper(lclTable$X2) %in% toupper(stopsA), ] , lclTable[toupper(lclTable$X2) %in% toupper(stopsB), ])
          
          if(nrow(possRoutes)>=2) {
            
            
            stop1indice <- min(which(possRoutes$X2 %in% stopsA ))
            stop2indice <- min(which(possRoutes$X2 %in% stopsB ))
            
            if(!is.infinite(stop1indice) & !is.infinite(stop2indice))
            { 
              passRoutesInUse <- rbind(possRoutes[stop1indice,], possRoutes[stop2indice,])
              
              lastCol <- ncol(passRoutesInUse) -1
              firstCol <- 5
              
              for(c in firstCol:lastCol) {
              
                thisDepTm <-  tryCatch({times(paste0(passRoutesInUse[1,c], ":00"))},
                                       warning = function(e) {
                                         times("23:30:00")},
                                       error = function(e) {
                                         times("23:30:00")})
                
                thisArrTm <- tryCatch({times(paste0(passRoutesInUse[2,c], ":00"))},
                                      warning = function(e) {
                                        times("23:30:00")},
                                      error = function(e) {
                                        times("23:30:00")})
                
                
                if(thisArrTm < thisDepTm) {
                  thisDepTm <- times("23:30:00")
                  thisArrTm <- times("23:31:00")
                  
                }
                
                #browser()
                if(thisDepTm > curTime && thisDepTm < earliestDepTime) {
                  earliestDepTime <- thisDepTm
                  earliestArrTime <- thisArrTm
                  #browser()
                  winningRouteIndice <- b
                }
              }
            }
          }
        }
      }
      
      #browser()
      
      
      
      outputText <- paste0(outputText, " I drink at ", visitedPubs[p], "\n")
      outputText <- paste0(outputText, "I depart at ", earliestDepTime, " on the number ", busNoLookUp[winningRouteIndice], "\n")
      outputText <- paste0(outputText, " I arrive at the  ", visitedPubs[p+1], "\n")
      outputText <- paste0(outputText, "I arrive at ", earliestArrTime, "\n")
      #browser()
      curTime <- earliestArrTime
      
      if(earliestArrTime > times("23:00:01")) {
        outputText <- paste0(outputText, "I run out of time")
        blIWon <- FALSE
      } else {
        pubMoveCounter <- pubMoveCounter+ 1
      }
    }

    if(blIWon) {
      outputText <- paste0(outputText,"we won it")
      writeLines(outputText, paste0("output", winningSolutions, ".txt"))
      winningSolutions <- winningSolutions + 1
      print(paste0("Found winner ", winningSolutions))
      print(outputText)
      
    } else {
      outputText <- paste0(outputText,paste0("managed ",  pubMoveCounter, " moves"), "\n")
      print(paste0("Found a route with ", pubMoveCounter, " pubs"))
    }
  }
}
  
# 
# why does this not fnd a route at 2pm?
# 
# "St Mary's Country Inn"
# "St John's Country Inn"
# 
# 
# stopsA <- pubs[pubs$pub == "St Mary's Country Inn",]$bus
# stopsB <- pubs[pubs$pub == "St John's Country Inn",]$bus
# 
# for(b in 1:length(sundayBusesFinalReplace)) {
#   print(b)
# 
#   lclTable <- sundayBusesFinalReplace[[b]]
# 
#   print(paste0("rows=", nrow(lclTable)))
#   
#   if(!is.null(nrow(lclTable))) {
#     if(b==14) {browser()}
#     possRoutes <- rbind(lclTable[toupper(lclTable$X2) %in% toupper(stopsA), ] , lclTable[toupper(lclTable$X2) %in% toupper(stopsB), ])
#     
#     if(nrow(possRoutes)>=2) {
#       
#       stop1indice <- min(which(possRoutes$X2 %in% stopsA ))
#       stop2indice <- min(which(possRoutes$X2 %in% stopsB ))
#       
#       passRoutesInUse <- rbind(possRoutes[stop1indice,], possRoutes[stop2indice,])
#       }
#     }
# }
# 
# for(b in 1:length(sundayBusesFinalReplace)) {
#   lclTable <- sundayBusesFinalReplace[[b]]
#   print(b)
#   print(unique(lclTable$X2))
# }