pubs <- read.csv("12pcbusstops.csv", stringsAsFactors = F)

pubs <- pubs[,1:4]
names(pubs) <- c("parish", "pub", "bus", "extra")

pubs[pubs$bus == "LA Hocq Slipway W", ]$bus <- "Le Hocq Slipway W"
pubs[pubs$bus == "La Hocq Slipway E", ]$bus <- "Le Hocq Slipway E"



# But we only need to worry about stations at pubs
pubStops2a <-  pairdfDist[toupper(pairdfDist$fromPub) %in% toupper(pubs$bus), ]$fromPub %>% unique()
pubStops2b <-  pairdfDist[toupper(pairdfDist$toPub) %in% toupper(pubs$bus), ]$toPub %>% unique()


pairDFdist2 <- pairdfDist[toupper(pairdfDist$fromPub) %in% toupper(pubStops2a) & toupper(pairdfDist$toPub) %in% toupper(pubStops2b) , ]

pairDFdist2 <- pairDFdist2[,1:2]

pairDFdist2$fromPub <- as.character(pairDFdist2$fromPub)
pairDFdist2$toPub <- as.character(pairDFdist2$toPub)


unique(pairDFdist2$toPub) %>% sort()

pairDFdist2$fromPub <- replace(x = pairDFdist2$fromPub , 
                               list = grepl("Trinity Church", pairDFdist2$fromPub),
                                       value  = "Trinity Church" )

pairDFdist2$toPub <- replace(x = pairDFdist2$toPub , 
                               list = grepl("Trinity Church", pairDFdist2$toPub),
                               value  = "Trinity Church" )

pairDFdist2 <- distinct(pairDFdist2)                       

pairMatrix2 <- table(pairDFdist2)

pairNetwork2 <- as.network(pairMatrix2, vertex.attrnames = colnames(pairMatrix2))

plot(pairNetwork2, displaylabels = T, label.pos  = 5)

sundayBusesFinalReplace <- sundayBusesFinal

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




# Add a St H transit bus option

pubs2 <- rbind(pubs, c("ST_H_TRANSIT","ST_H_TRANSIT","Bus Station", FALSE))
pubs3 <- rbind(pubs2, c("ST_H_TRANSIT2","ST_H_TRANSIT2","Bus Station", FALSE))

pubs <- pubs3  
