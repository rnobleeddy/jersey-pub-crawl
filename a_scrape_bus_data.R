library(RCurl)
library(rvest)
library(dplyr)

# hardcode a list of buses
routes <- c('1', '1A','1G','2','2A','3','4','5','7','7a',
            '8','9','12','12B','13','14','15','16','19','20',
            '21','22','23','27','28')


urls <- paste0("https://libertybus.je/routes_times/timetables/",routes, "/FALSE")


pages <- RCurl::getURI(urls)


getTables <- function(page) {
  page %>% 
    read_html() %>%
    html_table()
}

tables <- vector(mode = "list", length = length(pages))

for(i in 1:length(pages)) {
  tables[[i]] <- getTables(pages[[i]])
  print(length(tables[[i]]))
}

getTables(pages[[6]])
