library(rvest)
library(rJava)
library(RSelenium)
# Lenovo Product Scraper 2 (reads a product page list directly eg .../p/91731037)
# For UK, US region page formats

# Get links from ordinary scrapable pages eg .../c/legion-7-series
linklist <- read.delim("lenovo_US_urls.txt", stringsAsFactor=F, header=F)
listtwo <- list()

for (j in 1:nrow(linklist)) {
  pagehtml <- read_html(linklist[j,])
  
  # Scrape list of indiv product pages
  firstlist <- pagehtml %>% 
    html_nodes("h3.seriesListings-title > a") %>% 
    html_attr("href") %>% 
    na.omit()
  firstlist <- sprintf("https://www.lenovo.com%s", firstlist)
  listtwo <- append(listtwo, firstlist)
}

write.table(listtwo, "lenovo_US_prodlinks.txt", sep = "\n", dec = ".",
            row.names = F, col.names = F, quote = FALSE)

# At this point, combine the txt file above with the 2nd set of product page links to iterate through
# Skip above steps if product links txt (.../p/...) are already populated

# kill any orphaned java processes to free up ports
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)

# start up local selenium server 
rD <- rsDriver(browser="chrome",
               chromever="90.0.4430.24", 
               extraCapabilities = list(
                 chromeOptions = list(
                   args = list('--headless','--disable-gpu',
                               '--disable-logging','--silent', '--log-level=3')
                 )
               ), 
               verbose = F
)
remDr <- rD$client
remDr$open()

namelist <- c()
processorlist <- c()
storagelist <- c()
pricelist <- c()

# Read the combined product link file .../p/12937210jas
pagelist <- read.delim("lenovo_UK_prodlinks.txt", stringsAsFactor=F, header=F)

for (i in 1:nrow(pagelist)) {
  cat("\n--- URL #",i," of ",nrow(pagelist),": ", sep="")
  currentpage <- pagelist[i,]
  try({
      remDr$navigate(currentpage)
      # Delay 2s to let page elements load before grabbing
      Sys.sleep(2)
      # extract ALL product names
      item_element <- remDr$findElements(using = "css", 
                                         'h3.tabbedBrowse-productListing-title')
      names <- c()
      names <- unlist(lapply(item_element, function(x){x$getElementText()}))  
      names <- gsub("\n", " ", names)
      
      # extract ALL storage data
      storage <- c()
      item_element <- remDr$findElements(using = "xpath", 
                                         "//dd[contains(text(),'SSD') or contains(text(),'HDD')]")
      storage <- unlist(lapply(item_element, function(x){x$getElementText()}))
      
      # extract ALL processor data
      processor <- c()
      item_element <- remDr$findElements(using = "css", 
                                         'dd.pdp-valuerow-0')
      processor <- unlist(lapply(item_element, function(x){x$getElementText()}))
      
      # extract ALL price data
      # USA page selector: dd.saleprice.pricingSummary-details-final-price
      # UK page selector: div.saleprice
      prices <- c()
      item_element <- remDr$findElements(using = "css", 
                                         'div.saleprice')
      prices <- unlist(lapply(item_element, function(x){x$getElementText()}))
      
    }, silent=T)
  
  namelist <- append(namelist, names)
  processorlist <- append(processorlist, processor)
  storagelist <- append(storagelist, storage)
  pricelist <- append(pricelist, prices)
  
  minlength2 <- min(length(namelist),length(storagelist),length(processorlist),length(pricelist))
  namelist <- namelist[1:minlength2]
  processorlist <- processorlist[1:minlength2]
  storagelist <- storagelist[1:minlength2]
  pricelist <- pricelist[1:minlength2]
  
  cat(length(namelist),length(processorlist),length(storagelist),length(pricelist),sep=" ")
}


# close browser & stop server
remDr$close()
rD$server$stop()
rm(rD, remDr, item_element)
gc()
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)

# Split storage list into 2 columns - size and type
storagesize <- sub("(?<=[B]).*", "", storagelist, perl = TRUE)
storagetype <- sub(".*?B ", "", storagelist)

# Print rows of each column for troubleshooting mismatches
dflist <- list(namelist=namelist, processorlist=processorlist, pricelist=pricelist, storagelist=storagelist)
for (item in c("namelist","processorlist","pricelist", "storagelist")) {
  cat(item,": ", length(dflist[[item]]), " rows\n", sep="")
}

# Compile into dataframe
compiled <- data.frame(Brand = 'Lenovo', Model = namelist, Processor = processorlist, 
                       Price = pricelist, Size = storagesize, 
                       Storage = storagetype)

write.csv(compiled, "combined_lenovo_UK.csv", row.names=FALSE)
