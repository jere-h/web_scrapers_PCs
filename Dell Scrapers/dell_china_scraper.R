# Run this chunk of code first to load required packages
if (!require("rvest")) install.packages("rvest")
if (!require("rJava")) install.packages("rJava")
if (!require("RSelenium")) install.packages("RSelenium")
if (!require("xlsx")) install.packages("xlsx")
if (!require("rstudioapi")) install.packages("rstudioapi")
library(rstudioapi)
library(xlsx)
library(rvest)
library(rJava)
library(RSelenium)

# Select all (Ctrl+A), then Run code (Ctrl+Enter)
# Dell Product Scraper (for China page)

# Set source folder as working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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
urllist <- c()

# Read the combined product link file 
pagelist <- read.delim("dell_china_urls.txt", stringsAsFactor=F, header=F)

for (i in 1:nrow(pagelist)) {
  cat("\n--- URL #",i," of ",nrow(pagelist),": ", sep="")
  currentpage <- pagelist[i,]
  
  try({
    remDr$navigate(currentpage)
    # Delay 2s to let page load, otherwise doesn't get any items
    Sys.sleep(2) 
    
    # Scrape list of indiv product pages
    temp_urls <- c()
    item_element <- remDr$findElements(using = "xpath", 
                                       "//div[@class='ps-title']/h4/a")
    temp_urls <- unlist(lapply(item_element, function(x){x$getElementAttribute('href')}))
    
    
    # extract ALL product names
    item_element <- remDr$findElements(using = "xpath", 
                                       "//div[@class='ps-title']/h4/a/span")
    names <- c()
    names <- unlist(lapply(item_element, function(x){x$getElementText()}))  
    names <- gsub("\n", " ", names) %>% trimws()
    names <- names[names != ""]
    
    # extract ALL storage data
    storage <- c()
    item_element <- remDr$findElements(using = "xpath", 
                                       "//div[@class='ps-iconography-specs']/ul/li/div[1]/span/small[contains(text(), '硬盘')]")
    storage <- unlist(lapply(item_element, function(x){x$getElementText()}))
    
    # extract ALL processor data
    processor <- c()
    item_element <- remDr$findElements(using = "xpath", 
                                       "//div[@class='ps-iconography-specs']/ul/li[1]/div[1]/span/small")
    processor <- unlist(lapply(item_element, function(x){x$getElementText()}))
    
    # extract ALL price data
    prices <- c()
    item_element <- remDr$findElements(using = "xpath", 
                                       "//div[@class='ps-simple-price']/h4/span/strong[@data-testid='psDellPrice']")
    prices <- unlist(lapply(item_element, function(x){x$getElementText()}))
    
  }, silent=T)
  
  urllist <- append(urllist, temp_urls)
  namelist <- append(namelist, names)
  processorlist <- append(processorlist, processor)
  storagelist <- append(storagelist, storage)
  pricelist <- append(pricelist, prices)
  
  minlength2 <- min(length(namelist),length(storagelist),length(processorlist),length(pricelist))
  namelist <- namelist[1:minlength2]
  processorlist <- processorlist[1:minlength2]
  storagelist <- storagelist[1:minlength2]
  pricelist <- pricelist[1:minlength2]
  urllist <- urllist[1:minlength2]
  
  cat(length(namelist),length(processorlist),length(storagelist),length(pricelist),sep=" ")
}

# close browser & stop server
remDr$close()
rD$server$stop()
rm(rD, remDr, item_element)
gc()
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)

# Split storage list into 2 columns - size and type
storagetype <- sub(".*?B", "", storagelist) %>% trimws(whitespace = "[ \t\r\n,]")
storagesize <- sub("(?<=[B]).*", "", storagelist, perl = TRUE)
storagesize2 <- c()
pricelist <- sub("¥ ","",pricelist) %>% trimws()

# Storagesize may contain excess terms (eg. M.2 2230 256 GB), need to remove them
# Storagesize2 will contain only the size values (eg. 256 GB)
for (i in 1:length(storagesize)) {
  if (nchar(storagesize[i]) > 6) {
    checkspace <- substr(storagesize[i],nchar(storagesize[i])-2, nchar(storagesize[i])-2)
    if (checkspace==" ") { # Spaces eg "256 GB"
      # split by space, get the element in 2nd last index
      currsize <- paste(sapply(strsplit(storagesize[i], " "), tail, 2),collapse=" ")
      # replace excess terms into storagetype
      storagetype[i] <- paste(trimws(sub(currsize, "", storagesize[i])), storagetype[i], sep=" ")
    } else { # No space eg "256GB"
      currsize <- sapply(strsplit(storagesize[i], " "), tail, 1)
      # replace excess terms into storagetype
      storagetype[i] <- paste(trimws(sub(currsize, "", storagesize[i])), storagetype[i], sep=" ")
      # Add a space for consistent formatting
      currsize <- sub("([TB|GB])", " \\1", currsize, perl=T)
    }
    storagesize2 <- append(storagesize2, currsize)
  } else {
    storagesize2 <- append(storagesize2, storagesize[i])
  }
}

# Print rows of each column for troubleshooting mismatches
dflist <- list(namelist=namelist, processorlist=processorlist, pricelist=pricelist, storagelist=storagelist)
for (item in c("namelist","processorlist","pricelist", "storagelist")) {
  cat(item,": ", length(dflist[[item]]), " rows\n", sep="")
}

# Compile into dataframe
compiled <- data.frame(Month="", Brand = 'Dell', LOB="","Dell LOB"="",
                       Model = namelist, Processor = processorlist, 
                       "YuanPrice" = pricelist, "Storage Size" = storagesize2, 
                       "Storage Type" = storagetype, "Other Storage Details"="",
                       "Product Type"="", Country="China", URL=urllist)

write.xlsx(compiled, "dell_china_products.xlsx", row.names=FALSE)
