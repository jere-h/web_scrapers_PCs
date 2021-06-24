# Run this chunk of code first to load required packages
if (!require("rvest")) install.packages("rvest")
if (!require("rJava")) install.packages("rJava")
if (!require("RSelenium")) install.packages("RSelenium")
if (!require("rstudioapi")) install.packages("rstudioapi")
library(rstudioapi)
library(rvest)
library(rJava)
library(RSelenium)

# Select all (Ctrl+A), then Run code (Ctrl+Enter)
# HP Product Scraper Type 2 (for UK page)

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

for (i in 1:nrow(pagelist)) {
  cat("\n--- URL #",i," of ",nrow(pagelist),": ", sep="")
  currentpage <- pagelist[i,]
  
  try({
    remDr$navigate(currentpage)
    clickable <- remDr$findElements(using='xpath','//div[@class="sorting__numbers"]/a[contains(text(),"All")]')
    clickable[[1]]$clickElement() # Opens up ALL
    # Delay 3s to let page load, otherwise doesn't get any items
    Sys.sleep(3) 
    # extract ALL product names
    item_element <- remDr$findElements(using = "xpath", 
                                       "//div[@class='product__title']/h3/a")
    names <- c()
    names <- unlist(lapply(item_element, function(x){x$getElementText()}))  
    names <- gsub("\n", " ", names) %>% trimws()
    
    # extract ALL storage data
    storage <- c()
    item_element <- remDr$findElements(using = "xpath", 
                                       "//div[@class='product__specs']/ul/li[contains(text(),'SSD') or contains(text(),'HDD') or contains(text(),'eMMC')]")
    storage <- unlist(lapply(item_element, function(x){x$getElementText()}))
    
    # extract ALL processor data
    processor <- c()
    item_element <- remDr$findElements(using = "xpath", 
                                       "//div[@class='product__specs']/ul/li[2]")
    processor <- unlist(lapply(item_element, function(x){x$getElementText()}))
    
    # extract ALL price data
    prices <- c()
    item_element <- remDr$findElements(using = "css", 
                                       "div.pb-price>div>p.pb-price__now>nobr")
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
storagesize <- sub(".*?; ", "", storagelist, perl = TRUE)
storagesize2 <- sub("(?<=[B]).*", "", storagesize, perl = TRUE)
storagetype <- sub(".*?B ", "", storagesize)
pricelist <- sub("£","",pricelist) %>% trimws()

# Print rows of each column for troubleshooting mismatches
dflist <- list(namelist=namelist, processorlist=processorlist, pricelist=pricelist, storagelist=storagelist)
for (item in c("namelist","processorlist","pricelist", "storagelist")) {
  cat(item,": ", length(dflist[[item]]), " rows\n", sep="")
}

compile0 <- data.frame(Month="", Brand = 'HP', LOB="", "Dell LOB"="", Model = namelist, Processor = processorlist, 
                       GBP.Price = pricelist)
compile0 <- compile0[!grepl("Thin Client", compile0$Model),]
compile0$Storage.Size <- storagesize2
compile0$Storage.Type <- storagetype
compile0$Country <- 'UK'

write.csv(compile0, "hp_UK_products.csv", row.names=FALSE)
