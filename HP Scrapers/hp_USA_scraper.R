# Run this chunk of code first to load required packages
if (!require("rvest")) install.packages("rvest")
if (!require("rJava")) install.packages("rJava")
if (!require("RSelenium")) install.packages("RSelenium")
if (!require("rstudioapi")) install.packages("rstudioapi")
library(rvest)
library(rJava)
library(RSelenium)
library(rstudioapi)

# Select all (Ctrl+A), then Run code (Ctrl+Enter)
# HP Product Scraper Type 3 (for USA page)

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

# Read the landing page links (no product specs yet - /mlp/ pages)
list1 <- read.delim("hpUSAlinks.txt", stringsAsFactor=F, header=F)  

links <- c()
# iterate thru landing pages to collate product links (those that include specs)
for (i in 1:nrow(list1)) {
  remDr$navigate(list1[i,])
  item_element <- remDr$findElements(using = "xpath", 
                                     "//p[@class='groupviewallarea']/a[contains(text(),'(View all)')]")
  templinks <- unlist(lapply(item_element, function(x){x$getElementAttribute("href")})) 
  links <- append(links, templinks)
}

write.table(links, "hp_USA_prod_links.txt",quote=F,sep="\n",row.names=F,col.names=F)

remDr$close()
rD$server$stop()
rm(rD, remDr, item_element)
gc()

# Skip to here after links ("...#vao") have been collated into prod_links file
# Feel free to add additional product links (ending in ..#vao) to the txt file
# Can highlight code after this line and run if product links already collated

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
links <- read.delim("hp_USA_prod_links.txt", stringsAsFactor=F, header=F)

# Iterate through actual product pages to grab product specs
for (j in 1:nrow(links)) {
  cat("\n--- URL #",j," of ",nrow(links),": ", sep="")

  try({
    remDr$navigate(links[j,])
    Sys.sleep(1) # 1s delay to let page load (hopefully) 
    # extract ALL product names
    item_element <- remDr$findElements(using = "xpath", 
                                       "//div[@class='productInfo2']/h3/a[@class='productHdr']")
    names <- c()
    names <- unlist(lapply(item_element, function(x){x$getElementText()}))  
    names <- gsub("\n", " ", names) %>% trimws()
    
    # extract ALL storage data
    storage <- c()
    item_element <- remDr$findElements(using = "xpath", 
                                       "//div[@class='productInfo2']/ul[@class='productFeatLst']/li/span[contains(text(),'SSD') or contains(text(),'HDD') or contains(text(),'eMMC') or contains(text(),'512 GB')]")
    storage <- unlist(lapply(item_element, function(x){x$getElementText()}))
    
    # extract ALL processor data
    processor <- c()
    item_element <- remDr$findElements(using = "xpath", 
                                       "//div[@class='productInfo2']/ul[@class='productFeatLst']/li/span[not(contains(text(),'Graphics') or contains(text(),'memory') or contains(text(),'Radeon')) and (contains(text(),'Intel') or contains(text(),'AMD') or contains(text(),'processor'))]")
    processor <- unlist(lapply(item_element, function(x){x$getElementText()}))
    
    # extract ALL price data
    prices <- c()
    item_element <- remDr$findElements(using = "xpath", 
                                       "//div[@class='productPrice']/span[@class='price_amount']")
    prices <- unlist(lapply(item_element, function(x){x$getElementText()}))
    
  }, silent=T)
  
  # Sometimes, get non-product listings (eg "Bundle"), need to remove 
  if ((length(names)==length(prices)) & (length(prices)!=length(storage))) {
    try({
      temp_df <- data.frame(temp_name=names, temp_price=prices, stringsAsFactors=F)
      temp_df <- temp_df[!grepl("Bundle", temp_df$temp_name),]
      names <- as.character(temp_df$temp_name) 
      prices <- as.character(temp_df$temp_price)
    },silent=T)
  }
  
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

# Print rows of each column for troubleshooting mismatches
dflist <- list(namelist=namelist, processorlist=processorlist, pricelist=pricelist, storagelist=storagelist)
for (item in c("namelist","processorlist","pricelist", "storagelist")) {
  cat(item,": ", length(dflist[[item]]), " rows\n", sep="")
}

# Compile into dataframe
compiled <- data.frame(Month="", Brand = 'HP', LOB="","Dell LOB"="",
                       Model = namelist, Processor = processorlist, 
                       USD.Price = pricelist, "Storage Size" = storagesize2, 
                       "Storage Type" = storagetype, "Other Storage Details"="",
                       "Product Type"="", Country="USA", URL=urllist)

write.csv(compiled, "hp_US_products.csv", row.names=FALSE)
