if (!require("rvest")) install.packages("rvest")
if (!require("rstudioapi")) install.packages("rstudioapi")
library(rstudioapi)
library(rvest)

# Select all (Ctrl+A), then Run code (Ctrl+Enter)
# HP Product Scraper Type 1 (for Singapore site)

# Set source folder as working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pagelist0 <- read.delim("hp_SG_urls.txt", stringsAsFactor=F, header=F)

namelist <- c()
processorlist <- c()
storagelist <- c()
pricelist <- c()
urllist <- c()

for (i in 1:nrow(pagelist0)) {
  cat("\n--- URL #",i," of ",nrow(pagelist0),": ", sep="")
  pagehtml <- read_html(pagelist0[i,]) 
  
  # Scrape list of indiv product pages
  pagelist <- pagehtml %>% 
    html_nodes("div.product.details.product-item-details > strong > a.product-item-link") %>% 
    html_attr("href") %>% 
    na.omit()
  
  urllist <- append(urllist, pagelist)
  
  # Iterate through product pages
  cat(length(pagelist), "products found; ")
  for (i in 1:length(pagelist)) {
    currentpage <- read_html(pagelist[i])
    
    # Scrape product name
    prodname <- currentpage %>% 
      html_node("h1.page-title > span") %>% 
      html_text(trim=T) %>% 
      na.omit()
    
    # Processor details
    currprocessor <- currentpage %>% 
      html_node(xpath="//tr[@class='item']/td[@data-th='Processor']") %>% 
      html_text(trim=T)
    
    # Storage details
    currstorage <- currentpage %>% 
      html_node(xpath = "//tr[@class='item']/td[@data-th='Hard drive description']") %>%
      html_text(trim=T)
    
    # Price details
    currprice <- currentpage %>% 
      html_node(xpath="//span[@data-price-type='finalPrice']/span[@class='price']") %>% 
      html_text(trim=T)
    
    namelist <- append(namelist, prodname)
    processorlist <- append(processorlist, currprocessor)
    storagelist <- append(storagelist, currstorage)
    pricelist <- append(pricelist, currprice)
    
    minlength2 <- min(length(namelist),length(storagelist),length(processorlist),length(pricelist))
    namelist <- namelist[1:minlength2]
    processorlist <- processorlist[1:minlength2]
    storagelist <- storagelist[1:minlength2]
    pricelist <- pricelist[1:minlength2]
  }
  urllist <- urllist[1:minlength2]
  cat(length(namelist),length(processorlist),length(storagelist),length(pricelist),sep=" ")
}

# Combine any extra storage terms (eg "optane memory")
tagged_locations <- c()
for (s in 1:length(storagelist)) {
  if (grepl("Memory", storagelist[s], fixed = TRUE)) {
    storagelist[s-1] <- paste(storagelist[s-1], storagelist[s], sep=" with ")
    tagged_locations <- append(tagged_locations, -s)
  }
} 
if (length(tagged_locations)>0) { # otherwise storagelist will be wiped
  storagelist <- storagelist[tagged_locations]
}

storagesize <- sub("(?<=[B]).*", "", storagelist, perl = TRUE)
storagetype <- sub(".*?B ", "", storagelist)
storagesize2 <- c()

# Storagesize may contain excess terms (eg. M.2 2230 256 GB), need to remove them
# Storagesize2 will contain only the size values (eg. 256 GB)
for (i in 1:length(storagesize)) {
  if (is.na(storagesize[i])) {
    storagesize2 <- append(storagesize2, " ")
  } else {
    if (nchar(storagesize[i]) >= 6) {
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
}

# Print rows of each column for troubleshooting mismatches
dflist <- list(namelist=namelist, processorlist=processorlist, pricelist=pricelist, storagelist=storagelist)
for (item in c("namelist","processorlist","pricelist", "storagelist")) {
 cat(item,": ", length(dflist[[item]]), " rows\n", sep="")
}

# Compile into dataframe
compiled <- data.frame(Month="", Brand = 'HP', LOB="","Dell LOB"="",
                       Model = namelist, Processor = processorlist, 
                       "SGD Price" = pricelist, "Storage Size" = storagesize2, 
                       "Storage Type" = storagetype, "Other Storage Details"="",
                       "Product Type"="", Country="Singapore", URL=urllist)

# Export compiled dataframe as CSV
write.csv(compiled, "hp_SG_products.csv", row.names=FALSE)
