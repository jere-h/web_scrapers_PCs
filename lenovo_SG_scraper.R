lenovo_scraper <- function(url) {
  if (length(url)!=1) {
    stop("One argument must be supplied (URL)", call.=FALSE)
  } 
  
  if (!require("rvest")) install.packages("rvest")
  if (!require("rJava")) install.packages("rJava")
  if (!require("RSelenium")) install.packages("RSelenium")
  
  library(rvest)
  library(rJava)
  library(RSelenium)
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
  
  # Lenovo Catalogue Scraper
  # Insert link to product listing here:
  pagehtml <- read_html(url)
  
  # Run the code below to get product data
  # Scrape product category for file naming
  categoryname <- pagehtml %>% 
    html_node('meta[name="category.id"]') %>%
    html_attr("content")
  
  # Scrape product names
  prodnames <- pagehtml %>% 
    html_nodes("h3.seriesListings-title > a") %>% 
    html_text(trim=T) %>% 
    na.omit()
  
  # Scrape list of indiv product pages
  pagelist <- pagehtml %>% 
    html_nodes("h3.seriesListings-title > a") %>% 
    html_attr("href") %>% 
    na.omit()
  pagelist <- sprintf("https://www.lenovo.com%s", pagelist)
  
  processorlist <- c()
  storagelist <- c()
  pricelist <- c()
  
  # Iterate through product pages
  if (length(pagelist)>1) {
    cat(length(pagelist), "product URLs found, getting individual product data...\n")
    for (i in 1:length(pagelist)) {
      currentpage <- read_html(pagelist[i])
      
      currprocessor <- currentpage %>% 
        html_node("li[data-pn='Processor'] > div > p") %>% 
        html_text(trim=T)
      
      currstorage <- currentpage %>% 
        html_node("li[data-pn='Hard Drive'] > div > p") %>% 
        html_text(trim=T)
      
      currprice <- currentpage %>% 
        html_node("dd.saleprice.pricingSummary-details-final-price") %>% 
        html_text(trim=T)
      
      # For dynamic pages, storage data might be missing
      # Use Selenium to load dynamic pages
      if (is.na(currstorage)) {
        try({
        # navigate to page
        remDr$navigate(pagelist[i])
        
        # extract storage data
        item_element <- remDr$findElement(using = "css", 
                                          'dd.pdp-valuerow-3')
        currstorage <- item_element$getElementText()[[1]]
        
        # extract processor data
        item_element <- remDr$findElement(using = "css", 
                                          'dd.pdp-valuerow-0')
        currprocessor <- item_element$getElementText()[[1]]
        }, silent=T)
      }
      processorlist <- append(processorlist, currprocessor)
      storagelist <- append(storagelist, currstorage)
      pricelist <- append(pricelist, currprice)
    }
  }
  
  # If main page only shows 1 product, check for nested listings
  if (length(pagelist)==1) {
    warning("1 product URL found, checking for nested listings...\n")
    try({
      # navigate to page
      remDr$navigate(pagelist[1])
      # Delay 2s to let page elemends load in before scraping
      Sys.sleep(2)
      # extract ALL product names
      item_element <- remDr$findElements(using = "css", 
                                         'h3.tabbedBrowse-productListing-title')
      prodnames <- c()
      prodnames <- unlist(lapply(item_element, function(x){x$getElementText()}))  
      prodnames <- gsub("\n", " ", prodnames)
      
      # extract ALL storage data
      item_element <- remDr$findElements(using = "css", 
                                        'dd.pdp-valuerow-3')
      storagelist <- unlist(lapply(item_element, function(x){x$getElementText()}))
      
      # extract ALL processor data
      item_element <- remDr$findElements(using = "css", 
                                        'dd.pdp-valuerow-0')
      processorlist <- unlist(lapply(item_element, function(x){x$getElementText()}))
      
      # extract ALL price data
      item_element <- remDr$findElements(using = "css", 
                                         'dd.saleprice.pricingSummary-details-final-price')
      pricelist <- unlist(lapply(item_element, function(x){x$getElementText()}))
      pricelist <- pricelist[1:length(storagelist)]
    }, silent=T)
    
    if (length(prodnames)==0) {
      try({
        warning("No nested listings, trying different scrape format...\n")
        processorlist <- c()
        storagelist <- c()
        pricelist <- c()
        
        currentpage <- read_html(pagelist[1])
        
        currname <- currentpage %>% 
          html_node("h1.desktopHeader") %>% 
          html_text(trim=T)
        
        currprocessor <- currentpage %>% 
          html_node("li[data-pn='Processor'] > div > p") %>% 
          html_text(trim=T)
        
        currstorage <- currentpage %>% 
          html_node("li[data-pn='Hard Drive'] > div > p") %>% 
          html_text(trim=T)
        
        currprice <- currentpage %>% 
          html_node("dd.saleprice.pricingSummary-details-final-price") %>% 
          html_text(trim=T)
        
        prodnames <- append(prodnames, currname)
        processorlist <- append(processorlist, currprocessor)
        storagelist <- append(storagelist, currstorage)
        pricelist <- append(pricelist, currprice)
      }, silent=T)
    }
  }
  
  # For differently formatted pages
  if (length(pagelist)==0) {
    warning("0 product URLs found, trying different scrape format...\n")
    # navigate to page
    remDr$navigate(url)
    
    # extract page list
    item_element <- remDr$findElements(using = "css", 
                                       'p.va-item-title > a')
    pagelist <- unlist(sapply(item_element, function(x) {x$getElementAttribute("href")}))
    pagelist <- pagelist[grepl("^.*/p/.*$", pagelist)]
    
    prodnames <- c()
    
    for (i in 1:length(pagelist)) {
      currentpage <- read_html(pagelist[i])
      
      currname <- currentpage %>% 
        html_node("h1.desktopHeader") %>% 
        html_text(trim=T)
      
      currprocessor <- currentpage %>% 
        html_node("li[data-pn='Processor'] > div > p") %>% 
        html_text(trim=T)
      
      currstorage <- currentpage %>% 
        html_node("li[data-pn='Hard Drive'] > div > p") %>% 
        html_text(trim=T)
      
      currprice <- currentpage %>% 
        html_node("dd.saleprice.pricingSummary-details-final-price") %>% 
        html_text(trim=T)
      
      # For dynamic pages, storage data might be missing
      # Use Selenium to load dynamic pages
      if (is.na(currstorage)) {
        # navigate to page
        remDr$navigate(pagelist[i])
        
        # extract prod name
        item_element <- remDr$findElement(using = "css", 
                                          'h1.desktopHeader')
        currname <- item_element$getElementText()[[1]] %>% trimws()
        
        # extract storage data
        item_element <- remDr$findElement(using = "css", 
                                          'dd.pdp-valuerow-3')
        currstorage <- item_element$getElementText()[[1]]
        
        # extract processor data
        item_element <- remDr$findElement(using = "css", 
                                          'dd.pdp-valuerow-0')
        currprocessor <- item_element$getElementText()[[1]]
        
      }
      prodnames <- append(prodnames, currname)
      processorlist <- append(processorlist, currprocessor)
      storagelist <- append(storagelist, currstorage)
      pricelist <- append(pricelist, currprice)
    }
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
  # Reformat price column 
  pricelist <- sub(".*G", "", pricelist)
  pricelist <- sub(" ", "", pricelist)
  
  # Print rows of each column for troubleshooting mismatches
  dflist <- list(prodnames=prodnames, processorlist=processorlist, pricelist=pricelist, storagelist=storagelist)
  for (item in c("prodnames","processorlist","pricelist", "storagelist")) {
    cat(item,": ", length(dflist[[item]]), " rows\n", sep="")
  }
  
  # Compile into dataframe
  compiled <- data.frame(Brand = 'Lenovo', Model = prodnames, Processor = processorlist, 
                         Price = pricelist, Size = storagesize, 
                         Storage = storagetype)
  
  # Export compiled dataframe as CSV
  filename <- paste("lenovo", categoryname, sep="_") %>% paste(".csv",sep="")
  write.csv(compiled, filename)
  return(cat("Export complete: ", filename, " (", nrow(compiled), " rows)\n\n", sep=""))
}