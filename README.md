# web_scrapers_PCs
Web scrapers for PC market leaders' product listings

Project files for scraping product pages in R, using:
- rvest
- RSelenium

The scraper collects names, prices, and technical specifications like storage type and processor type. Where possible, this was done using rvest by extracting elements directly from html. Otherwise, selenium was used to load dynamic elements that would be missed by rvest. 
