plot2 <- function(){
    ################################### 1. LOAD DATA ###################################
    ## load library
    library(plyr)
    library(dplyr)
    
    ## download url zip file
    fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
    zipfileName <- "./fnei_data.zip"
    download.file (fileUrl, zipfileName, "curl")
    
    ## unzip file in working directory
    unzip(zipfileName, exdir = "./")
    
    ## keep track of date of the download
    dateDownloaded <- Sys.Date()
    
    ################################### 2. READ FILE ###################################
    ## read rds file
    nei <- readRDS("summarySCC_PM25.rds")
    # scc <- readRDS("Source_Classification_Code.rds")
    
    ################################### 3. MANIPULATING DATA ###################################
    ## total emission by year for Baltimore City
    x <- nei %>% filter(fips == "24510") %>% group_by(year) %>% summarise(PM25 = log(sum(Emissions)))
    
    ## plotting
    # open graphic device
    png("plot2.png", 480, 480, "px")
    
    # draw plot
    plot(x, type = "l", main = "Baltimore City: Total emission", ylab = expression("log" * PM[2.5]))
    
    # close graphic device
    dev.off()
    
    
}