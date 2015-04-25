plot3 <- function(){
    ################################### 1. LOAD DATA ###################################
    ## load library
    library(plyr)
    library(dplyr)
    library(ggplot2)
    
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
    ## total emission by type and year for Baltimore City
    x <- nei %>% filter(fips == "24510") %>% group_by(type, year) %>% summarise(PM25 = log(sum(Emissions)))
    
    ## plotting
    # open graphic device
    png("plot3.png", 480, 480, "px")
    
    # plot declaration
    g <- ggplot(x)
    
    # one graph per 'type'
    # geom_line(aes(x=year, y=PM25)) + facet_grid(type ~.)
    
    # or all 'type' on one graph with different color
    # geom_line(aes(x=year, y=PM25, color = type))
    
    # draw plot
    gp <- g + geom_line(aes(x=year, y=PM25, color = type)) +
        labs(title = "Baltimore City: Total Emission") + labs(y = expression("log" * PM[2.5]))
    print(gp)
    
    # close graphic device
    dev.off()
    
    
}