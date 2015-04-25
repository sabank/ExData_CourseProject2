plot5 <- function(){
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
    scc <- readRDS("Source_Classification_Code.rds")
    
    ################################### 3. MANIPULATING DATA ###################################
    ## check available values in column 'EI.Sector' (i.e column 4)
    #values <- as.character(unique(scc[,4]))
    
    ## logical vector searching for string 'Vehicles'
    logicalVector <- with(scc, grepl("Vehicles", scc[,4]))
    extractrow <- which(logicalVector)
    code <- scc[extractrow,1]
    
    ## total emission by year in Baltimore City from motor vehicle sources
    x <- nei %>% filter(SCC %in% code & fips == "24510") %>% group_by(year) %>%
        summarise(PM25 = log(sum(Emissions)))
    
    ## plotting
    # open graphic device
    png("plot5.png", 480, 480, "px")
    
    # plot declaration
    g <- ggplot(x)
    
    # draw plot
    gp <- g + geom_line(aes(x=year, y=PM25)) +
        labs(title = "Baltimore City: Total Emission \n Motor vehicle sources") +
        labs(y = expression("log" * PM[2.5]))
    print(gp)
    
    ## close graphic device
    dev.off()
      
}