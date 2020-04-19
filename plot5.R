# ========================================================================================================================================
# Load Libraries
# ========================================================================================================================================
library('dplyr')
library('plyr')
library('ggplot2')
# ========================================================================================================================================
# Definitions
# ========================================================================================================================================

SCC.On.Road <- TRUE

# ========================================================================================================================================
# Download and extract Data and load file
# ========================================================================================================================================

zipFile <- "exdata%2Fdata%2FNEI_data.zip"

if (!file.exists("Data/Source_Classification_Code.rds") && !file.exists("Data/summarySCC_PM25.rds")) {
  dataURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(dataURL, zipFile, mode = "wb")
  unzip(zipFile, files = NULL, list = FALSE, overwrite = TRUE, junkpaths = FALSE, exdir = "Data", unzip = "internal", setTimes = FALSE)
  file.remove(zipFile)
}

# Define Directory where File is located
dirName <- 'Data'

# load classification code data
fileNameClass = "Source_Classification_Code.rds"
fileNameClass <- file.path(dirName, fileNameClass)

# load Summary CSS PM25 data
fileNameSummary = "summarySCC_PM25.rds"
fileNameSummary <- file.path(dirName, fileNameSummary)

############data load#################
NEI <- readRDS(file = fileNameClass)
SCC <- readRDS(file = fileNameSummary)

if (SCC.On.Road) {
  
  # ========================================================================================================================================
  # Data preparation
  # ========================================================================================================================================
  
  # subset and retrieve only Baltimore City data
  pm25_Baltimore <- subset(SCC, fips == '24510' & type == 'ON-ROAD')
  
  # calculate total amount of emissions per year and type - 
  # rounding of emission values to 2 digits
  dataSCC <- ddply(pm25_Baltimore, .(year), summarise, Emissions = round(sum(Emissions), 2))
  
  # ========================================================================================================================================
  # Create and plot graph
  # ========================================================================================================================================
  
  png(filename = "plot5.png", width = 600, height = 600, units = "px", bg = "white")
  
  par(mfrow = c(1, 1), mar = c(5, 5, 3, 1))
  
  with(dataSCC, plot(year, Emissions, pch = 20, col = "red", xlim = c(1998, 2009), xaxt = "n", cex = 2.5, panel.first = grid(), 
                     main = expression("Motor vehicle - related PM"[2.5] * " emissions in Baltimore City "),
                     xlab = "Year", ylab = expression("PM"[2.5] * " Emissions (tonnes)")))
  
  # add a line between points
  lines(dataSCC$year, dataSCC$Emissions, type = "l", lwd = 2)
  axis(1, c(1999, 2002, 2005, 2008))
  
  # print values for each point in graph
  text(dataSCC$year, dataSCC$Emissions, dataSCC$Emissions, cex = 1.0, pos = 4, col = "black")
  
  dev.off()
  
}