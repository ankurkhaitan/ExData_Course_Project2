# ========================================================================================================================================
# Load Libraries
# ========================================================================================================================================

library('dplyr')
library('plyr')
library('ggplot2')

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

# data <- read.table(file = fileNamePower, header = TRUE, sep = ';')
NEI <- readRDS(file = fileNameClass)
SCC <- readRDS(file = fileNameSummary)

# ========================================================================================================================================
# Data Preparation
# ========================================================================================================================================

# subset and retrieve only Baltimore City data
pm25_Baltimore <- subset(SCC, fips == '24510' & type == 'ON-ROAD')
pm25_LosAngeles <- subset(SCC, fips == '06037' & type == 'ON-ROAD')
pm25_Baltimore_LA <- subset(SCC, (fips == '06037' & type == 'ON-ROAD') | (fips == '24510' & type == 'ON-ROAD'))

# calculate total amount of emissions per year and type - rounding of emission values to 2 digits
dataSCC_Baltimore <- ddply(pm25_Baltimore, .(year), summarise, Emissions = round(sum(Emissions), 2))
dataSCC_LA <- ddply(pm25_LosAngeles, .(year), summarise, Emissions = round(sum(Emissions), 2))
dataSCC_Baltimore_LA <- ddply(pm25_Baltimore_LA, .(year, fips), summarise, Emissions = round(sum(Emissions), 2))

# ========================================================================================================================================
# Create and plot graph
# ========================================================================================================================================

png(filename = "plot6.png", width = 800, height = 800, units = "px", bg = "white")
g <- ggplot(dataSCC_Baltimore_LA, aes(year, Emissions))
g + geom_point(color = dataSCC_Baltimore_LA$fips, pch = 19, size = 5) +
  geom_line(data = dataSCC_Baltimore, col = 'blue', lwd = 1) +
  geom_line(data = dataSCC_LA, col = 'green', lwd = 1) +
  
  # adjust the size of the facet_grid variable 
  theme(strip.text.y = element_text(size = 14, face = "bold"), plot.margin = unit(c(2, 3, 2, 3), 'cm')) +
  
  # change the size of the title 
  theme(plot.title = element_text(lineheight = 3, face = "bold", color = "black", size = 20)) +
  
  # add and change the size of the x and y label
  xlab("Year") + ylab("Emissions [tonnes]") +
  
  # add a title to the graph
  ggtitle(expression(paste('Motor vehicle related emissions ', pm[2.5], ' Baltimore City vs Los Angeles'))) +
  scale_x_continuous(breaks = c(1999, 2002, 2005, 2008))
dev.off()

