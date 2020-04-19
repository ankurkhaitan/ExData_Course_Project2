# ========================================================================================================================================
# Load Libraries
# ========================================================================================================================================
library('ggplot2')
library('plyr')
library('grid')

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
# Data preparation
# ========================================================================================================================================

# subset and retrieve only Baltimore City data
pm25_Baltimore <- subset(SCC, fips == '24510')

# calculate total amount of emissions per year and type - 
# rounding of emission values to 2 digits
data <- ddply(pm25_Baltimore, .(year, type), summarise, Emissions = round(sum(Emissions), 2))

# ========================================================================================================================================
# Create and plot graph
# ========================================================================================================================================

png(filename = "plot3.png", width = 800, height = 800, units = "px", bg = "white")

g <- ggplot(data, aes(year, Emissions))
g + facet_grid(type ~ ., scale = 'free') +
  geom_line(col = 'black', lwd = 1) +
  geom_point(col = 'black', size = 5) +
  
  # adjust the size of the facet_grid variable 
  theme(strip.text.y = element_text(size = 14, face = "bold"), plot.margin = unit(c(2, 3, 2, 3), 'cm')) +
  
  # change the size of the title 
  theme(plot.title = element_text(lineheight = 3, face = "bold", color = "black", size = 20)) +
  
  # add and change the size of the x and y label
  xlab("Year") + ylab("Emissions [tonnes]") +
  theme(axis.title.y = element_text(size = rel(2), angle = 90)) +
  theme(axis.title.x = element_text(size = rel(2), angle = 00)) +
  
  # add a title to the graph
  ggtitle(expression(paste('Baltimore City\'s Annual ', pm[2.5], ' emissions based on type'))) +
  scale_x_continuous(breaks = c(1999, 2002, 2005, 2008))

dev.off()
