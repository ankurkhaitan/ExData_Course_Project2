# ========================================================================================================================================
# Load Libraries
# ========================================================================================================================================

library('plyr')

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

# calculate total amount of emissions per year
data <- ddply(SCC, .(year), summarise, Emissions = sum(Emissions[fips == "24510"]))

# devide total amount of emissions by 1'000 (thousend tons)
data$Emissions <- lapply(data$Emissions, function(x) round(x / 1e3, 2))

# ========================================================================================================================================
# Create and plot graph
# ========================================================================================================================================

png(filename = "plot2.png", width = 600, height = 600, units = "px", bg = "white")

# define margins
par(mfrow = c(1, 1), mar = c(5, 5, 3, 1))

with(data, plot(year, Emissions, pch = 20, col = "red", ylim = c(1.5, 3.5), xlim = c(1998, 2009), xaxt = "n", cex = 2.5, panel.first = grid(),
                main = expression("Baltimore City Annual PM"[2.5] * " Emissions"), xlab = "Year", ylab = expression("PM"[2.5] * " Emissions (thousend tonnes)")))

# add a line between points
lines(data$year, data$Emissions, type = "l", lwd = 2)
axis(1, c(1999, 2002, 2005, 2008))

# print values for each point in graph
text(data$year, data$Emissions, data$Emissions, cex = 1.0, pos = 4, col = "black")

dev.off()